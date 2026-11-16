#' @title Download a SANDRE WMS Raster Layer
#'
#' @description
#' Download a raster representation of a SANDRE geographic layer
#' over a specified spatial extent.
#'
#' The function uses the GDAL WMS driver to retrieve the requested
#' layer and reprojects the resulting raster to the requested
#' coordinate reference system and spatial resolution.
#'
#' @param x An `sf` or `sfc` object defining the download extent.
#' @param layer A `character` string specifying the WMS layer name,
#'   obtained from [get_layers()].
#' @param res A positive `numeric` value specifying the output
#'   spatial resolution in the units of `crs`.
#' @param crs An EPSG code or coordinate reference system accepted
#'   by [sf::st_crs()]. Defaults to EPSG:2154.
#' @param filename A `character` string specifying the output
#'   GeoTIFF filename, or `NULL` for a temporary file.
#' @param overwrite A `logical` value indicating whether an
#'   existing file should be overwritten.
#' @param verbose A `logical` value indicating whether progress
#'   messages should be displayed.
#'
#' @return
#' A `SpatRaster` object from the `terra` package.
#'
#' @importFrom sf st_bbox st_crs st_transform gdal_utils
#' @importFrom terra rast
#' @importFrom jsonlite fromJSON
#'
#' @export
get_wms <- function(
    x,
    layer,
    res = 10,
    crs = 2154,
    filename = NULL,
    overwrite = FALSE,
    verbose = TRUE
) {

  # --- Validate arguments ---
  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort(
      "Argument {.arg x} must be an {.cls sf} or {.cls sfc} object.",
      call = NULL
    )
  }

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(
      "Argument {.arg x} must have a valid coordinate reference system.",
      call = NULL
    )
  }

  if (!is.character(layer) || length(layer) != 1L ||
      is.na(layer) || !nzchar(layer)) {
    cli::cli_abort(
      "Argument {.arg layer} must be a non-empty character string.",
      call = NULL
    )
  }

  if (!is.numeric(res) || length(res) != 1L ||
      is.na(res) || !is.finite(res) || res <= 0) {
    cli::cli_abort(
      "Argument {.arg res} must be a positive finite number.",
      call = NULL
    )
  }

  if (length(overwrite) != 1L || is.na(overwrite) ||
      !is.logical(overwrite)) {
    cli::cli_abort(
      "Argument {.arg overwrite} must be a single logical value.",
      call = NULL
    )
  }

  if (length(verbose) != 1L || is.na(verbose) ||
      !is.logical(verbose)) {
    cli::cli_abort(
      "Argument {.arg verbose} must be a single logical value.",
      call = NULL
    )
  }

  if (!is.null(filename)) {
    if (!is.character(filename) || length(filename) != 1L ||
        is.na(filename) || !nzchar(filename)) {
      cli::cli_abort(
        "Argument {.arg filename} must be a non-empty character string or {.val NULL}.",
        call = NULL
      )
    }
  }

  # --- Validate CRS ---
  target_crs <- tryCatch(
    sf::st_crs(crs),
    error = function(e) {
      cli::cli_abort(
        "Argument {.arg crs} must be a valid coordinate reference system.",
        call = NULL
      )
    }
  )

  x <- sf::st_transform(x, target_crs)

  if (is.na(target_crs)) {
    cli::cli_abort(
      "Argument {.arg crs} must specify a valid coordinate reference system.",
      call = NULL
    )
  }

  target_srs <- target_crs$input

  if (is.null(target_srs) || is.na(target_srs)) {
    target_srs <- target_crs$wkt
  }

  # --- Check existing file ---
  if (!is.null(filename) && file.exists(filename) && !overwrite) {

    if (verbose) {
      cli::cli_alert_info(
        "Using existing raster: {.file {filename}}"
      )
    }

    return(terra::rast(filename))
  }

  # --- Transform extent ---
  x <- sf::st_transform(x, target_crs)

  bb <- sf::st_bbox(x)

  if (any(!is.finite(bb))) {
    cli::cli_abort(
      "Argument {.arg x} has an invalid spatial extent.",
      call = NULL
    )
  }

  # --- Retrieve WMS subdataset ---
  url <- paste0(
    "https://services.sandre.eaufrance.fr/geo/sandre?",
    "SERVICE=WMS&",
    "VERSION=1.3.0&",
    "REQUEST=GetCapabilities"
  )

  metadata <- tryCatch(
    sf::gdal_utils(
      "info",
      source = url,
      quiet = TRUE,
      options = "-json"
    ),
    error = function(e) {
      cli::cli_abort(
        "Unable to retrieve SANDRE WMS capabilities: {.message {conditionMessage(e)}}",
        call = NULL
      )
    }
  )

  metadata <- jsonlite::fromJSON(metadata)

  subdatasets <- metadata$metadata$SUBDATASETS

  if (is.null(subdatasets)) {
    cli::cli_abort(
      "No WMS layers were returned by the SANDRE service.",
      call = NULL
    )
  }

  subdatasets <- unlist(subdatasets, use.names = TRUE)

  subdatasets <- subdatasets[
    grepl("_NAME$", names(subdatasets))
  ]

  pattern <- paste0("LAYERS=", layer, "&")

  sd <- subdatasets[
    grepl(pattern, subdatasets, fixed = TRUE)
  ]

  if (length(sd) != 1L) {
    cli::cli_abort(
      "Layer {.val {layer}} was not found or is ambiguous in the SANDRE WMS service.",
      call = NULL
    )
  }

  sd <- unname(sd)

  # --- Generate GDAL WMS description ---
  tmp_xml <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp_xml), add = TRUE)

  tryCatch(
    sf::gdal_utils(
      "translate",
      source = sd,
      destination = tmp_xml,
      options = c("-of", "WMS"),
      quiet = !verbose
    ),
    error = function(e) {
      cli::cli_abort(
        "Unable to generate the WMS description: {.message {conditionMessage(e)}}",
        call = NULL
      )
    }
  )

  # --- Configure WMS description ---
  doc <- xml2::read_xml(tmp_xml)

  root <- xml2::xml_find_first(doc, "//GDAL_WMS")

  if (inherits(root, "xml_missing")) {
    cli::cli_abort(
      "Invalid GDAL WMS description returned by the service.",
      call = NULL
    )
  }

  xml2::xml_set_text(
    xml2::xml_find_first(root, ".//BlockSizeX"),
    "1024"
  )

  xml2::xml_set_text(
    xml2::xml_find_first(root, ".//BlockSizeY"),
    "1024"
  )

  xml2::xml_add_child(root, "ZeroBlockHttpCodes", "400, 404, 502")
  xml2::xml_add_child(root, "ZeroBlockOnServerException", "true")

  xml2::write_xml(
    doc,
    tmp_xml,
    options = "no_declaration"
  )

  # --- Define output file ---
  if (is.null(filename)) {

    cache_dir <- tools::R_user_dir("frsandre", "cache")

    dir.create(
      cache_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    filename <- tempfile(
      pattern = paste0(layer, "_"),
      tmpdir = cache_dir,
      fileext = ".tif"
    )

  } else {

    dir.create(
      dirname(filename),
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (file.exists(filename) && !overwrite) {
      cli::cli_abort(
        "File {.file {filename}} already exists.",
        call = NULL
      )
    }

  }

  # --- Download raster ---
  if (verbose) {
    cli::cli_alert_info(
      "Downloading SANDRE layer {.val {layer}}..."
    )
  }

  tryCatch(
    sf::gdal_utils(
      "warp",
      source = tmp_xml,
      destination = filename,
      options = c(
        "-of", "GTiff",
        "-te",
        as.character(bb[c("xmin", "ymin", "xmax", "ymax")]),
        "-te_srs", target_srs,
        "-t_srs", target_srs,
        "-tr", as.character(res), as.character(res),
        "-r", "bilinear",
        "-co", "TILED=YES",
        "-co", "COMPRESS=DEFLATE",
        "-co", "BIGTIFF=IF_NEEDED",
        if (overwrite) "-overwrite"
      ),
      quiet = !verbose
    ),
    error = function(e) {
      cli::cli_abort(
        "WMS download failed: {.message {conditionMessage(e)}}",
        call = NULL
      )
    }
  )

  # --- Read raster ---
  rast <- tryCatch(
    terra::rast(filename),
    error = function(e) {
      cli::cli_abort(
        "Unable to read the downloaded raster: {.message {conditionMessage(e)}}",
        call = NULL
      )
    }
  )

  if (verbose) {
    cli::cli_alert(
      "{.val {layer}} raster successfully downloaded"
    )
  }

  return(rast)
}
