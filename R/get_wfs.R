#' @title Retrieve Features from a SANDRE WFS Layer
#'
#' @description
#' Retrieve spatial features from the SANDRE geographic Web Feature Service
#' (WFS) within the bounding box of a given spatial object.
#'
#' The function transforms the input bounding box to EPSG:3857, queries the
#' WFS GetFeature endpoint and returns the retrieved features as an `sf`
#' object reprojected to the requested coordinate reference system.
#'
#' @param x An `sf` or `sfc` object defining the spatial extent of the query.
#'
#' @param layer A `character` string specifying the name of the WFS layer
#'   to retrieve. Available layers can be listed using [get_layers()].
#'
#' @param crs A positive numeric EPSG code specifying the output coordinate
#'   reference system. Defaults to `2154` (RGF93 / Lambert-93).
#'
#' @param verbose A `logical` value indicating whether to display a
#'   success message. Defaults to `TRUE`.
#'
#' @return An `sf` object containing the retrieved features, reprojected
#'   to the requested coordinate reference system.
#'
#' @importFrom cli cli_abort cli_alert
#' @importFrom httr2 request req_url_query req_perform resp_check_status resp_body_raw
#' @importFrom sf st_bbox st_transform st_read st_crs
#' @importFrom xml2 read_xml xml_root xml_name xml_text xml_find_all
#'
#' @export
get_wfs <- function(
    x,
    layer,
    crs = 2154,
    verbose = TRUE
) {

  # --- Validate arguments ---
  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort(
      "Argument {.arg x} must be an {.cls sf} or {.cls sfc} object",
      call = NULL
    )
  }

  if (!is.character(layer) || length(layer) != 1L ||
      is.na(layer) || !nzchar(layer)) {
    cli::cli_abort(
      "Argument {.arg layer} must be a non-empty character string",
      call = NULL
    )
  }

  if (!is.numeric(crs) || length(crs) != 1L ||
      is.na(crs) || !is.finite(crs) || crs <= 0) {
    cli::cli_abort(
      "Argument {.arg crs} must be a positive EPSG code.",
      call = NULL
    )
  }

  if (!is.logical(verbose) || length(verbose) != 1L ||
      is.na(verbose)) {
    cli::cli_abort(
      "Argument {.arg verbose} must be {.val TRUE} or {.val FALSE}.",
      call = NULL
    )
  }

  # --- Get bounding box ---
  bbox <- sf::st_bbox(
    sf::st_transform(x, crs = 3857)
  )

  bbox <- paste(
    format(
      as.numeric(bbox),
      scientific = FALSE,
      trim = TRUE,
      digits = 15
    ),
    collapse = ","
  )

  bbox <- paste0(bbox, ",EPSG:3857")

  # --- WFS request ---
  url <- "https://services.sandre.eaufrance.fr/geo/sandre"

  response <- httr2::request(url) |>
    httr2::req_url_query(
      SERVICE = "WFS",
      VERSION = "2.0.0",
      REQUEST = "GetFeature",
      TYPENAMES = layer,
      SRSNAME = "EPSG:3857",
      BBOX = bbox
    ) |>
    httr2::req_perform()

  httr2::resp_check_status(response)

  # --- Validate WFS response ---
  doc <- xml2::read_xml(
    httr2::resp_body_raw(response)
  )

  root <- xml2::xml_root(doc)
  root_name <- xml2::xml_name(root)

  if (root_name == "ExceptionReport") {
    message <- xml2::xml_text(
      xml2::xml_find_all(
        doc,
        "//*[local-name()='ExceptionText']"
      )
    )

    cli::cli_abort(
      "WFS request failed: {.val {paste(message, collapse = '; ')}}",
      call = NULL
    )
  }

  if (root_name != "FeatureCollection") {
    cli::cli_abort(
      "Unexpected WFS response: {.val {root_name}}.",
      call = NULL
    )
  }

  # --- Read GML response ---
  file <- tempfile(fileext = ".gml")
  on.exit(unlink(file), add = TRUE)

  writeBin(
    httr2::resp_body_raw(response),
    file
  )

  res <- sf::st_read(
    file,
    quiet = TRUE
  )

  # --- Validate features ---
  if (!inherits(res, "sf")) {
    cli::cli_abort(
      "Unable to read WFS features.",
      call = NULL
    )
  }

  if (nrow(res) == 0L) {
    cli::cli_abort(
      "WFS returned no features for the requested bounding box",
      call = NULL
    )
  }

  if (is.na(sf::st_crs(res))) {
    cli::cli_abort(
      "WFS features have no defined coordinate reference system",
      call = NULL
    )
  }

  # --- Reproject geometries ---
  res <- sf::st_transform(
    res,
    crs = crs
  )

  # --- Report success ---
  if (verbose) {
    cli::cli_alert(
      "{.val {nrow(res)}} features retrieved from {.val {layer}}"
    )
  }

  return(res)
}
