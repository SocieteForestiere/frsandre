#' @title Get Available SANDRE Web Service Layers
#'
#' @description
#' Retrieve the list of available resources from the SANDRE geographic
#' Web Feature Service (WFS) or Web Map Service (WMS).
#'
#' The function queries the service's GetCapabilities endpoint and returns
#' a data frame containing the name and title of each available resource.
#'
#' @param key A `character` string specifying the service type:
#'   `"wfs"` for Web Feature Service or `"wms"` for Web Map Service.
#'
#' @return A `data frame` with two columns:
#' \describe{
#'   \item{resource_name}{The unique name of the service resource.}
#'   \item{resource_label}{The human-readable title of the resource.}
#' }
#'
#' @importFrom cli cli_abort
#' @importFrom httr2 request req_url_query req_perform resp_check_status resp_body_raw
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text
#'
#' @export
get_layers <- function(key) {

  # --- Validate arguments ---
  if (!is.character(key) || length(key) != 1L ||
      is.na(key) || !nzchar(key)) {
    cli::cli_abort(
      "Argument {.arg key} must be a non-empty character string.",
      call = NULL
    )
  }

  if (!key %in% c("wfs", "wms")) {
    cli::cli_abort(
      "Argument {.arg key} must be {.val wfs} or {.val wms}.",
      call = NULL
    )
  }

  # --- Service parameters ---
  service <- toupper(key)
  version <- if (key == "wfs") "2.0.0" else "1.3.0"

  url <- "https://services.sandre.eaufrance.fr/geo/sandre"

  # --- Get capabilities ---
  response <- httr2::request(url) |>
    httr2::req_url_query(
      SERVICE = service,
      REQUEST = "GetCapabilities",
      VERSION = version
    ) |>
    httr2::req_perform()

  httr2::resp_check_status(response)

  capabilities <- xml2::read_xml(
    httr2::resp_body_raw(response)
  )

  # --- Find resources ---
  node_name <- if (key == "wfs") "FeatureType" else "Layer"

  resources <- xml2::xml_find_all(
    capabilities,
    paste0(
      "//*[local-name()='", node_name, "']",
      "[*[local-name()='Name']]"
    )
  )

  # --- Format table ---
  res <- data.frame(
    resource_name = vapply(
      resources,
      function(x) {
        xml2::xml_text(
          xml2::xml_find_first(
            x,
            "./*[local-name()='Name']"
          )
        )
      },
      character(1)
    ),
    resource_label = vapply(
      resources,
      function(x) {
        xml2::xml_text(
          xml2::xml_find_first(
            x,
            "./*[local-name()='Title']"
          )
        )
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  return(res)
}
