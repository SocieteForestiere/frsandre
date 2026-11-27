# Reals tests ----
testthat::test_that("get_wfs() retrieves real SANDRE features", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        matrix(
          c(
            2.0, 48.0,
            2.1, 48.0,
            2.1, 48.1,
            2.0, 48.1,
            2.0, 48.0
          ),
          ncol = 2,
          byrow = TRUE
        )
      )),
      crs = 4326
    )
  )

  res <- get_wfs(
    x = x,
    layer = "sa:TronconHydrographique_FXX_Topage2026",
    crs = 2154,
    verbose = FALSE
  )

  testthat::expect_s3_class(res, "sf")
  testthat::expect_gt(nrow(res), 0L)
  testthat::expect_equal(sf::st_crs(res)$epsg, 2154)
})

testthat::test_that("get_wfs() handles Lambert-93 input", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        matrix(
          c(
            650000, 6800000,
            660000, 6800000,
            660000, 6810000,
            650000, 6810000,
            650000, 6800000
          ),
          ncol = 2,
          byrow = TRUE
        )
      )),
      crs = 2154
    )
  )

  res <- get_wfs(
    x = x,
    layer = "sa:TronconHydrographique_FXX_Topage2026",
    crs = 2154,
    verbose = FALSE
  )

  testthat::expect_s3_class(res, "sf")
  testthat::expect_gt(nrow(res), 0L)
  testthat::expect_equal(sf::st_crs(res)$epsg, 2154)
})

# Mocked tests ----
testthat::test_that("get_wfs() validates x", {

  testthat::expect_error(
    get_wfs(
      x = data.frame(x = 1),
      layer = "sa:TronconHydrographique_FXX_Topage2026"
    )
  )

  testthat::expect_error(
    get_wfs(
      x = NULL,
      layer = "sa:TronconHydrographique_FXX_Topage2026"
    )
  )
})

testthat::test_that("get_wfs() validates layer", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  testthat::expect_error(
    get_wfs(x, layer = "")
  )

  testthat::expect_error(
    get_wfs(x, layer = NA_character_)
  )

  testthat::expect_error(
    get_wfs(x, layer = c("a", "b"))
  )

  testthat::expect_error(
    get_wfs(x, layer = 123)
  )
})

testthat::test_that("get_wfs() validates crs", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", crs = 0)
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", crs = -2154)
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", crs = NA_real_)
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", crs = Inf)
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", crs = c(2154, 4326))
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", crs = "2154")
  )
})

testthat::test_that("get_wfs() validates verbose", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", verbose = NA)
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", verbose = c(TRUE, FALSE))
  )

  testthat::expect_error(
    get_wfs(x, layer = "BassinDCE", verbose = 1)
  )
})

testthat::test_that("get_wfs() retrieves and transforms mocked features", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        matrix(
          c(
            2.0, 48.0,
            2.1, 48.0,
            2.1, 48.1,
            2.0, 48.1,
            2.0, 48.0
          ),
          ncol = 2,
          byrow = TRUE
        )
      )),
      crs = 4326
    )
  )

  gml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<wfs:FeatureCollection ',
    'xmlns:wfs="http://www.opengis.net/wfs/2.0" ',
    'xmlns:gml="http://www.opengis.net/gml/3.2" ',
    'xmlns:sandre="http://services.sandre.eaufrance.fr/geo/sandre">',
    '<wfs:member>',
    '<sandre:BassinDCE gml:id="b1">',
    '<sandre:code>001</sandre:code>',
    '<sandre:geometry>',
    '<gml:Polygon srsName="urn:ogc:def:crs:EPSG::3857">',
    '<gml:exterior>',
    '<gml:LinearRing>',
    '<gml:posList>',
    '222638.9816 6106854.8349 ',
    '233770.9781 6106854.8349 ',
    '233770.9781 6120989.1230 ',
    '222638.9816 6120989.1230 ',
    '222638.9816 6106854.8349',
    '</gml:posList>',
    '</gml:LinearRing>',
    '</gml:exterior>',
    '</gml:Polygon>',
    '</sandre:geometry>',
    '</sandre:BassinDCE>',
    '</wfs:member>',
    '</wfs:FeatureCollection>'
  )

  response <- list(
    body = charToRaw(gml)
  )

  testthat::local_mocked_bindings(
    request = function(url) {
      list(url = url)
    },
    req_url_query = function(req, ...) {
      req$query <- list(...)
      req
    },
    req_perform = function(req) {
      response
    },
    resp_check_status = function(response) {
      invisible(response)
    },
    resp_body_raw = function(response) {
      response$body
    },
    .package = "httr2"
  )

  res <- get_wfs(
    x = x,
    layer = "BassinDCE",
    crs = 2154,
    verbose = FALSE
  )

  testthat::expect_s3_class(res, "sf")
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_true("code" %in% names(res))
  testthat::expect_equal(res$code, "001")
  testthat::expect_equal(sf::st_crs(res)$epsg, 2154)
})


testthat::test_that("get_wfs() handles request errors", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  testthat::local_mocked_bindings(
    request = function(url) {
      list(url = url)
    },
    req_url_query = function(req, ...) {
      req
    },
    req_perform = function(req) {
      stop("Connection failed")
    },
    .package = "httr2"
  )

  testthat::expect_warning(
    res <- get_wfs(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "Connection failed"
  )

  testthat::expect_null(res)
})


testthat::test_that("get_wfs() handles HTTP errors", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  response <- list()

  testthat::local_mocked_bindings(
    request = function(url) {
      list(url = url)
    },
    req_url_query = function(req, ...) {
      req
    },
    req_perform = function(req) {
      response
    },
    resp_check_status = function(response) {
      stop("HTTP 500 Internal Server Error")
    },
    .package = "httr2"
  )

  testthat::expect_warning(
    res <- get_wfs(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "HTTP 500"
  )

  testthat::expect_null(res)
})


testthat::test_that("get_wfs() reports WFS ExceptionReport", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<ows:ExceptionReport ',
    'xmlns:ows="http://www.opengis.net/ows/1.1">',
    '<ows:Exception exceptionCode="InvalidParameterValue">',
    '<ows:ExceptionText>Unknown layer</ows:ExceptionText>',
    '</ows:Exception>',
    '</ows:ExceptionReport>'
  )

  response <- list(
    body = charToRaw(xml)
  )

  testthat::local_mocked_bindings(
    request = function(url) {
      list(url = url)
    },
    req_url_query = function(req, ...) {
      req
    },
    req_perform = function(req) {
      response
    },
    resp_check_status = function(response) {
      invisible(response)
    },
    resp_body_raw = function(response) {
      response$body
    },
    .package = "httr2"
  )

  testthat::expect_warning(
    res <- get_wfs(
      x = x,
      layer = "UnknownLayer",
      verbose = FALSE
    ),
    regexp = "Unknown layer"
  )

  testthat::expect_null(res)
})


testthat::test_that("get_wfs() handles unexpected WFS responses", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<UnexpectedResponse>',
    '<message>Something went wrong</message>',
    '</UnexpectedResponse>'
  )

  response <- list(
    body = charToRaw(xml)
  )

  testthat::local_mocked_bindings(
    request = function(url) {
      list(url = url)
    },
    req_url_query = function(req, ...) {
      req
    },
    req_perform = function(req) {
      response
    },
    resp_check_status = function(response) {
      invisible(response)
    },
    resp_body_raw = function(response) {
      response$body
    },
    .package = "httr2"
  )

  testthat::expect_warning(
    res <- get_wfs(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "Unexpected WFS response"
  )

  testthat::expect_null(res)
})


testthat::test_that("get_wfs() handles empty WFS responses", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    )
  )

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<wfs:FeatureCollection ',
    'xmlns:wfs="http://www.opengis.net/wfs/2.0"/>'
  )

  response <- list(
    body = charToRaw(xml)
  )

  testthat::local_mocked_bindings(
    request = function(url) {
      list(url = url)
    },
    req_url_query = function(req, ...) {
      req
    },
    req_perform = function(req) {
      response
    },
    resp_check_status = function(response) {
      invisible(response)
    },
    resp_body_raw = function(response) {
      response$body
    },
    .package = "httr2"
  )

  testthat::local_mocked_bindings(
    st_read = function(dsn, quiet = TRUE) {
      sf::st_sf(
        geometry = sf::st_sfc(
          crs = 3857
        )
      )
    },
    .package = "sf"
  )

  testthat::expect_message(
    res <- get_wfs(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "No features found"
  )

  testthat::expect_null(res)
})
