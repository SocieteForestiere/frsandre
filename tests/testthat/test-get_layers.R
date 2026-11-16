# Reals tests ----
testthat::test_that("get_layers() retrieves WFS layers from SANDRE", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  res <- get_layers("wfs")

  testthat::expect_true(inherits(res, "data.frame"))
  testthat::expect_named(
    res,
    c("resource_name", "resource_label")
  )
  testthat::expect_gt(nrow(res), 0L)
  testthat::expect_type(res$resource_name, "character")
  testthat::expect_type(res$resource_label, "character")
  testthat::expect_true(all(nzchar(res$resource_name)))
})

testthat::test_that("get_layers() retrieves WMS layers from SANDRE", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  res <- get_layers("wms")

  testthat::expect_true(inherits(res, "data.frame"))
  testthat::expect_named(
    res,
    c("resource_name", "resource_label")
  )
  testthat::expect_gt(nrow(res), 0L)
  testthat::expect_type(res$resource_name, "character")
  testthat::expect_type(res$resource_label, "character")
  testthat::expect_true(all(nzchar(res$resource_name)))
})

# Mocked tests ----
testthat::test_that("get_layers() validates key", {

  testthat::expect_error(
    get_layers(NULL),
    regexp = "must be a non-empty character string"
  )

  testthat::expect_error(
    get_layers(character()),
    regexp = "must be a non-empty character string"
  )

  testthat::expect_error(
    get_layers(NA_character_),
    regexp = "must be a non-empty character string"
  )

  testthat::expect_error(
    get_layers(""),
    regexp = "must be a non-empty character string"
  )

  testthat::expect_error(
    get_layers(c("wfs", "wms")),
    regexp = "must be a non-empty character string"
  )

  testthat::expect_error(
    get_layers("foo"),
    regexp = 'must be "wfs" or "wms"'
  )
})

testthat::test_that("get_layers() returns an empty table when no named resource is found", {

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<wfs:WFS_Capabilities ',
    'xmlns:wfs="http://www.opengis.net/wfs/2.0">',
    '<wfs:FeatureTypeList>',
    '<wfs:FeatureType>',
    '<wfs:Title>Without name</wfs:Title>',
    '</wfs:FeatureType>',
    '</wfs:FeatureTypeList>',
    '</wfs:WFS_Capabilities>'
  )

  testthat::local_mocked_bindings(
    request = function(...) "request",
    req_url_query = function(req, ...) req,
    req_perform = function(...) "response",
    resp_check_status = function(response) invisible(response),
    resp_body_raw = function(response) charToRaw(xml),
    .package = "httr2"
  )

  res <- get_layers("wfs")

  testthat::expect_true(inherits(res, "data.frame"))
  testthat::expect_named(
    res,
    c("resource_name", "resource_label")
  )
  testthat::expect_equal(nrow(res), 0L)
})

testthat::test_that("get_layers() parses WFS capabilities", {

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<wfs:WFS_Capabilities ',
    'xmlns:wfs="http://www.opengis.net/wfs/2.0">',
    '<wfs:FeatureTypeList>',
    '<wfs:FeatureType>',
    '<wfs:Name>BassinDCE</wfs:Name>',
    '<wfs:Title>Bassin DCE</wfs:Title>',
    '</wfs:FeatureType>',
    '<wfs:FeatureType>',
    '<wfs:Name>CoursEau</wfs:Name>',
    '<wfs:Title>Cours d&apos;eau</wfs:Title>',
    '</wfs:FeatureType>',
    '</wfs:FeatureTypeList>',
    '</wfs:WFS_Capabilities>'
  )

  testthat::local_mocked_bindings(
    request = function(...) "request",
    req_url_query = function(req, ...) req,
    req_perform = function(...) "response",
    resp_check_status = function(response) invisible(response),
    resp_body_raw = function(response) charToRaw(xml),
    .package = "httr2"
  )

  res <- get_layers("wfs")

  testthat::expect_equal(
    res,
    data.frame(
      resource_name = c("BassinDCE", "CoursEau"),
      resource_label = c("Bassin DCE", "Cours d'eau"),
      stringsAsFactors = FALSE
    )
  )
})

testthat::test_that("get_layers() parses WMS capabilities", {

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<wms:WMS_Capabilities ',
    'xmlns:wms="http://www.opengis.net/wms">',
    '<wms:Capability>',
    '<wms:Layer>',
    '<wms:Name>BassinDCE</wms:Name>',
    '<wms:Title>Bassin DCE</wms:Title>',
    '</wms:Layer>',
    '<wms:Layer>',
    '<wms:Name>CoursEau</wms:Name>',
    '<wms:Title>Cours d&apos;eau</wms:Title>',
    '</wms:Layer>',
    '</wms:Capability>',
    '</wms:WMS_Capabilities>'
  )

  testthat::local_mocked_bindings(
    request = function(...) "request",
    req_url_query = function(req, ...) req,
    req_perform = function(...) "response",
    resp_check_status = function(response) invisible(response),
    resp_body_raw = function(response) charToRaw(xml),
    .package = "httr2"
  )

  res <- get_layers("wms")

  testthat::expect_equal(
    res,
    data.frame(
      resource_name = c("BassinDCE", "CoursEau"),
      resource_label = c("Bassin DCE", "Cours d'eau"),
      stringsAsFactors = FALSE
    )
  )
})
