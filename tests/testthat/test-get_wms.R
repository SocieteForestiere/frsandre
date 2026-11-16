# Reals tests ----
testthat::test_that("get_wms() downloads a real SANDRE WMS raster", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(
          c(700000, 6600000),
          c(700000, 6601000),
          c(701000, 6601000),
          c(701000, 6600000),
          c(700000, 6600000)
        )
      )),
      crs = 2154
    )
  )

  r <- get_wms(
    x = x,
    layer = "BassinDCE",
    res = 100,
    crs = 2154,
    filename = tempfile(fileext = ".tif"),
    verbose = FALSE
  )

  testthat::expect_s4_class(r, "SpatRaster")
  testthat::expect_equal(terra::nlyr(r), 3L)
  testthat::expect_equal(terra::crs(r), sf::st_crs(2154)$wkt)
  testthat::expect_gt(terra::ncell(r), 0)

  testthat::expect_true(
    all(
      is.finite(
        c(
          terra::xmin(r),
          terra::ymin(r),
          terra::xmax(r),
          terra::ymax(r)
        )
      )
    )
  )
})

testthat::test_that("get_wms() respects the requested extent and resolution", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(
          c(700000, 6600000),
          c(700000, 6601000),
          c(701000, 6601000),
          c(701000, 6600000),
          c(700000, 6600000)
        )
      )),
      crs = 2154
    )
  )

  res <- 100

  r <- get_wms(
    x = x,
    layer = "BassinDCE",
    res = res,
    crs = 2154,
    filename = tempfile(fileext = ".tif"),
    verbose = FALSE
  )

  ext <- terra::ext(r)

  testthat::expect_equal(
    terra::res(r),
    c(res, res)
  )

  testthat::expect_equal(
    c(
      terra::xmin(ext),
      terra::ymin(ext),
      terra::xmax(ext),
      terra::ymax(ext)
    ),
    c(700000, 6600000, 701000, 6601000),
    tolerance = res
  )
})


# Mocked tests ----
testthat::test_that("get_wms() downloads a WMS raster", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(
          c(700000, 6600000),
          c(700000, 6601000),
          c(701000, 6601000),
          c(701000, 6600000),
          c(700000, 6600000)
        )
      )),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(util, source, destination = NULL,
                          quiet = FALSE, options = NULL, ...) {

      if (util == "info") {

        return(
          paste0(
            '{',
            '"metadata": {',
            '"SUBDATASETS": {',
            '"SUBDATASET_1_NAME": ',
            '"WMS:https://example.com?LAYERS=BassinDCE&FORMAT=image/png",',
            '"SUBDATASET_1_DESC": "BassinDCE"',
            '}',
            '}',
            '}'
          )
        )
      }

      if (util == "translate") {

        xml <- paste0(
          '<?xml version="1.0" encoding="UTF-8"?>',
          '<GDAL_WMS>',
          '<Service name="WMS">',
          '<Version>1.3.0</Version>',
          '<ServerUrl>https://example.com/wms?</ServerUrl>',
          '<Layers>BassinDCE</Layers>',
          '</Service>',
          '<DataWindow>',
          '<UpperLeftX>0</UpperLeftX>',
          '<UpperLeftY>1000</UpperLeftY>',
          '<LowerRightX>1000</LowerRightX>',
          '<LowerRightY>0</LowerRightY>',
          '<SizeX>256</SizeX>',
          '<SizeY>256</SizeY>',
          '</DataWindow>',
          '</GDAL_WMS>'
        )

        writeLines(xml, destination)

        return(invisible(NULL))
      }

      if (util == "warp") {

        r <- terra::rast(
          nrows = 10,
          ncols = 10,
          xmin = 700000,
          xmax = 701000,
          ymin = 6600000,
          ymax = 6601000,
          crs = "EPSG:2154"
        )

        terra::values(r) <- seq_len(terra::ncell(r))

        terra::writeRaster(
          r,
          destination,
          overwrite = TRUE
        )

        return(invisible(NULL))
      }

      cli::cli_abort(
        "Unexpected GDAL utility {.val {util}}.",
        call = NULL
      )
    },
    .package = "sf"
  )

  filename <- tempfile(fileext = ".tif")
  on.exit(unlink(filename), add = TRUE)

  r <- get_wms(
    x = x,
    layer = "BassinDCE",
    res = 100,
    crs = 2154,
    filename = filename,
    verbose = FALSE
  )

  testthat::expect_s4_class(r, "SpatRaster")
  testthat::expect_true(file.exists(filename))
  testthat::expect_equal(terra::nlyr(r), 1L)
  testthat::expect_gt(terra::ncell(r), 0)
  testthat::expect_equal(terra::res(r), c(100, 100))
})

testthat::test_that("get_wms() uses an existing raster when overwrite is FALSE", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  filename <- tempfile(fileext = ".tif")
  on.exit(unlink(filename), add = TRUE)

  r0 <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 700000,
    xmax = 700200,
    ymin = 6600000,
    ymax = 6600200,
    crs = "EPSG:2154"
  )

  terra::values(r0) <- 1:4

  terra::writeRaster(
    r0,
    filename,
    overwrite = TRUE
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(...) {
      testthat::fail("sf::gdal_utils() should not be called.")
    },
    .package = "sf"
  )

  r <- get_wms(
    x = x,
    layer = "BassinDCE",
    filename = filename,
    overwrite = FALSE,
    verbose = FALSE
  )

  testthat::expect_s4_class(r, "SpatRaster")
  testthat::expect_equal(terra::ncell(r), 4L)
  testthat::expect_equal(
    as.vector(terra::values(r)),
    c(1, 2, 3, 4)
  )
})

testthat::test_that("get_wms() refuses to overwrite an existing file", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  filename <- tempfile(fileext = ".tif")
  on.exit(unlink(filename), add = TRUE)

  r0 <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 700000,
    xmax = 700200,
    ymin = 6600000,
    ymax = 6600200,
    crs = "EPSG:2154"
  )

  terra::values(r0) <- 1:4

  terra::writeRaster(
    r0,
    filename,
    overwrite = TRUE
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(...) {
      testthat::fail("sf::gdal_utils() should not be called.")
    },
    .package = "sf"
  )

  r <- get_wms(
    x = x,
    layer = "BassinDCE",
    filename = filename,
    overwrite = FALSE,
    verbose = FALSE
  )

  testthat::expect_s4_class(r, "SpatRaster")
  testthat::expect_equal(terra::ncell(r), 4L)
  testthat::expect_equal(
    as.vector(terra::values(r)),
    c(1, 2, 3, 4)
  )
})

testthat::test_that("get_wms() validates x", {

  testthat::expect_error(
    get_wms(
      x = data.frame(x = 1),
      layer = "BassinDCE"
    ),
    regexp = "must be an <sf> or <sfc> object"
  )

  x <- sf::st_sfc(
    sf::st_point(c(700000, 6600000))
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE"
    ),
    regexp = "valid coordinate reference system"
  )
})

testthat::test_that("get_wms() validates layer and res", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::expect_error(
    get_wms(x, layer = ""),
    regexp = "non-empty character string"
  )

  testthat::expect_error(
    get_wms(x, layer = NA_character_),
    regexp = "non-empty character string"
  )

  testthat::expect_error(
    get_wms(x, layer = "BassinDCE", res = 0),
    regexp = "positive finite number"
  )

  testthat::expect_error(
    get_wms(x, layer = "BassinDCE", res = NA_real_),
    regexp = "positive finite number"
  )

  testthat::expect_error(
    get_wms(x, layer = "BassinDCE", res = Inf),
    regexp = "positive finite number"
  )
})

testthat::test_that("get_wms() validates crs", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      crs = "not-a-crs"
    ),
    regexp = "valid coordinate reference system"
  )
})

testthat::test_that("get_wms() validates optional arguments", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      filename = ""
    ),
    regexp = "non-empty character string or"
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      overwrite = NA
    ),
    regexp = "single logical value"
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      verbose = c(TRUE, FALSE)
    ),
    regexp = "single logical value"
  )
})

testthat::test_that("get_wms() handles WMS capabilities errors", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(...) {
      cli::cli_abort(
        "mock capabilities error",
        call = NULL
      )
    },
    .package = "sf"
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "Unable to retrieve SANDRE WMS capabilities"
  )
})

testthat::test_that("get_wms() handles missing WMS subdatasets", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(...) {
      '{"metadata": {}}'
    },
    .package = "sf"
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "No WMS layers were returned"
  )
})

testthat::test_that("get_wms() handles an unknown WMS layer", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(util, ...) {

      if (util == "info") {
        return(
          paste0(
            '{"metadata": {',
            '"SUBDATASETS": {',
            '"SUBDATASET_1_NAME": ',
            '"WMS:https://example.com?LAYERS=BassinDCE&FORMAT=image/png"',
            '}',
            '}}'
          )
        )
      }

      cli::cli_abort(
        "Unexpected GDAL utility.",
        call = NULL
      )
    },
    .package = "sf"
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "UnknownLayer",
      verbose = FALSE
    ),
    regexp = "was not found or is ambiguous"
  )
})

testthat::test_that("get_wms() handles WMS description errors", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(util, ...) {

      if (util == "info") {
        return(
          paste0(
            '{"metadata": {',
            '"SUBDATASETS": {',
            '"SUBDATASET_1_NAME": ',
            '"WMS:https://example.com?LAYERS=BassinDCE&FORMAT=image/png"',
            '}',
            '}}'
          )
        )
      }

      if (util == "translate") {
        cli::cli_abort(
          "mock translate error",
          call = NULL
        )
      }

      cli::cli_abort(
        "Unexpected GDAL utility.",
        call = NULL
      )
    },
    .package = "sf"
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "Unable to generate the WMS description"
  )
})

testthat::test_that("get_wms() handles an invalid GDAL WMS description", {

  x <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_point(c(700000, 6600000)),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    gdal_utils = function(util, source, destination = NULL, ...) {

      if (util == "info") {
        return(
          paste0(
            '{"metadata": {',
            '"SUBDATASETS": {',
            '"SUBDATASET_1_NAME": ',
            '"WMS:https://example.com?LAYERS=BassinDCE&FORMAT=image/png"',
            '}',
            '}}'
          )
        )
      }

      if (util == "translate") {
        writeLines(
          "<InvalidGDALDocument/>",
          destination
        )
        return(invisible(NULL))
      }

      cli::cli_abort(
        "Unexpected GDAL utility.",
        call = NULL
      )
    },
    .package = "sf"
  )

  testthat::expect_error(
    get_wms(
      x = x,
      layer = "BassinDCE",
      verbose = FALSE
    ),
    regexp = "Invalid GDAL WMS description"
  )
})
