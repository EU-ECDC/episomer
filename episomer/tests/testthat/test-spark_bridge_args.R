test_that("java_path uses JAVA_HOME when set and the path otherwise", {
  skip_on_os("windows")
  withr::local_envvar(JAVA_HOME = "")
  expect_equal(java_path(), "java")
  withr::local_envvar(JAVA_HOME = "/opt/jdk-21")
  expect_equal(java_path(), "\"/opt/jdk-21/bin/java\"")
})

test_that("the MKL options only apply on Windows", {
  skip_on_os("windows")
  local_conf(use_mkl = TRUE)
  expect_equal(get_blas_java_opt(), "")
  path <- Sys.getenv("PATH")
  set_blas_env()
  expect_equal(Sys.getenv("PATH"), path)
})

test_that("set_scala_r_env exports the R to run and its library paths", {
  withr::local_envvar(EPISOMER_RSCRIPT = NA, R_LIBS = NA)
  testthat::local_mocked_bindings(rscript_exe_for_scala = function() "/usr/bin/Rscript", .package = "episomer")
  set_scala_r_env()
  expect_equal(Sys.getenv("EPISOMER_RSCRIPT"), "/usr/bin/Rscript")
  expect_equal(Sys.getenv("R_LIBS"), paste(.libPaths(), collapse = .Platform$path.sep))
})

test_that("conf_languages_as_arg lists codes, names and vector paths", {
  local_conf(languages = list(
    list(code = "en", name = "English", vectors = "/d/en.txt.gz"),
    list(code = "fr", name = "French", vectors = "/d/fr.txt.gz")
  ))
  expect_equal(
    conf_languages_as_arg(),
    paste0(
      "langCodes \"en,fr\" langNames \"English,French\" ",
      "langPaths \"/d/en.txt.gz,/d/fr.txt.gz\" "
    )
  )
})

test_that("conf_languages_as_arg handles no language", {
  local_conf(languages = list())
  expect_equal(conf_languages_as_arg(), "langCodes \"\" langNames \"\" langPaths \"\" ")
})

test_that("conf_geonames_as_arg points at the geo folder of the data directory", {
  local_conf(data_dir = "/data/epi", geonames_simplify = TRUE)
  expect_equal(
    conf_geonames_as_arg(),
    paste(
      "geonamesSource \"/data/epi/geo/allCountries.txt\"",
      "geonamesDestination \"/data/epi/geo\"",
      "geonamesSimplify \"TRUE\""
    )
  )
})
