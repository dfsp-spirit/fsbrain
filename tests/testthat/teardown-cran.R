# Starting in Jan 2021, CRAN starts nagging you about leaving files in the userdir.
# We have to store data there to be able to get away with the 5 MB (!) package size limit and
# still be able to run unit tests on CRAN.
# We have 2 options: 1) do not run any unit tests requiring data on CRAN (almost all of our tests require data).
#                    2) download the data and delete everything afterwards, on CRAN (users will want to keep the data, as they only have it if they decided to download it).
# So we delete all data ONLY if we are on CRAN in this teardown file.
#
# Most tests that need optional data skip on CRAN nowadays, so usually nothing is downloaded during
# a CRAN check and there is nothing to delete. A few tests download the data unconditionally (they
# only skip *after* the download attempt), so we keep this cleanup as a safety net.
#
# How to detect that:
#
# 1) 'NOT_CRAN' alone is NOT a sufficient test, which is easy to get wrong: the variable is only set
#    by the devtools/testthat wrappers (like 'devtools::test()'), and NOT by a plain
#    'testthat::test_dir("tests/testthat")' call from a normal R session. testthat's own
#    'skip_on_cran()' uses 'testthat:::on_cran()', which treats an *unset* 'NOT_CRAN' as "on CRAN"
#    whenever the session is not interactive -- exactly what a plain 'Rscript -e ...test_dir...' run
#    looks like. Deleting the cache in that situation deleted the data of the developer who was
#    running the test suite, which is not funny.
# 2) So we additionally require '_R_CHECK_PACKAGE_NAME_', the environment variable that 'R CMD check'
#    sets for the package it is checking. Both CRAN and a local 'R CMD check' set it, a plain test
#    run does not.
# 3) Finally, we only delete the data if the cache was empty when this test run started (see
#    'setup-cran.R'), i.e., if the files we would delete are the ones that this very check has
#    downloaded. On CRAN, the cache always starts out empty, so the original behavior is unchanged
#    there. On the machine of a developer who runs 'R CMD check' on a package with a populated
#    cache (or who runs the test suite with 'NOT_CRAN' unset), nothing is deleted.
#
# If you run the test suite manually and want the data-related tests to actually run (instead of
# being skipped with "On CRAN"), set the environment variable NOT_CRAN to 'true', e.g.:
#   NOT_CRAN=true Rscript -e 'testthat::test_dir("tests/testthat")'

if(!identical(Sys.getenv("NOT_CRAN"), "true") && nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
  initial_cache_file_count = getOption("fsbrain.tests.initial.cache.file.count", default = 0L);
  # Only if this check downloaded something: the cache was empty before, but is not anymore.
  if(initial_cache_file_count == 0L && length(fsbrain::list_optional_data()) > 0L) {
    fsbrain::delete_all_optional_data();
  }
}
