# Companion to 'teardown-cran.R': record the state of the optional data cache before any test runs,
# so that the teardown can tell whether the cache was created by this test run (and may be deleted
# afterwards) or whether it belongs to the developer who is running the test suite (and must be left
# alone). See 'teardown-cran.R' for the full explanation.

# The number of optional data files that are available in the package cache right now. This is 0 if
# no data has ever been downloaded, e.g., on a fresh CRAN check machine.
options(fsbrain.tests.initial.cache.file.count = length(fsbrain::list_optional_data()));
