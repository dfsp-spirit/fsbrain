library(testthat)
library(fsbrain)

# Break up tests to allow tests to run longer than 10 min in travis,
# see https://github.com/travis-ci/travis-ci/issues/3849#issuecomment-345686242.
test_check("fsbrain", filter = "^[v-z]")
