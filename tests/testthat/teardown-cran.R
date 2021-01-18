# Starting in Jan 2021, CRAN starts nagging you about leaving files in the userdir.
# We have to store data there to be able to get away with the 5 MB (!) package size limit and
# still be able to run unit tests on CRAN.
# We have 2 options: 1) do not run any unit tests requiring data on CRAN (almost all of our tests require data).
#                    2) download the data and delete everything afterwards, on CRAN (users will want to keep the data, as they only have it if they decided to download it).
# So we delete all data ONLY if we are on CRAN in this teardown file.

if(!identical(Sys.getenv("NOT_CRAN"), "true")) {
  fsbrain::delete_all_optional_data();
}
