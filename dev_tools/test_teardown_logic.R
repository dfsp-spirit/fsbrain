#!/usr/bin/env Rscript
#
# test_teardown_logic.R -- Check when the fsbrain test suite deletes the optional data cache.
#
# The teardown file 'tests/testthat/teardown-cran.R' deletes the optional data cache, which CRAN
# requires us to do (we must not leave files in the user's home directory), but which is very
# annoying when it happens on a developer machine. This script checks that the teardown only does
# so in the situations in which it is supposed to, by running the 'setup-cran.R' / 'teardown-cran.R'
# pair under all relevant combinations of the environment variables and of the initial cache state.
#
# The package cache is redirected to throwaway directories (via the 'pkgfilecache.cachedir' option),
# so the cache of the user running this script is never touched. Run it from the package root:
#
#   Rscript dev_tools/test_teardown_logic.R
#
# Exit code: 0 if all cases behave as expected, 1 otherwise.

suppressMessages(library(fsbrain));

if(! file.exists("tests/testthat/teardown-cran.R")) {
    stop("Run this script from the fsbrain package root directory.");
}
test_dir = "tests/testthat";
preexisting_file = "subjects_dir/fsaverage/surf/lh.white";       # a file of the user running the tests
downloaded_file = "subjects_dir/fsaverage/surf/lh.subcortical";  # a file that a test downloads

results = list();

# Simulate one test run: an optional pre-existing cache file, an optional download during the run,
# the relevant environment variables, and the files that the teardown is expected to remove.
simulate = function(label, warm_cache, downloads, not_cran = NULL, check_pkg = NULL, expected_removed) {
    root = file.path(tempdir(), paste0("fsbrain_teardown_check_", gsub("[^a-zA-Z0-9]", "", label)));
    unlink(root, recursive = TRUE);
    old_options = options(pkgfilecache.cachedir = root);
    on.exit({ options(old_options); unlink(root, recursive = TRUE); });

    cache_dir = pkgfilecache:::get_cache_dir(pkgfilecache::get_pkg_info("fsbrain"));
    write_cache_file = function(rel_path) {
        abs_path = file.path(cache_dir, rel_path);
        dir.create(dirname(abs_path), recursive = TRUE, showWarnings = FALSE);
        writeLines("dummy", abs_path);
    }
    if(warm_cache) {
        write_cache_file(preexisting_file);
    }
    if(is.null(not_cran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv(NOT_CRAN = not_cran);
    if(is.null(check_pkg)) Sys.unsetenv("_R_CHECK_PACKAGE_NAME_") else Sys.setenv("_R_CHECK_PACKAGE_NAME_" = check_pkg);

    env = new.env(parent = globalenv());
    sys.source(file.path(test_dir, "setup-cran.R"), envir = env);     # testthat sources this first
    if(downloads) {
        write_cache_file(downloaded_file);                            # a test downloads data
    }
    before = list.files(cache_dir, recursive = TRUE);
    sys.source(file.path(test_dir, "teardown-cran.R"), envir = env);  # and this one last
    removed = as.character(setdiff(before, list.files(cache_dir, recursive = TRUE)));

    ok = identical(sort(removed), sort(as.character(expected_removed)));
    results[[label]] <<- ok;
    cat(sprintf("%-28s warm_cache=%-5s download=%-5s R_CMD_check=%-5s NOT_CRAN=%-5s  removed: %-16s %s\n",
        label, warm_cache, downloads, ! is.null(check_pkg), ! is.null(not_cran),
        if(length(removed) == 0L) "nothing" else paste(removed, collapse = ", "),
        if(ok) "OK" else "MISMATCH"));
}

cat("=== fsbrain test suite cache teardown behavior ===\n");
# A developer running the tests: the cache, and anything downloaded into it, must survive.
simulate("plain dev test run", TRUE, TRUE, NULL, NULL, character(0));
simulate("devtools::test()", TRUE, TRUE, "true", NULL, character(0));
# A real 'R CMD check' on a fresh machine (like CRAN): everything the check downloaded is removed.
simulate("CRAN check (downloads)", FALSE, TRUE, NULL, "fsbrain", downloaded_file);
simulate("CRAN check (no download)", FALSE, FALSE, NULL, "fsbrain", character(0));
# A local 'R CMD check' of a developer: the previously downloaded data is kept.
simulate("local R CMD check", TRUE, TRUE, NULL, "fsbrain", character(0));
simulate("check with NOT_CRAN=true", FALSE, TRUE, "true", "fsbrain", character(0));

n_failed = sum(! unlist(results));
cat(sprintf("\n%d of %d cases behave as expected.\n", length(results) - n_failed, length(results)));
if(n_failed > 0L) {
    quit(status = 1L);
}
