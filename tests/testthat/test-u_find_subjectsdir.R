# Tests for finding a subjects dir that contains a given subject. These tests do not need any
# downloaded data: they create a fake package cache (and mask any real FreeSurfer installation on
# this machine), so that only the cache is searched.

# Create a fake package cache and return the path of the 'subjects_dir' in it.
create.fake.cache <- function() {
    fake_cache = tempfile("fsbrain_fake_cache_");
    withr::local_options(list(pkgfilecache.cachedir = fake_cache), .local_envir = parent.frame());
    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    subjects_dir = file.path(pkgfilecache::get_cache_dir(pkg_info), "subjects_dir");
    dir.create(subjects_dir, recursive = TRUE);
    return(subjects_dir);
}

# Mask a FreeSurfer installation: point FREESURFER_HOME at an existing but empty directory. This
# makes 'find.freesurferhome' report it (so it does not fall back to guessing typical install
# locations), but it contains no subjects.
mask.freesurfer <- function() {
    empty_fs_home = tempfile("no_freesurfer_");
    dir.create(empty_fs_home);
    withr::local_envvar(list(FREESURFER_HOME = empty_fs_home, SUBJECTS_DIR = NA), .local_envir = parent.frame());
}


test_that("An incomplete subject in the package cache is not reported as found", {
    subjects_dir = create.fake.cache();
    mask.freesurfer();

    # An incomplete fsaverage: only atlas files, as downloaded by 'download_fsaverage_atlases'.
    atlas_file = file.path(subjects_dir, "fsaverage", "label", "lh.subcortical.annot");
    dir.create(dirname(atlas_file), recursive = TRUE);
    file.create(atlas_file);

    # It must not be reported as the fsaverage subject ...
    res = find.subjectsdir.of(subject_id = "fsaverage", mustWork = FALSE);
    expect_false(res$found);
    expect_null(res$found_at);
    # ... but the location has to be reported, so that code which looks for specific files in it
    # (like the atlas files) still finds them.
    expect_true(subjects_dir %in% res$found_all_locations);

    # The helper which decides whether a subject is complete.
    expect_false(subject.dir.has.core.files(file.path(subjects_dir, "fsaverage")));

    # 'fsaverage.path' must not return the incomplete directory, it either finds a complete
    # fsaverage somewhere else (e.g. a FreeSurfer installation) or fails with a helpful message.
    expect_error(fsaverage.path(allow_fetch = FALSE), "download_fsaverage");
})


test_that("A complete subject in the package cache is reported as found", {
    subjects_dir = create.fake.cache();
    mask.freesurfer();

    white_surface = file.path(subjects_dir, "fsaverage", "surf", "lh.white");
    dir.create(dirname(white_surface), recursive = TRUE);
    file.create(white_surface);

    expect_true(subject.dir.has.core.files(file.path(subjects_dir, "fsaverage")));

    res = find.subjectsdir.of(subject_id = "fsaverage", mustWork = FALSE);
    expect_true(res$found);
    expect_equal(res$found_at, subjects_dir);
    expect_true(subjects_dir %in% res$found_all_locations);

    # 'mustWork=TRUE' returns the path itself.
    expect_equal(find.subjectsdir.of(subject_id = "fsaverage", mustWork = TRUE), subjects_dir);
})


test_that("A subject which is not in the cache at all is not found", {
    subjects_dir = create.fake.cache();
    mask.freesurfer();

    res = find.subjectsdir.of(subject_id = "fsaverage", mustWork = FALSE);
    expect_false(res$found);
    expect_length(res$found_all_locations, 0L);
    expect_error(find.subjectsdir.of(subject_id = "fsaverage", mustWork = TRUE), "Could not find subjects dir");
    expect_error(fsaverage.path(allow_fetch = FALSE), "download_fsaverage");

    # An empty subjects dir (not even a subject subdir) is not reported either.
    file.create(file.path(subjects_dir, "not_a_subject.txt"));
    expect_false(find.subjectsdir.of(subject_id = "fsaverage", mustWork = FALSE)$found);
})


test_that("the mesh atlas resolver finds the cache with an incomplete subject", {
    subjects_dir = create.fake.cache();
    mask.freesurfer();

    # The atlas files, but no surfaces: the subcortical atlas is fully available, while the subject
    # is not usable as a template (e.g., it cannot provide a cortex mesh as context).
    atlas_files = mesh.atlas.file.paths(subjects_dir, "fsaverage", "subcortical", "subcortical");
    dir.create(dirname(atlas_files[1L]), recursive = TRUE);
    dir.create(dirname(atlas_files[3L]), recursive = TRUE, showWarnings = FALSE);
    file.create(atlas_files);

    expect_false(find.subjectsdir.of(subject_id = "fsaverage", mustWork = FALSE)$found);
    expect_equal(mesh.atlas.resolve.subjects.dir("fsaverage"), subjects_dir);
    expect_true(all(file.exists(mesh.atlas.file.paths(subjects_dir, "fsaverage", "subcortical", "subcortical"))));

    # Without the atlas files, the resolver reports the best location anyway, so that the caller
    # can complain about the missing files.
    expect_equal(mesh.atlas.resolve.subjects.dir("fsaverage", atlas = "nosuchatlas"), subjects_dir);
    expect_error(mesh.atlas.check.files(mesh.atlas.resolve.subjects.dir("fsaverage", atlas = "nosuchatlas"), "fsaverage", "nosuchatlas", "subcortical"), "missing");
})
