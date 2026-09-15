# Tests for the visualization of atlases that ship their own mesh (R/vis_mesh_atlas.R).
#
# Most tests here are offline: they build a tiny synthetic subject containing a "mesh atlas"
# (an 8-vertex cube as both the atlas mesh and the cortical context mesh, plus a 2-region
# annotation), so they do not require any downloaded data.

# Helper: create a minimal subjects dir with a synthetic mesh atlas for one subject.
# The atlas ('testatlas' on mesh 'testmesh') has 2 regions with 4 vertices each, the same
# cube is also available as the cortical surface 'white'. Returns the subjects dir.
create.synthetic.mesh.atlas.subject <- function(subject_id = "fsaverage") {

    subjects_dir = tempfile("fsbrain_mesh_atlas_subjects_dir_");
    subject_dir = file.path(subjects_dir, subject_id);
    dir.create(file.path(subject_dir, "surf"), recursive = TRUE);
    dir.create(file.path(subject_dir, "label"), recursive = TRUE);

    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
    cube$vertices = cube$vertices * 50;   # scale the unit cube to a brain-like size, for rendering tests
    num_vertices = nrow(cube$vertices);

    test_colortable = data.frame(
        "struct_name" = c("unknown", "RegionA", "RegionB"),
        "r" = c(0L, 255L, 0L), "g" = c(0L, 0L, 255L), "b" = c(0L, 0L, 0L), "a" = 0L,
        "struct_index" = seq.int(0L, 2L), stringsAsFactors = FALSE);

    first_region = seq_len(num_vertices / 2L);
    second_region = seq.int(num_vertices / 2L + 1L, num_vertices);
    region_indices = integer(num_vertices);
    region_indices[first_region] = 2L;   # index of 'RegionA' in the colortable
    region_indices[second_region] = 3L;  # index of 'RegionB' in the colortable

    for(hemi in c("lh", "rh")) {
        freesurferformats::write.fs.surface(file.path(subject_dir, "surf", sprintf("%s.white", hemi)), cube$vertices, cube$faces);
        freesurferformats::write.fs.surface(file.path(subject_dir, "surf", sprintf("%s.testmesh", hemi)), cube$vertices, cube$faces);
        freesurferformats::write.fs.annot(file.path(subject_dir, "label", sprintf("%s.testatlas.annot", hemi)),
            num_vertices = num_vertices, colortable = test_colortable,
            labels_as_indices_into_colortable = region_indices);
    }

    return(subjects_dir);
}

# Convenience wrapper to call the function under test on the synthetic subject.
vis.synth.atlas <- function(subjects_dir, atlas = "testatlas", surface = "testmesh", cortex = NULL,
        style = "default", rglactions = list("no_vis" = TRUE),
        lh_region_value_list = list("RegionA" = 0.1, "RegionB" = 0.9),
        rh_region_value_list = list("RegionA" = 0.2, "RegionB" = 0.8), ...) {
    return(vis.subcortical.region.values(subjects_dir, subject_id = "fsaverage",
        lh_region_value_list = lh_region_value_list, rh_region_value_list = rh_region_value_list,
        atlas = atlas, surface = surface, cortex = cortex, style = style,
        rglactions = rglactions, silent = TRUE, ...));
}


test_that("vis.subcortical.region.values assigns the region values to the atlas mesh vertices", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    cm = vis.synth.atlas(subjects_dir);
    expect_equal(length(cm), 2L);
    expect_true(all(sapply(cm, is.fs.coloredmesh)));

    # One color per region, and the two regions must have different colors.
    for(cmesh in cm) {
        expect_equal(length(unique(cmesh$col[1:4])), 1L);
        expect_equal(length(unique(cmesh$col[5:8])), 1L);
        expect_false(identical(cmesh$col[1], cmesh$col[5]));
    }
    # Regions which are not listed in the value lists get the value 'value_for_unlisted_regions'
    # (NA by default), and hemispheres without any values are not rendered at all.
    cm_partial = vis.synth.atlas(subjects_dir, lh_region_value_list = list("RegionA" = 0.5),
        rh_region_value_list = NULL);
    expect_equal(length(cm_partial), 1L);
    expect_equal(cm_partial[[1]]$hemi, "lh");
    expect_equal(cm_partial[[1]]$metadata$src_data$lh[1:4], rep(0.5, 4L));
    expect_true(all(is.na(cm_partial[[1]]$metadata$src_data$lh[5:8])));
    expect_equal(length(unique(cm_partial[[1]]$col[1:4])), 1L);
    expect_false(identical(cm_partial[[1]]$col[1], cm_partial[[1]]$col[5]));
})


test_that("vis.subcortical.region.values returns a flat list, not a hemilist", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    cm = vis.synth.atlas(subjects_dir);
    # This is required: renderers treat a list with entries named 'lh'/'rh' as a hemilist and
    # would silently ignore all meshes but the first one per hemisphere.
    expect_null(names(cm));
    expect_equal(sum(sapply(cm, function(x) x$hemi == "lh")), 1L);
    expect_equal(sum(sapply(cm, function(x) x$hemi == "rh")), 1L);
})


test_that("vis.subcortical.region.values supports a semi-transparent context mesh", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    cm = vis.synth.atlas(subjects_dir, cortex = "white", style = "shiny");

    expect_equal(length(cm), 4L);   # 2 data meshes + 2 context meshes
    # The context meshes come first (they must be rendered below the opaque data meshes).
    expect_equal(cm[[1]]$style$alpha, 0.12);
    expect_equal(cm[[2]]$style$alpha, 0.12);
    expect_equal(cm[[3]]$style, "shiny");
    expect_equal(cm[[4]]$style, "shiny");

    # The context mesh must not contribute to the colorbar of the data meshes.
    expect_null(cm[[1]]$metadata$makecmap_options);
    expect_equal(coloredmeshes.combined.data.range(cm), c(0.1, 0.9));

    # The context options can be given as a list.
    cm2 = vis.synth.atlas(subjects_dir, cortex = list("surface" = "white", "color" = "#123456", "alpha" = 0.5));
    expect_equal(cm2[[1]]$style$alpha, 0.5);
    expect_equal(unique(cm2[[1]]$col), "#123456");
})


test_that("vis.subcortical.region.values reports missing atlas files with a helpful error", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    expect_error(vis.synth.atlas(subjects_dir, surface = "nosuchmesh"), "not available", fixed = TRUE);
    expect_error(vis.synth.atlas(subjects_dir, surface = "nosuchmesh"), "download", fixed = TRUE);
    expect_error(vis.synth.atlas(subjects_dir, atlas = "nosuchatlas"), "not available", fixed = TRUE);
})


test_that("vis.subcortical.region.values reports missing context surfaces with a helpful error", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    # The synthetic subject has a 'white' surface, but no 'pial' surface.
    expect_error(vis.synth.atlas(subjects_dir, cortex = "pial"), "context surface");
    expect_error(vis.synth.atlas(subjects_dir, cortex = "pial"), "download_fsaverage");
})


test_that("vis.subcortical.region.values warns about unsupported rglactions", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    expect_warning(vis.synth.atlas(subjects_dir, rglactions = list("no_vis" = TRUE, "shift_hemis_apart" = TRUE)),
        "shift_hemis_apart");
})


test_that("meshes which carry their own style are rendered with it", {
    mesh_style = list("alpha" = 0.3);

    # The mesh style is used when the default style is requested ...
    expect_equal(get.rglstyle.parameters(list("style" = mesh_style), "default"), mesh_style);
    expect_equal(get.rglstyle.parameters(list("style" = "shiny"), "default"), get.rglstyle("shiny"));
    # ... but an explicitly requested style wins.
    expect_equal(get.rglstyle.parameters(list("style" = mesh_style), "glass"), get.rglstyle("glass"));
    # A mesh with the 'default' style must not cause infinite recursion.
    expect_equal(get.rglstyle.parameters(list("style" = "default"), "default"), get.rglstyle("default"));
    # Meshes without a style fall back to the default style.
    expect_equal(get.rglstyle.parameters(list(), "default"), get.rglstyle("default"));
})


test_that("regions with the value NaN are hidden completely", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    cm_all = vis.synth.atlas(subjects_dir);   # RegionA (vertices 1-4) = 0.1, RegionB (vertices 5-8) = 0.9

    cm_hidden = vis.synth.atlas(subjects_dir, lh_region_value_list = list("RegionA" = NaN, "RegionB" = 0.9),
        rh_region_value_list = NULL);
    expect_equal(length(cm_hidden), 1L);
    cmesh = cm_hidden[[1]];

    # The vertices of the hidden region are removed from the mesh, together with all faces that use them.
    # For the cube, only the 2 faces of the second region survive.
    expect_equal(ncol(cmesh$mesh$vb), 4L);
    expect_equal(ncol(cmesh$mesh$it), 2L);
    expect_true(min(cmesh$mesh$it) >= 1L);
    expect_true(max(cmesh$mesh$it) <= 4L);
    expect_false(any(is.na(cmesh$mesh$it)));

    # The remaining vertices, colors and data are those of the visible region, and the hidden
    # region is not drawn in the NA color but really gone.
    expect_equal(length(cmesh$col), 4L);
    expect_equal(length(unique(cmesh$col)), 1L);
    expect_false(identical(cmesh$col[1], getOption('fsbrain.brain_na_color', default = "#FEFEFE")));
    expect_equal(cmesh$metadata$src_data$lh, rep(0.9, 4L));
    expect_false(any(is.nan(cmesh$metadata$src_data$lh)));

    # The vertices are a subset of the atlas mesh, in the same order (not reordered).
    cube = freesurferformats::read.fs.surface(file.path(subjects_dir, "fsaverage", "surf", "lh.testmesh"));
    expect_equal(unname(t(cmesh$mesh$vb[1:3, ])), unname(cube$vertices[5:8, ]));

    # The source surface in the metadata is kept consistent as well. Note that for fs.surface
    # instances, the faces are stored one row per face (in contrast to the tmesh3d 'it' matrix).
    expect_equal(nrow(cmesh$metadata$fs_mesh$vertices), 4L);
    expect_equal(unname(cmesh$metadata$fs_mesh$vertices), unname(cube$vertices[5:8, ]));
    expect_equal(nrow(cmesh$metadata$fs_mesh$faces), 2L);
    expect_equal(as.integer(t(cmesh$metadata$fs_mesh$faces)), as.integer(cmesh$mesh$it));

    # Hidden regions do not contribute to the colorbar range.
    expect_equal(coloredmeshes.combined.data.range(cm_hidden), c(0.9, 0.9));
    expect_equal(coloredmeshes.combined.data.range(cm_all), c(0.1, 0.9));

    # Hiding works per hemisphere and does not affect the other one.
    cm_rh_hidden = vis.synth.atlas(subjects_dir, rh_region_value_list = list("RegionA" = 0.2, "RegionB" = NaN));
    expect_equal(ncol(cm_rh_hidden[[1]]$mesh$vb), 8L);   # the left hemisphere is not affected
    expect_equal(ncol(cm_rh_hidden[[2]]$mesh$vb), 4L);
    expect_equal(cm_rh_hidden[[1]]$metadata$src_data$lh, cm_all[[1]]$metadata$src_data$lh);

    # Hiding composes with a rendering style and a context mesh.
    cm_ctx = vis.synth.atlas(subjects_dir, lh_region_value_list = list("RegionA" = NaN, "RegionB" = 0.9),
        rh_region_value_list = NULL, cortex = "white", style = "shiny");
    expect_equal(length(cm_ctx), 3L);   # 2 context meshes + 1 data mesh
    expect_equal(ncol(cm_ctx[[3]]$mesh$vb), 4L);
    expect_equal(cm_ctx[[3]]$style, "shiny");
})


test_that("all regions of a hemisphere can be hidden with value_for_unlisted_regions", {
    subjects_dir = create.synthetic.mesh.atlas.subject();

    # Only the listed regions are visible, all others are hidden.
    cm = vis.synth.atlas(subjects_dir, lh_region_value_list = list("RegionA" = 0.5),
        rh_region_value_list = NULL, value_for_unlisted_regions = NaN);
    expect_equal(length(cm), 1L);
    expect_equal(ncol(cm[[1]]$mesh$vb), 4L);   # 'RegionB' has been hidden
    expect_equal(ncol(cm[[1]]$mesh$it), 2L);
    expect_equal(cm[[1]]$metadata$src_data$lh, rep(0.5, 4L));
    expect_equal(unique(cm[[1]]$col), cm[[1]]$col[1]);
})


test_that("hiding all regions reports a helpful error", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    all_hidden = list("RegionA" = NaN, "RegionB" = NaN);

    expect_error(vis.synth.atlas(subjects_dir, lh_region_value_list = all_hidden, rh_region_value_list = NULL,
        value_for_unlisted_regions = NaN), "All regions are hidden");
    # Same for a hemisphere in which all regions are listed explicitly and all of them are hidden.
    expect_error(vis.synth.atlas(subjects_dir, lh_region_value_list = all_hidden, rh_region_value_list = NULL),
        "All regions are hidden");
})


test_that("hiding all but one region still allows exporting the scene", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    cm = vis.synth.atlas(subjects_dir, lh_region_value_list = list("RegionA" = 0.5, "RegionB" = NaN),
        rh_region_value_list = NULL, value_for_unlisted_regions = NaN);

    # All remaining vertices carry the same value, so the data range is degenerate. This must not
    # make the export fail: no colorbar can be plotted for such data.
    expect_equal(coloredmeshes.combined.data.range(cm), c(0.5, 0.5));
    expect_false(can.plot.colorbar.from.coloredmeshes(cm));

    if(requireNamespace("scimesh", quietly = TRUE) && requireNamespace("magick", quietly = TRUE)) {
        old_backend = getOption("fsbrain.renderer_backend");
        options(fsbrain.renderer_backend = "scimesh");
        output_img = tempfile("fsbrain_mesh_atlas_hidden_", fileext = ".png");
        export(cm, view_angles = c("sd_lateral_lh"), output_img = output_img, silent = TRUE);
        options(fsbrain.renderer_backend = old_backend);
        expect_true(file.exists(output_img));
    }
})


test_that("regions with a NA value are not hidden but drawn in the NA color", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    cm = vis.synth.atlas(subjects_dir, lh_region_value_list = list("RegionA" = NA, "RegionB" = 0.9),
        rh_region_value_list = NULL);
    cmesh = cm[[1]];
    # In contrast to NaN, a NA value does not remove any vertices.
    expect_equal(ncol(cmesh$mesh$vb), 8L);
    expect_equal(length(cmesh$col), 8L);
    expect_true(all(cmesh$col[1:4] == cmesh$col[1]));
    expect_false(identical(cmesh$col[1], cmesh$col[5]));
})


test_that("coloredmesh.from.color stores the style in the coloredmesh", {
    subjects_dir = create.synthetic.mesh.atlas.subject();
    cm = coloredmesh.from.color(subjects_dir, "fsaverage", "#FF0000", "lh", surface = "white",
        style = list("alpha" = 0.25));
    expect_equal(cm$style$alpha, 0.25);
    cm_no_style = coloredmesh.from.color(subjects_dir, "fsaverage", "#FF0000", "lh", surface = "white");
    expect_null(cm_no_style$style);
})


test_that("the subcortical atlas works with the fsaverage template subject", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");
    skip_if_not(file.exists(file.path(subjects_dir, "fsaverage", "label", "lh.subcortical.annot")),
        message = "Subcortical atlas is not available.");

    lh_values = list("Left-Caudate" = 0.1, "Left-Putamen" = 0.9);
    rh_values = list("Right-Caudate" = 0.4, "Right-Hippocampus" = 0.6);
    cm = vis.subcortical.region.values(subjects_dir, "fsaverage",
        lh_region_value_list = lh_values, rh_region_value_list = rh_values,
        rglactions = list("no_vis" = TRUE), silent = TRUE);

    expect_equal(length(cm), 2L);
    expect_equal(coloredmeshes.combined.data.range(cm), c(0.1, 0.9));
    expect_equal(ncol(cm[[1]]$mesh$vb), 25910L);    # left hemisphere subcortical mesh
    expect_equal(ncol(cm[[2]]$mesh$vb), 25368L);    # right hemisphere subcortical mesh

    # The subjects dir is optional, it is resolved when NULL and the atlas is in the package cache.
    cm_auto = vis.subcortical.region.values(subject_id = "fsaverage",
        lh_region_value_list = lh_values, rh_region_value_list = rh_values,
        rglactions = list("no_vis" = TRUE), silent = TRUE);
    expect_equal(length(cm_auto), 2L);

    # Regions can be hidden by assigning NaN to them: their vertices are removed from the mesh.
    annot = subject.annot(subjects_dir, "fsaverage", "lh", "subcortical");
    keep = annot$label_names != "Left-Putamen";
    surf = freesurferformats::read.fs.surface(file.path(subjects_dir, "fsaverage", "surf", "lh.subcortical"));
    face_kept = keep[surf$faces[, 1]] & keep[surf$faces[, 2]] & keep[surf$faces[, 3]];
    cm_hidden = vis.subcortical.region.values(subjects_dir, "fsaverage",
        lh_region_value_list = list("Left-Caudate" = 0.1, "Left-Putamen" = NaN),
        rh_region_value_list = NULL, rglactions = list("no_vis" = TRUE), silent = TRUE);
    expect_equal(ncol(cm_hidden[[1]]$mesh$vb), sum(keep));
    expect_equal(ncol(cm_hidden[[1]]$mesh$it), sum(face_kept));
    expect_equal(unname(t(cm_hidden[[1]]$mesh$vb[1:3, ])), unname(surf$vertices[keep, ]));
    expect_equal(coloredmeshes.combined.data.range(cm_hidden), c(0.1, 0.1));

    # Hiding does not change the colors of the regions that are still visible: the hidden value
    # is excluded from the colorbar range, so both runs use the same colormap.
    caudate_vertices = which(annot$label_names == "Left-Caudate");
    vis_caudate = function(putamen_value) {
        vis.subcortical.region.values(subjects_dir, "fsaverage",
            lh_region_value_list = list("Left-Caudate" = 0.1, "Left-Amygdala" = 0.9, "Left-Putamen" = putamen_value),
            rh_region_value_list = NULL, rglactions = list("no_vis" = TRUE), silent = TRUE);
    }
    cm_visible_putamen = vis_caudate(0.5);
    cm_hidden_putamen = vis_caudate(NaN);
    expect_equal(coloredmeshes.combined.data.range(cm_visible_putamen), c(0.1, 0.9));
    expect_equal(coloredmeshes.combined.data.range(cm_hidden_putamen), c(0.1, 0.9));
    expect_equal(cm_hidden_putamen[[1]]$col[match(caudate_vertices, which(keep))],
        cm_visible_putamen[[1]]$col[caudate_vertices]);

    # Rendering with the context mesh of the same subject works as well, provided that the
    # cortical surfaces of the subject are available.
    if(file.exists(file.path(subjects_dir, "fsaverage", "surf", "lh.white"))) {
        cm_ctx = vis.subcortical.region.values(subjects_dir, "fsaverage",
            lh_region_value_list = lh_values, rh_region_value_list = rh_values,
            cortex = list("surface" = "white", "alpha" = 0.2),
            rglactions = list("no_vis" = TRUE), silent = TRUE);
        expect_equal(length(cm_ctx), 4L);
        expect_equal(cm_ctx[[1]]$style$alpha, 0.2);

        # The scene can be exported to an image file. Note that no 'style' is passed here on
        # purpose: the scene remembers the styles of its meshes.
        if(requireNamespace("scimesh", quietly = TRUE) && requireNamespace("magick", quietly = TRUE)) {
            old_backend = getOption("fsbrain.renderer_backend");
            options(fsbrain.renderer_backend = "scimesh");
            output_img = tempfile("fsbrain_mesh_atlas_", fileext = ".png");
            export(cm_ctx, view_angles = c("sd_lateral_lh"), output_img = output_img, silent = TRUE);
            options(fsbrain.renderer_backend = old_backend);
            expect_true(file.exists(output_img));
            # The rendered scene must not be empty: the colored structures should be visible
            # inside the semi-transparent cortex.
            rgba = magick::image_data(magick::image_read(output_img), channels = "rgb");
            r = as.integer(rgba[1, , ]); g = as.integer(rgba[2, , ]); b = as.integer(rgba[3, , ]);
            expect_true(mean(pmax(r, g, b) - pmin(r, g, b) > 40) > 0.001);
        }
    } else {
        # An incomplete subject dir must be reported with a helpful message.
        expect_error(vis.subcortical.region.values(subjects_dir, "fsaverage",
            lh_region_value_list = lh_values, rh_region_value_list = rh_values,
            cortex = "white", rglactions = list("no_vis" = TRUE), silent = TRUE), "context surface");
    }
})
