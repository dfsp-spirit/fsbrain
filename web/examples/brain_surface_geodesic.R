#!/usr/bin/env Rscript
#
# brain_surface_geodesic.R -- Compute geodesic distances on brain surface meshes
# using the Rvcg package (https://github.com/zarquon42b/Rvcg).
#
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# Dependencies:
#   Requires the 'fsbrain' package plus optional dependencies to be installed.
#   To get them, run in your R session:
#       install.packages("fsbrain", dependencies = TRUE);
#
# USAGE: ./brain_surface_geodesic.R [--vis] [--outdir <dir>]
#
# OPTIONS:
#   --vis          : try to open an OpenGL window and plot the results.
#                    Turn off on headless machines (default: FALSE).
#   --outdir <dir> : directory to write distance map files and images into
#                    (default: the current working directory).
#
# Written by Tim Schaefer

library("fsbrain");
library("freesurferformats");
library("Rvcg");


# --- Distance helpers --------------------------------------------------------------

# Compute the Euclidian (straight-line) distance between two points in 3D.
euclidian.dist <- function(x1, x2) {
    sqrt(sum((x1 - x2) ^ 2));
}

# Compute the Euclidian distance between two vertices of a surface mesh.
vertex.euclid.dist <- function(surf, v1, v2) {
    euclidian.dist(surf$vertices[v1, ], surf$vertices[v2, ]);
}


# --- Data loading --------------------------------------------------------------------

# Download the fsaverage template (if not present) and load the white matter surface
# meshes for both hemispheres. Returns a list with the loaded data.
load_brain_data <- function() {
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = "fsaverage";
    brain_hemispheres = fsbrain::subject.surface(subjects_dir, subject_id, "white", hemi = "both");
    lh_tmesh3d = rgl::tmesh3d(c(t(brain_hemispheres$lh$vertices)), c(t(brain_hemispheres$lh$faces)), homogeneous = FALSE);
    return(list(subjects_dir = subjects_dir, subject_id = subject_id,
                brain_hemispheres = brain_hemispheres, lh_tmesh3d = lh_tmesh3d));
}


# --- Tests 1 & 2: pairwise distances between fixed vertices ----------------------------

# Compute geodesic and Euclidian distances between a few well-known vertices on the left
# hemisphere: on the precentral gyrus, at the bottom of the central sulcus, and on the
# postcentral gyrus. Results are printed to the console. If 'do_vis' is TRUE, the three
# vertices are also highlighted on an interactive brain surface.
run_pairwise_distance_tests <- function(data, do_vis = FALSE) {
    brain_hemispheres = data$brain_hemispheres;
    lh_tmesh3d = data$lh_tmesh3d;

    lh_vertex_idx_precentral_gyrus = 32258;  # on precentral gyrus         (red in the image plotted later)
    lh_vertex_idx_central_sulcus = 32220;    # at bottom of central sulcus (green)
    lh_vertex_idx_postcentral_gyrus = 17792; # on postcentral gyrus        (blue)

    lh_source_point = brain_hemispheres$lh$vertices[lh_vertex_idx_precentral_gyrus, ];
    lh_mid_point = brain_hemispheres$lh$vertices[lh_vertex_idx_central_sulcus, ];
    lh_destination_point = brain_hemispheres$lh$vertices[lh_vertex_idx_postcentral_gyrus, ];

    # Optional: show source and destination points on brain surface.
    if(do_vis) {
        fsbrain::highlight.vertices.on.subject(data$subjects_dir, data$subject_id,
            verts_lh = c(lh_vertex_idx_precentral_gyrus, lh_vertex_idx_central_sulcus, lh_vertex_idx_postcentral_gyrus),
            verts_rh = NULL, views = "si",
            color_verts_lh = c("#FF0000", "#00FF00", "#0000FF"));
    }

    cat("\n## Test 1: from precentral gyrus (red) to postcentral gyrus (blue).\n");
    # Compute geodesic distance along mesh (through the central sulcus: from red down
    # along the gyral wall to green, then back up to blue).
    geod_red_blue = Rvcg::vcgGeodist(lh_tmesh3d, lh_source_point, lh_destination_point);
    # Compute Euclidian distance (air distance over the central sulcus), should be way shorter.
    euclid_red_blue = euclidian.dist(lh_source_point, lh_destination_point);
    cat(sprintf("  Geodesic distance:  %.2f mm\n", geod_red_blue));
    cat(sprintf("  Euclidian distance: %.2f mm\n", euclid_red_blue));

    cat("\n## Test 2: from central sulcus (green) to postcentral gyrus (blue).\n");
    cat("  The difference between geodesic and Euclidian distance should be small.\n");
    geod_green_blue = Rvcg::vcgGeodist(lh_tmesh3d, lh_mid_point, lh_destination_point);
    euclid_green_blue = euclidian.dist(lh_mid_point, lh_destination_point);
    cat(sprintf("  Geodesic distance:  %.2f mm\n", geod_green_blue));
    cat(sprintf("  Euclidian distance: %.2f mm\n", euclid_green_blue));

    invisible(list(geod_red_blue = geod_red_blue, euclid_red_blue = euclid_red_blue,
                   geod_green_blue = geod_green_blue, euclid_green_blue = euclid_green_blue));
}


# --- Test 3: full hemisphere distance map -----------------------------------------------

# Compute per-vertex Euclidian and geodesic distances from a source vertex on the left
# hemisphere and write them to disk as FreeSurfer morph data files ('lh.disteuclid' and
# 'lh.distgeod', in curv format) into 'outdir'. If 'do_vis' is TRUE, the distance maps are
# also visualized on the white and inflated surfaces and images are saved into 'outdir'.
run_full_hemi_distance_map <- function(data, do_vis = FALSE, outdir = ".") {
    brain_hemispheres = data$brain_hemispheres;
    lh_tmesh3d = data$lh_tmesh3d;

    verts = brain_hemispheres$lh$vertices;
    source_vert_idx = 32258;
    source_coord = verts[source_vert_idx, ];

    cat("\n## Test 3: full hemisphere distance map.\n");
    cat(sprintf("  Computing distance maps for all %d left hemisphere vertices (source vertex %d).\n", nrow(verts), source_vert_idx));

    # First compute Euclidian distance for comparison.
    euclid_dists_to_source = apply(verts, 1, euclidian.dist, source_coord);
    cat(sprintf("  Euclidian distances to source: min %.2f mm, max %.2f mm.\n", min(euclid_dists_to_source), max(euclid_dists_to_source)));
    freesurferformats::write.fs.morph(file.path(outdir, "lh.disteuclid"), euclid_dists_to_source, format = "curv");
    cat(sprintf("  Wrote '%s'.\n", file.path(outdir, "lh.disteuclid")));

    geodesic_dists_to_source = Rvcg::vcgDijkstra(lh_tmesh3d, source_vert_idx);
    cat(sprintf("  Geodesic distances to source: min %.2f mm, max %.2f mm.\n", min(geodesic_dists_to_source), max(geodesic_dists_to_source)));
    freesurferformats::write.fs.morph(file.path(outdir, "lh.distgeod"), geodesic_dists_to_source, format = "curv");
    cat(sprintf("  Wrote '%s'.\n", file.path(outdir, "lh.distgeod")));

    if(do_vis) {
        cm_euclid = fsbrain::vis.data.on.subject(data$subjects_dir, data$subject_id, morph_data_lh = euclid_dists_to_source);
        fsbrain::export(cm_euclid, colorbar_legend = sprintf("Euclidian distance to vertex %d [mm]", source_vert_idx), view_angles = c("sd_medial_lh", "sd_lateral_lh"), output_img = file.path(outdir, "dist_euclid.png"));

        cm_geod = fsbrain::vis.data.on.subject(data$subjects_dir, data$subject_id, morph_data_lh = geodesic_dists_to_source);
        fsbrain::export(cm_geod, colorbar_legend = sprintf("Geodesic distance to vertex %d [mm]", source_vert_idx), view_angles = c("sd_medial_lh", "sd_lateral_lh"), output_img = file.path(outdir, "dist_geodesic.png"));

        ## We can illustrate the difference between Euclidian and geodesic distance by
        ## plotting both (computed on the white surface) on the inflated mesh:
        cm_euclid_infl = fsbrain::vis.data.on.subject(data$subjects_dir, data$subject_id, morph_data_lh = euclid_dists_to_source, surface = "inflated", views = NULL);
        fsbrain::export(cm_euclid_infl, colorbar_legend = sprintf("Euclidian distance to vertex %d [mm]", source_vert_idx), view_angles = c("sd_medial_lh", "sd_lateral_lh"), output_img = file.path(outdir, "dist_inflated_euclid.png"));

        cm_geod_infl = fsbrain::vis.data.on.subject(data$subjects_dir, data$subject_id, morph_data_lh = geodesic_dists_to_source, surface = "inflated", views = NULL);
        fsbrain::export(cm_geod_infl, colorbar_legend = sprintf("Geodesic distance to vertex %d [mm]", source_vert_idx), view_angles = c("sd_medial_lh", "sd_lateral_lh"), output_img = file.path(outdir, "dist_inflated_geodesic.png"));
    }

    invisible(list(euclid_dists = euclid_dists_to_source, geodesic_dists = geodesic_dists_to_source));
}


# --- Command line argument parsing -----------------------------------------------------

# Parse the command line arguments given as 'args' (a character vector as returned by
# 'commandArgs(trailingOnly = TRUE)'). Returns a list with the settings.
parse_args <- function(args) {
    do_vis = FALSE;
    outdir = ".";
    if(length(args) > 0) {
        idx = 1;
        while(idx <= length(args)) {
            arg = args[idx];
            if(arg == "--vis") {
                do_vis = TRUE;
                idx = idx + 1;
            } else if(arg == "--outdir") {
                if(idx + 1 > length(args)) {
                    stop("Option '--outdir' requires a directory argument.");
                }
                outdir = args[idx + 1];
                idx = idx + 2;
            } else {
                stop(sprintf("Unknown command line argument '%s'.\nUSAGE: ./brain_surface_geodesic.R [--vis] [--outdir <dir>]", arg));
            }
        }
    }
    return(list(do_vis = do_vis, outdir = outdir));
}


# --- Main ----------------------------------------------------------------------------------

main <- function(args) {
    settings = parse_args(args);
    if(!dir.exists(settings$outdir)) {
        dir.create(settings$outdir, recursive = TRUE);
    }
    cat(sprintf("Settings: do_vis = %s, outdir = '%s'.\n", settings$do_vis, settings$outdir));

    data = load_brain_data();

    run_pairwise_distance_tests(data, do_vis = settings$do_vis);
    run_full_hemi_distance_map(data, do_vis = settings$do_vis, outdir = settings$outdir);
}


args = commandArgs(trailingOnly = TRUE);
main(args);




