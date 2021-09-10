#!/usr/bin/env Rscript
#
# This demo script illustrates how to compute geodesic distances on
# brain surface meshes using the Rvcg package (https://github.com/zarquon42b/Rvcg).

#devtools::install_github("dfsp-spirit/fsbrain", ref="brain_mesh_geodesic") # >= 0.4.3
#devtools::install_github("dfsp-spirit/freesurferformats"); # >= 0.1.15
#devtools::install_github("zarquon42b/Rvcg"); # > 0.19.2

library("fsbrain");
library("freesurferformats");
library("Rvcg");

do_vis = FALSE; # whether to try opening an OpengL window and plot. Turn off on headless machines without any X11.

euclidian.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2));
vertex.euclid.dist <- function(surf, v1, v2) { euclidian.dist(surf$vertices[v1, ], surf$vertices[v2, ]) };

fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
subject_id = "fsaverage";

brain_hemispheres = fsbrain::subject.surface(subjects_dir, subject_id, "white", hemi="both");

lh_tmesh3d = rgl::tmesh3d(c(t(brain_hemispheres$lh$vertices)), c(t(brain_hemispheres$lh$faces)), homogeneous=FALSE);
#rh_tmesh3d = rgl::tmesh3d(c(t(brain_hemispheres$rh$vertices)), c(t(brain_hemispheres$rh$faces)), homogeneous=FALSE);

lh_vertex_idx_precentral_gyrus = 32258;  # on precentral gyrus         (red in the image plotted later)
lh_vertex_idx_central_sulcus = 32220;    # at bottom of central sulcus (green)
lh_vertex_idx_postcentral_gyrus = 17792; # on postcentral gyrus        (blue)

lh_source_point = brain_hemispheres$lh$vertices[lh_vertex_idx_precentral_gyrus, ];
lh_mid_point = brain_hemispheres$lh$vertices[lh_vertex_idx_central_sulcus, ];
lh_destination_point = brain_hemispheres$lh$vertices[lh_vertex_idx_postcentral_gyrus, ];
#freesurferformats::closest.vert.to.point(brain_hemispheres$lh, lh_source_point);

# Optional: show source and dest points on brain surface.
if(do_vis) {
    highlight.vertices.on.subject(subjects_dir, subject_id, verts_lh = c(lh_vertex_idx_precentral_gyrus, lh_vertex_idx_central_sulcus, lh_vertex_idx_postcentral_gyrus), verts_rh = NULL, views = "si", color_verts_lh = c("#FF0000", "#00FF00", "#0000FF"));
}

########## Test 1: from point red to blue. ##########ö

# Compute geodesic distance along mesh (through the central sulcus: from red down along the gyral wall to green, then back up to blue)
Rvcg::vcgGeodist(lh_tmesh3d, lh_source_point, lh_destination_point); # 36.40924
# Compute Euclidian distance (air distance over the central sulcus), should be way shorter.
euclidian.dist(lh_source_point, lh_destination_point); # 10.54589


########## Test 2: from point green to blue. The difference should be small.  ##########
Rvcg::vcgGeodist(lh_tmesh3d, lh_mid_point, lh_destination_point); # 21.62851

# Compute Euclidian distance (air distance over the central sulcus), should be way shorter.
euclidian.dist(lh_mid_point, lh_destination_point); # 18.59061


########## Test 3: Full hemi distance map ##########

do_run_full_hemi = TRUE;
if(do_run_full_hemi) {
    verts = brain_hemispheres$lh$vertices;
    source_vert_idx = 32258;
    source_coord = verts[source_vert_idx, ];

    # First compute Euclidian distance for comparison.
    euclid_dists_to_source = apply(verts, 1, euclidian.dist, source_coord);
    if(do_vis) {
        cm_euclid = fsbrain::vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = euclid_dists_to_source);
    }
    freesurferformats::write.fs.morph("lh.disteuclid", euclid_dists_to_source, format = "curv");

    geodesic_dists_to_source = Rvcg::vcgDijkstra(lh_tmesh3d, source_vert_idx);
    if(do_vis) {
        cm_geod = fsbrain::vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = geodesic_dists_to_source);
        fsbrain::vis.export.from.coloredmeshes(cm_geod, colorbar_legend = sprintf("Geodesic distance to vertex %d [mm]", source_vert_idx), view_angles = c("sd_medial_lh", "sd_lateral_lh"));
    }
    freesurferformats::write.fs.morph("lh.distgeod", geodesic_dists_to_source, format = "curv");

    ## We can illustrate the difference between Euclidean and geodesic distance by
    ## plotting both (computed on the white surface) on the inflated mesh:
    if(do_vis) {
        cm_euclid_infl = fsbrain::vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = euclid_dists_to_source, surface = "inflated", views = NULL);
        fsbrain::vis.export.from.coloredmeshes(cm_euclid_infl, colorbar_legend = sprintf("Euclidian distance to vertex %d [mm]", source_vert_idx), view_angles = c("sd_medial_lh", "sd_lateral_lh"), output_img = "dist_inflated_euclid.png");

        cm_geod_infl = fsbrain::vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = geodesic_dists_to_source, surface = "inflated", views = NULL);
        fsbrain::vis.export.from.coloredmeshes(cm_geod_infl, colorbar_legend = sprintf("Geodesic distance to vertex %d [mm]", source_vert_idx), view_angles = c("sd_medial_lh", "sd_lateral_lh"), output_img = "dist_inflated_geodesic.png");
    }
}




