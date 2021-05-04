#!/usr/bin/env Rscript
#
# This demo script illustrates how to compute geodesic distances on
# brain surface meshes using the Rvcg package (https://github.com/zarquon42b/Rvcg).

#devtools::install_github("dfsp-spirit/fsbrain", ref="brain_mesh_geodesic") # > 0.4.3
#devtools::install_github("dfsp-spirit/freesurferformats"); # > 0.1.14
#devtools::install_github("zarquon42b/Rvcg"); # > 0.19.2

library("fsbrain");
library("freesurferformats");
library("Rvcg");

do_vis = FALSE; # whether to try opening an OpengL window and plot. Turn off on headless machines.

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

########## Test one: from point red to blue. ##########ö

# Compute geodesic distance along mesh (through the central sulcus: from red down along the gyral wall to green, then back up to blue)
Rvcg::vcgGeodist(lh_tmesh3d, lh_source_point, lh_destination_point); # 36.40924
# Compute Euclidian distance (air distance over the central sulcus), should be way shorter.
euclidian.dist(lh_source_point, lh_destination_point); # 10.54589


########## Test two: from point green to blue. The difference should be small.  ##########
Rvcg::vcgGeodist(lh_tmesh3d, lh_mid_point, lh_destination_point); # 21.62851

# Compute Euclidian distance (air distance over the central sulcus), should be way shorter.
euclidian.dist(lh_mid_point, lh_destination_point); # 18.59061

do_run_full_hemi = TRUE; # takes a bit
if(do_run_full_hemi) {
    verts = brain_hemispheres$lh$vertices;
    source_vert_idx = 32258;
    source_coord = verts[source_vert_idx, ];

    # First compute Euclidian distance for comparison.
    euclid_dists_to_source = apply(verts, 1, euclidian.dist, source_coord);
    if(do_vis) {
        fsbrain::vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = euclid_dists_to_source);
    }
    freesurferformats::write.fs.morph("lh.disteuclid", euclid_dists_to_source, format = "curv");

    geodesic_dists_to_source = Rvcg::vcgDijkstra(lh_tmesh3d, source_vert_idx);
    if(do_vis) {
        fsbrain::vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = geodesic_dists_to_source);
    }
    freesurferformats::write.fs.morph("lh.distgeod", geodesic_dists_to_source, format = "curv");
}




