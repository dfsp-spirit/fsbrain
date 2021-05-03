#!/usr/bin/env Rscript

devtools::install_github("dfsp-spirit/fsbrain"); # > 0.4.3
devtools::install_github("dfsp-spirit/freesurferformats"); # > 0.1.14
devtools::install_github("zarquon42b/Rvcg"); # > 0.19.2

library("fsbrain");
library("freesurferformats");
library("Rvcg");

fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
subject_id = "fsaverage";

brain_hemispheres = fsbrain::subject.surface(subjects_dir, subject_id, "white", hemi="both");

lh_tmesh3d = rgl::tmesh3d(c(t(brain_hemispheres$lh$vertices)), c(t(brain_hemispheres$lh$faces)), homogeneous=FALSE);
#rh_tmesh3d = rgl::tmesh3d(c(t(brain_hemispheres$rh$vertices)), c(t(brain_hemispheres$rh$faces)), homogeneous=FALSE);

lh_source_vertex_idx = 32258;  # on precentral gyrus
lh_destination_vertex_idx = 17792; # on postcentral gyrus
lh_destination_vertex_sulc_idx = 32220; # at bottom of central sulcus

lh_source_point = brain_hemispheres$lh$vertices[lh_source_vertex_idx, ];
#freesurferformats::closest.vert.to.point(brain_hemispheres$lh, lh_source_point);
lh_destination_point = brain_hemispheres$lh$vertices[lh_destination_vertex_idx, ];

# Optional: show source and dest points on brain surface.
highlight.vertices.on.subject(subjects_dir, subject_id, verts_lh = c(lh_source_vertex_idx, lh_destination_vertex_sulc_idx, lh_destination_vertex_idx), verts_rh = NULL, views = "si", color_verts_lh = c("#FF0000", "#00FF00", "#0000FF"));

# Compute geodesic distance along mesh (through the central sulcus)
Rvcg::vcgGeodist(lh_tmesh3d, lh_source_point, lh_destination_point);

# Compute Euclidian distance (air distance over the central sulcus), should be way shorter.
euclidian.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2));
euclidian.dist(lh_source_point, lh_destination_point);





