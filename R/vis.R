# Visualization of morphometry data on subject level.




vis.subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white") {
    morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi);
    surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    vis.morph.on(morph_data, surface_data);
}


vis.morph.on <- function(morph_data, surface_data, meshcolor="vertices") {
    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
    rgl::rgl.open();
    col = cmap(ct, map = makecmap(ct, colFn = jet));  # apply 'jet' colormap to data
    wire3d(mesh, col=col, meshcolor=meshcolor);
}


vis.annot.on <- function(annot, surface_data, meshcolor="vertices") {
    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
    rgl::rgl.open();
    col = annot$hex_colors_rgb;
    wire3d(mesh, col=col, meshcolor=meshcolor);
}

