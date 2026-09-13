test_that("Label border can be computed", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = 'subject1';
    surface = 'white';
    hemi = 'lh';
    atlas = 'aparc';
    region = 'bankssts';

    # Create a label
    lh_annot = subject.annot(subjects_dir, subject_id, hemi, atlas);
    lh_label = label.from.annotdata(lh_annot, region);

    # Load a surface
    lh_surf = subject.surface(subjects_dir, subject_id, surface, hemi);

    lh_label_border = label.border(lh_surf, lh_label);
    #vis.labeldata.on.subject(subjects_dir, subject_id, lh_label_border$vertices, NULL);
    testthat::expect_equal(length(lh_label_border$vertices), 188L);
})


test_that("Label border can be computed, thickened and visualized", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    testthat::skip_if_not(box.can.run.all.tests(), "This test requires X11 and all test data.");
    skip_if_rgl_window_required();

    fsbrain::download_optional_data();

    subjects_dir = testdatapath.subjectsdir.full.subject1();
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = 'subject1';
    surface = 'white';
    hemi = 'lh';

    # Load surface mesh
    mesh = subject.surface(subjects_dir, subject_id, surface, hemi);

    # Create 3 labels. We just use random points and grow a neighborhood around them.
    l1 = mesh.vertex.neighbors(mesh, c(121543), k=5);
    l2 = mesh.vertex.neighbors(mesh, c(83862), k=5);
    l3 = mesh.vertex.neighbors(mesh, c(46324), k=5);


    l2_border = label.border(mesh, l2$vertices);
    l3_border_thick = label.border(mesh, l3$vertices, expand_inwards=2L);

    vis.labeldata.on.subject(subjects_dir, subject_id, c(l1$vertices, l2_border$vertices, l3_border_thick$vertices), NULL, surface = "inflated");

    # Another way to visualize this would be by constructing a mask from several labels. Or by merging them into an annotation:
    label_vertices_by_region = list("region1"=l1$vertices, "region2"=l2_border$vertices, "region3"=l3_border_thick$vertices);
    annot = label.to.annot(label_vertices_by_region, nrow(mesh$vertices));
    vis.subject.annot(subjects_dir, subject_id, annot, hemi, surface = "inflated");

    testthat::expect_equal(1L, 1L);   # empty tests will be skipped
})


test_that("The borders of all annotation regions can be computed", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    testthat::skip_if_not(box.can.run.all.tests(), "This test requires X11 and takes a while.");

    fsbrain::download_optional_data();
    subjects_dir = testdatapath.subjectsdir.full.subject1();
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = 'subject1';
    surface = 'inflated';
    hemi = 'lh';

    # Load surface mesh
    annot = subject.annot(subjects_dir, subject_id, hemi, "aparc");
    mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    vertex_colors = annot.outline(annot, mesh);  # What we came for: compute outlines of all annot regions


    # We could show morphometry data (or whatever) in the white inner parts, but that is a bit overkill imo.
    # It is still demonstrated here:
    show_background_morph = TRUE;
    if(show_background_morph) {
        ct = subject.morph.native(subjects_dir, subject_id, "thickness", hemi);
        vertex_colors_thickness = adjustcolor(squash::cmap(ct, map = squash::makecmap(ct, colFn = squash::jet)), alpha.f = 0.5);
        wi = which(vertex_colors=="white");
        vertex_colors[vertex_colors=="white"] = vertex_colors_thickness[wi];
    }

    vis.color.on.subject(subjects_dir, subject_id, vertex_colors_thickness, NULL);

    testthat::expect_equal(1L, 1L);   # empty tests will be skipped
})


#' @title Build a small planar triangulated grid mesh for unit tests.
#'
#' @description Creates an fs.surface instance which is a flat grid of quads, each of which is split into 2 triangles. Vertex 1 is located at position (1, 1), the vertices are numbered row-wise. This mesh is open, i.e., it has a boundary, which makes it useful to test border and neighborhood functions without requiring any downloaded test data.
#'
#' @param nrow integer, the number of vertex rows.
#'
#' @param ncol integer, the number of vertex columns.
#'
#' @return fs.surface instance
#'
#' @keywords internal
make.test.grid.mesh <- function(nrow = 4L, ncol = 4L) {
    vertices = matrix(0., nrow = nrow * ncol, ncol = 3L);
    vertex_id_at = matrix(0L, nrow = nrow, ncol = ncol);
    vertex_idx = 0L;
    for(row_idx in seq_len(nrow)) {
        for(col_idx in seq_len(ncol)) {
            vertex_idx = vertex_idx + 1L;
            vertex_id_at[row_idx, col_idx] = vertex_idx;
            vertices[vertex_idx, ] = c(as.numeric(col_idx), as.numeric(row_idx), 0.);
        }
    }
    faces = matrix(0L, nrow = 2L * (nrow - 1L) * (ncol - 1L), ncol = 3L);
    face_idx = 0L;
    for(row_idx in seq_len(nrow - 1L)) {
        for(col_idx in seq_len(ncol - 1L)) {
            v1 = vertex_id_at[row_idx, col_idx];
            v2 = vertex_id_at[row_idx, col_idx + 1L];
            v3 = vertex_id_at[row_idx + 1L, col_idx + 1L];
            v4 = vertex_id_at[row_idx + 1L, col_idx];
            face_idx = face_idx + 1L;
            faces[face_idx, ] = c(v1, v2, v3);
            face_idx = face_idx + 1L;
            faces[face_idx, ] = c(v1, v3, v4);
        }
    }
    return(structure(list(vertices = vertices, faces = faces), class = "fs.surface"));
}


test_that("Label borders can be computed on a synthetic grid mesh without downloaded data", {
    mesh = make.test.grid.mesh(4L, 4L);
    expect_equal(nrow(mesh$vertices), 16L);
    expect_equal(nrow(mesh$faces), 18L);

    # The border of the whole open mesh consists of the 12 vertices on the outer boundary.
    b_all = label.border(mesh, seq_len(16L));
    expect_equal(names(b_all), "vertices");
    expect_equal(typeof(b_all$vertices), "integer");
    expect_equal(sort(b_all$vertices), c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 12L, 13L, 14L, 15L, 16L));

    # A 2x2 block of interior vertices: the border consists of exactly those 4 vertices. The vertex order is the order in which the border edges are encountered.
    b_block = label.border(mesh, c(6L, 7L, 10L, 11L));
    expect_equal(b_block$vertices, c(6L, 7L, 11L, 10L));

    # A single interior vertex is part of no face which consists only of label vertices, so the border is empty.
    b_noface = label.border(mesh, 6L);
    expect_equal(length(b_noface$vertices), 0L);
    expect_equal(names(b_noface), c("vertices", "edges", "faces"));

    # If we also allow faces which contain at least one label vertex, we get the vertices of all faces around vertex 6.
    b_single = label.border(mesh, 6L, inner_only = FALSE);
    expect_equal(b_single$vertices, c(1L, 2L, 7L, 11L, 10L, 5L));

    # Empty labels are handled and result in empty vertex, edge and face lists.
    b_empty = label.border(mesh, integer(0));
    expect_equal(length(b_empty$vertices), 0L);
    expect_equal(names(b_empty), c("vertices", "edges", "faces"));

    # Labels which do not form any face result in an empty border as well.
    b_two_verts = label.border(mesh, c(1L, 2L));
    expect_equal(length(b_two_verts$vertices), 0L);

    # Border expansion by 1 adds the vertex in the center of the 3x3 block, but stays within the label.
    block_3x3 = c(1L, 2L, 3L, 5L, 6L, 7L, 9L, 10L, 11L);
    b_3x3 = label.border(mesh, block_3x3);
    expect_equal(sort(b_3x3$vertices), c(1L, 2L, 3L, 5L, 7L, 9L, 10L, 11L));
    b_3x3_expanded = label.border(mesh, block_3x3, expand_inwards = 1L);
    expect_equal(sort(b_3x3_expanded$vertices), block_3x3);   # all 9 vertices are border vertices now
    expect_true(all(b_3x3_expanded$vertices %in% block_3x3)); # expansion never leaves the label

    # The border edges and faces can be derived on request.
    b_derived = label.border(mesh, c(6L, 7L, 10L, 11L), derive = TRUE);
    expect_equal(b_derived$vertices, c(6L, 7L, 11L, 10L));
    expect_equal(colnames(b_derived$edges), c("V1", "V2"));
    expect_equal(dim(as.matrix(b_derived$edges)), c(4L, 2L));
    expect_equal(unname(as.matrix(b_derived$edges)), matrix(c(6L, 7L, 7L, 11L, 10L, 11L, 6L, 10L), ncol = 2L, byrow = TRUE));
    expect_equal(b_derived$faces, c(9L, 10L));

    # When the border is expanded, the derived edges are recomputed from the expanded faces.
    b_derived_expanded = label.border(mesh, c(6L, 7L, 10L, 11L), derive = TRUE, expand_inwards = 1L);
    expect_equal(dim(as.matrix(b_derived_expanded$edges)), c(6L, 2L));
    expect_equal(b_derived_expanded$faces, c(9L, 10L));
})


test_that("Label borders can be computed on the demo cube mesh", {
    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
    expect_equal(nrow(cube$vertices), 8L);

    # The cube is a closed mesh, so a label consisting of all vertices has no border.
    expect_equal(length(label.border(cube, seq_len(8L))$vertices), 0L);

    # A single face: all 3 of its vertices are border vertices.
    expect_equal(label.border(cube, c(1L, 3L, 4L))$vertices, c(1L, 3L, 4L));

    # Two adjacent faces: they share an edge, but all 4 vertices are border vertices.
    b_two_faces = label.border(cube, c(1L, 2L, 3L, 4L));
    expect_equal(sort(b_two_faces$vertices), c(1L, 2L, 3L, 4L));

    # Two vertices which do not form a face: empty border.
    expect_equal(length(label.border(cube, c(1L, 3L))$vertices), 0L);

    # Derived border edges of a single face form a triangle.
    b_derived = label.border(cube, c(1L, 3L, 4L), derive = TRUE);
    expect_equal(dim(as.matrix(b_derived$edges)), c(3L, 2L));
    expect_equal(b_derived$faces, 1L);
})


