
test_that("We can compute the vertex neighborhood using igraph.", {

    if(requireNamespace("igraph", quietly = TRUE)) {

        testthat::skip_on_cran();

        fsbrain::download_optional_data();
        subjects_dir = fsaverage.path(allow_fetch = TRUE);
        subject_id = 'fsaverage';

        surface = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "lh");
        g = fs.surface.to.igraph(surface);
        neighbors_igraph = igraph::neighborhood(g, order = 1, nodes = 15);
        neighbors_fsbrain = mesh.vertex.neighbors(surface, source_vertices = 15, k = 1);
        testthat::expect_true(all(sort(as.integer(unlist(neighbors_igraph))) == sort(neighbors_fsbrain$vertices)));

        neighbors_igraph_wrapped = fsbrain:::fs.surface.vertex.neighbors(surface, nodes = 15, order = 1, include_self = TRUE);
        testthat::expect_equal(sort(neighbors_igraph_wrapped), sort(neighbors_fsbrain$vertices));

    } else {
        testthat::skip("This test requires the 'igraph' package.");
    }
})

