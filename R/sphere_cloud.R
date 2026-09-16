# Sphere cloud geometry.
#
# Small, dependency-free generators for sphere meshes: a subdivision surface of
# the icosahedron (one vertex per mesh vertex, no welding needed) plus the
# merging of many such spheres into a single mesh. This is what the connectome
# node plot uses: one merged mesh with per-vertex colors renders as fast as a
# single mesh on both backends, while hundreds of separate sphere meshes would
# cost one draw call / one mesh conversion each.


#' @title Compute the vertices and faces of a unit icosphere (subdivision surface of the icosahedron).
#'
#' @description The returned sphere has radius 1 and is centered at the origin. The number of vertices is \code{10 * 4^subdivisions + 2} and the number of faces is \code{20 * 4^subdivisions} (42 and 80 for 1 subdivision, 162 and 320 for 2).
#'
#' @param subdivisions non-negative integer, the number of subdivision steps. 0 gives the plain icosahedron. Defaults to 2L, which is smooth enough for plot-sized spheres.
#'
#' @return named list with entries 'vertices' (n x 3 numeric matrix) and 'faces' (m x 3 integer matrix), i.e., an \code{freesurferformats::fs.surface} instance.
#'
#' @keywords internal
unit.icosphere <- function(subdivisions = 2L) {
    if(! (is.numeric(subdivisions) && length(subdivisions) == 1L && is.finite(subdivisions) && subdivisions >= 0L && abs(subdivisions - round(subdivisions)) < 1e-8)) {
        stop("Parameter 'subdivisions' must be a non-negative integer.\n");
    }
    subdivisions = as.integer(round(subdivisions));

    # The 12 vertices of the icosahedron, already scaled to unit length.
    phi = (1.0 + sqrt(5.0)) / 2.0;
    vertices = rbind(
        c(-1, phi, 0), c(1, phi, 0), c(-1, -phi, 0), c(1, -phi, 0),
        c(0, -1, phi), c(0, 1, phi), c(0, -1, -phi), c(0, 1, -phi),
        c(phi, 0, -1), c(phi, 0, 1), c(-phi, 0, -1), c(-phi, 0, 1));
    vertices = vertices / sqrt(rowSums(vertices^2));

    faces = rbind(
        c(1, 12, 6), c(1, 6, 2), c(1, 2, 8), c(1, 8, 11), c(1, 11, 12),
        c(2, 6, 10), c(6, 12, 5), c(12, 11, 3), c(11, 8, 7), c(8, 2, 9),
        c(4, 10, 5), c(4, 5, 3), c(4, 3, 7), c(4, 7, 9), c(4, 9, 10),
        c(5, 10, 6), c(3, 5, 12), c(7, 3, 11), c(9, 7, 8), c(10, 9, 2));

    for(step in seq_len(subdivisions)) {
        subdivided = subdivide.triangles(vertices, faces);
        vertices = subdivided$vertices;
        faces = subdivided$faces;
    }

    mesh = list("vertices"=vertices, "faces"=faces);
    class(mesh) = c("fs.surface", class(mesh));
    return(mesh);
}


#' @title Subdivide all triangles of a mesh (one subdivision step).
#'
#' @description Splits every triangle into 4 by adding the edge midpoints, and projects the new vertices onto the sphere, i.e., the new vertices are scaled to unit length. This is the standard icosphere refinement, and it requires the input to be a sphere centered at the origin.
#'
#' @param vertices n x 3 numeric matrix, the vertex coordinates.
#'
#' @param faces m x 3 integer matrix, the triangle vertex indices (1-based).
#'
#' @return named list with entries 'vertices' and 'faces', the refined mesh.
#'
#' @keywords internal
subdivide.triangles <- function(vertices, faces) {
    num_verts = nrow(vertices);
    num_faces = nrow(faces);

    # Identify the 3 edges of every face. Edges are unordered, so the smaller index comes first.
    edge_from = c(faces[, 1], faces[, 2], faces[, 3]);
    edge_to = c(faces[, 2], faces[, 3], faces[, 1]);
    edge_lo = pmin(edge_from, edge_to);
    edge_hi = pmax(edge_from, edge_to);

    # A unique integer key per unordered vertex pair (indices are positive, and small enough for integer arithmetic).
    num_verts = as.integer(num_verts);
    edge_key = edge_lo * (num_verts + 1L) + edge_hi;
    keep = ! duplicated(edge_key);
    unique_keys = edge_key[keep];
    unique_lo = edge_lo[keep];
    unique_hi = edge_hi[keep];

    mid_vertices = (vertices[unique_lo, , drop = FALSE] + vertices[unique_hi, , drop = FALSE]) / 2.0;
    mid_vertices = mid_vertices / sqrt(rowSums(mid_vertices^2));

    new_vertices = rbind(vertices, mid_vertices);
    mid_index = num_verts + match(edge_key, unique_keys);   # one new vertex index per (face, edge), in face-major order

    mid_ab = mid_index[seq_len(num_faces)];
    mid_bc = mid_index[num_faces + seq_len(num_faces)];
    mid_ca = mid_index[(2L * num_faces) + seq_len(num_faces)];

    a = faces[, 1]; b = faces[, 2]; c = faces[, 3];
    new_faces = rbind(
        cbind(a, mid_ab, mid_ca),
        cbind(mid_ab, b, mid_bc),
        cbind(mid_ca, mid_bc, c),
        cbind(mid_ab, mid_bc, mid_ca));

    return(list("vertices"=new_vertices, "faces"=new_faces));
}


#' @title Compute the mesh of a set of spheres.
#'
#' @description Merges one slightly refined icosphere per sphere into a single mesh, i.e., the result is a single surface with one connected component per input sphere. This is much cheaper to render (and to convert for the scimesh backend) than a list of hundreds of separate sphere meshes.
#'
#' @param centers n x 3 numeric matrix, the sphere centers.
#'
#' @param radii numeric vector of length n or a single number, the sphere radii.
#'
#' @param subdivisions non-negative integer, the number of subdivisions of the unit icosphere, see \code{\link[fsbrain]{unit.icosphere}}. Defaults to 2L.
#'
#' @return an fs.surface instance (a mesh) with one component per sphere.
#'
#' @examples
#'   m = fsbrain:::spheres.mesh(rbind(c(0, 0, 0), c(10, 0, 0)), c(1, 2));
#'   nrow(m$vertices);
#'
#' @keywords internal
spheres.mesh <- function(centers, radii = 1.0, subdivisions = 2L) {
    centers = check.segment.points(centers, 'centers');
    num_spheres = nrow(centers);

    if(! is.numeric(radii) || any(! is.finite(radii)) || any(radii <= 0.0)) {
        stop("Parameter 'radii' must be a positive number, or a vector of positive numbers (one per sphere).\n");
    }
    radii = recycle(radii, num_spheres);

    unit_sphere = unit.icosphere(subdivisions);
    num_verts = nrow(unit_sphere$vertices);
    num_faces = nrow(unit_sphere$faces);

    if(num_spheres == 0L) {
        empty = list("vertices"=matrix(numeric(0), ncol = 3L), "faces"=matrix(integer(0), ncol = 3L));
        class(empty) = c("fs.surface", class(empty));
        return(empty);
    }

    # Replicate the unit sphere num_spheres times: the vertices of sphere i occupy the rows (i-1)*num_verts + 1 .. i*num_verts.
    vertices = unit_sphere$vertices[rep(seq_len(num_verts), times = num_spheres), , drop = FALSE];
    vertices = vertices * rep(as.double(radii), each = num_verts);
    for(axis_idx in seq_len(3L)) {
        vertices[, axis_idx] = vertices[, axis_idx] + rep(centers[, axis_idx], each = num_verts);
    }

    faces = unit_sphere$faces[rep(seq_len(num_faces), times = num_spheres), , drop = FALSE];
    face_offsets = rep((seq_len(num_spheres) - 1L) * num_verts, each = num_faces);
    faces = faces + face_offsets;

    mesh = list("vertices"=vertices, "faces"=faces);
    class(mesh) = c("fs.surface", class(mesh));
    return(mesh);
}


#' @title Create a coloredmesh that draws a set of spheres (e.g., connectome nodes).
#'
#' @description Builds one mesh containing all spheres, with a per-vertex color taken from the color of the sphere the vertex belongs to. The result is a regular \code{\link[fsbrain]{fs.coloredmesh}} instance, so it can be rendered, transformed and exported like any other mesh. Note that the vertices of a sphere are not shared with any other sphere, so each sphere can have its own radius and color.
#'
#' @param centers n x 3 numeric matrix, the sphere centers.
#'
#' @param radii numeric vector of length n or a single number, the sphere radii.
#'
#' @param col vector of hex color strings, a single one or one per sphere.
#'
#' @param subdivisions non-negative integer, the number of subdivisions of the unit icosphere. Defaults to 2L.
#'
#' @param metadata named list, metadata for the resulting coloredmesh. See \code{\link[fsbrain]{fs.coloredpaths}} for the metadata convention which enables colorbars.
#'
#' @param hemi character string or NULL, the hemisphere this renderable belongs to. Defaults to NULL (not hemisphere-specific).
#'
#' @param style `NULL` or a rendering style for this mesh, see \code{\link[fsbrain]{get.rglstyle}}.
#'
#' @return fs.coloredmesh instance.
#'
#' @examples
#'   cm = fsbrain:::coloredmesh.from.spheres(rbind(c(0, 0, 0), c(10, 0, 0)), c(1, 2), c("#FF0000", "#00FF00"));
#'   class(cm);
#'
#' @keywords internal
coloredmesh.from.spheres <- function(centers, radii = 1.0, col = "#FF0000", subdivisions = 2L, metadata = list(), hemi = NULL, style = NULL) {
    centers = check.segment.points(centers, 'centers');
    num_spheres = nrow(centers);
    col = recycle(col, num_spheres);

    mesh = spheres.mesh(centers, radii = radii, subdivisions = subdivisions);
    num_verts_per_sphere = nrow(mesh$vertices) / max(num_spheres, 1L);
    vertex_colors = rep(col, each = num_verts_per_sphere);

    metadata$fs_mesh = mesh;
    cm = fs.coloredmesh(fs.surface.to.tmesh3d(mesh), vertex_colors, hemi = hemi, metadata = metadata);
    if(! is.null(style)) {
        cm$style = style;
    }
    return(cm);
}
