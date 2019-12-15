#' @title Transform first character of a string to uppercase.
#'
#' @description Transform first character of a string to uppercase. This is useful when labeling plots. Important: this function does not know about different encodings, languages or anything, it just calls \code{\link[base]{toupper}} for the first character.
#'
#' @param word, string. Any string.
#'
#' @return string, the input string with the first character transformed to uppercase.
#'
#' @examples
#'    word_up = fup("word");
#'
#' @export
fup <- function(word) {
  substr(word, 1, 1) <- toupper(substr(word, 1, 1));
  return(word);
}


#' @title Clip data at quantiles to remove outliers.
#'
#' @description Set all data values outside the given quantile range to the border values. This is usefull to properly visualize morphometry data that includes outliers. These outliers negatively affect the colormap, as all the non-outlier values become hard to distinguish. This function can be used to filter the data before plotting it.
#'
#' @param data, numeric vector. The input data.
#'
#' @param lower, numeric. The probability for the lower quantile, defaults to `0.05`.
#'
#' @param upper, numeric. The probability for the upper quantile, defaults to `0.95`.
#'
#' @return numeric vector. The output data.
#'
#' @examples
#'    full_data = rnorm(50, 3, 1);
#'    clipped = clip.data(full_data);
#'
#' @importFrom stats quantile
#' @export
clip.data <- function(data, lower=0.05, upper=0.95){
    quantiles = stats::quantile(data, c(lower, upper), na.rm = TRUE, names = FALSE);
    data[ data < quantiles[1] ] = quantiles[1];
    data[ data > quantiles[2] ] = quantiles[2];
    return(data);
}


#' @title Compute neighbors of a vertex
#'
#' @description Given a set of query vertex indices and a mesh *m*, compute all vertices which are adjacent to the query vertices in the mesh. A vertex *u* is *adjacent* to another vertex *v* iff there exists an edge *e = (u, v)* in *m*. While you could call this function repeatedly with the old output as its new input to extend the neighborhood, you should maybe use a proper graph library for this.
#'
#' @param surface a surface as returned by functions like \code{\link[fsbrain]{subject.surface}}.
#'
#' @param source_vertices Vector of source vertex indices.
#'
#' @return the neighbors as a list with two entries: "faces": an vector of the face indices of all faces the source_vertices are a part of. "vertices": an n x 3 matrix of the vertex indices of all vertices of the faces in the 'faces' property. These vertex indices contain the indices of the source_vertices themselves, and they can of course contain duplicates (but not within a single row of the matrix) in the case that two of the source_vertices share a neighbor.
#'
#' @family surface mesh functions
#'
#' @export
mesh.vertex.neighbors <- function(surface, source_vertices) {
  face_indices = which(apply(surface$faces, 1, function(face_vertidx) any(face_vertidx %in% source_vertices)));
  vertex_indices = surface$faces[face_indices, ];
  return(list("vertices"=vertex_indices, "faces"=face_indices))
}


#' @title Draw a 3D line from vertex to vertex
#'
#' @description To get a nice path along the surface, pass the vertex indices along a geodesic path.
#'
#' @param surface_vertices matrix of surface vertex coordinates, as returned by subject.surface, member "vertices"
#'
#' @param path_vertex_indices vector of vertex indices, the path
#'
#' @export
#' @importFrom rgl segments3d material3d
vis.path.along.verts <- function(surface_vertices, path_vertex_indices) {
  path_vertex_coords = surface_vertices[path_vertex_indices,];
  path_segments = c();

  for(vertex_row_idx in seq_len(nrow(path_vertex_coords))) {
    path_segments = c(path_segments, path_vertex_coords[vertex_row_idx,]);
    if(vertex_row_idx > 1 && vertex_row_idx < nrow(path_vertex_coords)) {
      # Add the vertex again, because the segment function always takes pairs of start and end point.
      # We want the old end point to be the next start point, so we have to duplicate the coords.
      path_segments = c(path_segments, path_vertex_coords[vertex_row_idx,]);
    }
  }


  path = matrix(path_segments, byrow = TRUE, ncol=3);
  rgl::material3d(size=2.0, lwd=2.0, color=c("red"), point_antialias=TRUE, line_antialias=TRUE);
  rgl::segments3d(path[,1], path[,2], path[,3]);
}


label.border.vertices <- function(surface, label_vertices) {
    neighborhood = mesh.vertex.neighbors(surface, label_vertices);
    edges = face.edges(surface, neighborhood$faces); # Not 100% correct: the faces here are all faces touching any of the vertices.
    # It would be better to restrict this to faces which are made up ONLY of vertices in label_vertices.
    ec = uniqueRowCounts(edges); # Count how often each edge occurs
    border_edges = ec[ec$N==1][,1:2]; # Border edges occur only once, as the other face they touch is not part of the label.
    border_vertices = unique(as.vector(t(border_edges)));
    # Now retrieve the faces from the neighborhood that include any border vertex.


}

#' @title Enumerate all edges of the given faces.
face.edges <- function(surface, face_indices) {
    e1 = surface$faces[face_indices, 1:2];
    e2 = surface$faces[face_indices, 2:3];
    e3 = surface$faces[face_indices, c(3,1)];
    return(rbind(e1, e2, e3));
}


# based on the f6prime function from
# https://stackoverflow.com/questions/28485735/fastest-way-to-count-the-occurrences-of-each-unique-column-in-a-matrix-in-r
uniqueRowCounts = function(mat) {
    dt = as.data.table(mat);
    return(dt[, .N, by = names(dt)]);
}

