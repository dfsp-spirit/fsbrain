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


#' @title Compute neighborhood of a vertex
#'
#' @description Given a set of query vertex indices and a mesh *m*, compute all vertices which are adjacent to the query vertices in the mesh. A vertex *u* is *adjacent* to another vertex *v* iff there exists an edge *e = (u, v)* in *m*. While you could call this function repeatedly with the old output as its new input to extend the neighborhood, you should maybe use a proper graph library for this.
#'
#' @param surface a surface as returned by functions like \code{\link[fsbrain]{subject.surface}}.
#'
#' @param source_vertices Vector of source vertex indices.
#'
#' @param k positive integer, how often to repeat the procedure and grow the neighborhood, using the output `vertices` as the `source_vertices` for the next iteration. Warning: settings this to high values will be very slow for large meshes.
#'
#' @param restrict_to_vertices integer vector of vertex indices. If given, the neighborhood growth will be limited to the given vertex indices. Defaults to NULL, which means the neighborhood is not restricted.
#'
#' @return the neighborhood as a list with two entries: "faces": integer vector, the face indices of all faces the source_vertices are a part of. "vertices": integer vector, the unique vertex indices of all vertices of the faces in the 'faces' property. These vertex indices include the indices of the source_vertices themselves.
#'
#' @family surface mesh functions
#'
#' @export
mesh.vertex.neighbors <- function(surface, source_vertices, k=1L, restrict_to_vertices=NULL) {
    if(k < 1L) {
      stop("Parameter k must be a positive integer.");
    }
    vertex_indices = source_vertices;
    if(is.null(restrict_to_vertices)) {
      max_neighborhood_size = nrow(surface$vertices);
    } else {
      max_neighborhood_size = length(restrict_to_vertices);
    }
    for(iter_idx in seq_len(k)) {
      if(is.null(restrict_to_vertices)) {
        #face_indices = which(apply(surface$faces, 1, function(face_vertidx) any(face_vertidx %in% vertex_indices)));
        face_indices = which(surface$faces[,1] %in% vertex_indices | surface$faces[,2] %in% vertex_indices | surface$faces[,3] %in% vertex_indices);
      } else {
        #face_indices = which(apply(surface$faces, 1, function(face_vertidx) any(face_vertidx %in% vertex_indices) && all(face_vertidx %in% restrict_to_vertices)));
        face_indices = which((surface$faces[,1] %in% restrict_to_vertices & surface$faces[,2] %in% restrict_to_vertices & surface$faces[,3] %in% restrict_to_vertices) & (surface$faces[,1] %in% vertex_indices | surface$faces[,2] %in% vertex_indices | surface$faces[,3] %in% vertex_indices));
      }
      vertex_indices = unique(as.vector(surface$faces[face_indices, ]));
      if(length(vertex_indices) == max_neighborhood_size) {
          break; # Neighborhood is already covering the whole mesh / allowed area.
      }
    }
    return(list("vertices"=vertex_indices, "faces"=face_indices))
}


#' @title Return all faces which are made up completely of the listed vertices.
#'
#' @param surface_mesh surface mesh, as loaded by \code{\link[fsbrain]{subject.surface}}
#'
#' @param source_vertices integer vector, the vertex indices.
#'
#' @return integer vector, the face indices
#'
#' @family surface mesh functions
#'
#' @keywords internal
mesh.vertex.included.faces <- function(surface_mesh, source_vertices) {
  #return(which(apply(surface_mesh$faces, 1, function(face_vertidx) all(face_vertidx %in% source_vertices))));
  return(which(surface_mesh$faces[,1] %in% source_vertices & surface_mesh$faces[,2] %in% source_vertices & surface_mesh$faces[,3] %in% source_vertices));
}


#' @title Compute outline vertex colors from annotation.
#'
#' @description For each region in an atlas, compute the outer border and color the respective vertices in the region-specific color from the annot's colortable.
#'
#' @param annotdata an annotation, as returned by functions like \code{\link[fsbrain]{subject.annot}}.
#'
#' @param surface_mesh brain surface mesh, as returned by functions like \code{\link[fsbrain]{subject.surface}}.
#'
#' @param background color, the background color to assign to the non-border parts of the regions. Defaults to 'white'.
#'
#' @param silent logical, whether to suppress status messages.
#'
#' @param expand_inwards integer, additional thickness of the borders. Increases computation time, defaults to 0L.
#'
#' @return vector of colors, one color for each mesh vertex
#'
#' @export
annot.outline <- function(annotdata, surface_mesh, background="white", silent=FALSE, expand_inwards=0L) {
    if(length(annotdata$vertices) != nrow(surface_mesh$vertices)) {
        stop(sprintf("Annotation is for %d vertices but mesh contains %d, vertex counts must match.\n", length(annotdata$vertices), nrow(surface_mesh$vertices)));
    }
    col = rep(background, length(annotdata$vertices));
    for(region_idx in seq_len(annotdata$colortable$num_entries)) {
        region_name = annotdata$colortable$struct_names[[region_idx]];
        if(!silent) {
          message(sprintf("Computing outline for region %d of %d: '%s'\n", region_idx, annotdata$colortable$num_entries, region_name));
        }
        label_vertices = label.from.annotdata(annotdata, region_name, error_on_invalid_region = FALSE);
        label_border = label.border(surface_mesh, label_vertices, expand_inwards=expand_inwards);
        col[label_border$vertices] = as.character(annotdata$colortable_df$hex_color_string_rgba[[region_idx]]);
    }
    return(col);
}


#' @title Draw a 3D line from vertex to vertex
#'
#' @description To get a nice path along the surface, pass the vertex indices along a geodesic path.
#'
#' @param surface_vertices float matrix of size (n, 3), the surface vertex coordinates, as returned by \code{\link[fsbrain]{subject.surface}}, member "vertices"
#'
#' @param path_vertex_indices vector of vertex indices, the path
#'
#' @family surface mesh functions
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


#' @title Compute border of a label.
#'
#' @param surface_mesh surface mesh, as loaded by \code{\link[fsbrain]{subject.surface}}
#'
#' @param label instance of class `fs.label` or an integer vector, the vertex indices. This function only makes sense if they form a patch on the surface, but that is not checked.
#'
#' @param inner_only logical, whether only faces consisting only of label_vertices should be considered to be label faces. If FALSE, faces containing at least one label vertex will be used. Defaults to TRUE. Leave this alone if in doubt, especially if you want to draw several label borders which are directly adjacent on the surface.
#'
#' @param expand_inwards integer, border thickness extension. If given, once the border has been computed, it is extended by the given graph distance. It is guaranteed that the border only extends inwards, i.e., it will never extend to vertices which are not part of the label.
#'
#' @param derive logical, whether the returned result should also include the border edges and faces in addition to the border vertices. Takes longer if requested, defaults to FALSE.
#'
#' @return the border as a list with the following entries: `vertices`: integer vector, the vertex indices of the border. Iff the parameter `derive` is TRUE, the following two additional fields are included: `edges`: integer matrix of size (n, 2) for n edges. Each row defines an edge by its start and target vertex. `faces`: integer vector, the face indices of the border.
#'
#' @family surface mesh functions
#'
#' @export
#' @importFrom data.table as.data.table .N
label.border <- function(surface_mesh, label, inner_only=TRUE, expand_inwards=0L, derive=FALSE) {

    if(freesurferformats::is.fs.label(label)) {
        label_vertices = label$vertexdata$vertex_index;
    } else {
        label_vertices = label;
    }

    if(length(label_vertices) == 0L) {
        return(list("vertices"=c(), "edges"=c(), "faces"=c()));
    }

    if(inner_only) {
      label_faces = mesh.vertex.included.faces(surface_mesh, label_vertices);
    } else {
      label_faces = mesh.vertex.neighbors(surface_mesh, label_vertices)$faces;
    }
    label_edges = face.edges(surface_mesh, label_faces);

    #cat(sprintf("Found %d label faces and %d label edges based on the %d label_vertices.\n", length(label_faces), nrow(label_edges), length(label_vertices)))
    if(nrow(label_edges) == 0L) {
        # return early in this case, because otherwise the line that computes border_edges below will fail (because the $N==1 part will return no rows)
        return(list("vertices"=c(), "edges"=c(), "faces"=c()));
    }

    label_edges_sorted = t(apply(label_edges, 1, sort)) %>%  as.data.frame();    # Sort start and target vertex within edge to count edges (u,v) and (v,u) as 2 occurrences of same edge later.
    #print(head(label_edges_sorted));
    edge_dt = data.table::as.data.table(label_edges_sorted);
    edgecount_dt = edge_dt[, .N, by = names(edge_dt)]; # add column 'N' which contains the counts (i.e., how often each edge occurs over all faces).
    border_edges = edgecount_dt[edgecount_dt$N==1][,1:2]; # Border edges occur only once, as the other face they touch is not part of the label.

    #cat(sprintf("Counted %d unique edges, out of those there were %d border edges which occured only once.\n", nrow(edgecount_dt), nrow(border_edges)));
    border_vertices = unique(as.vector(t(border_edges)));

    if(expand_inwards > 0L) {
      num_before_expansion = length(border_vertices);
      border_vertices = mesh.vertex.neighbors(surface_mesh, border_vertices, k=expand_inwards, restrict_to_vertices=label_vertices)$vertices;
      #cat(sprintf("Expanded border by %d, this increased the border vertex count from %d to %d.\n", expand_inwards, num_before_expansion, length(border_vertices)));
    }

    if(! derive) {
        return(list("vertices"=border_vertices));
    }

    # Now retrieve the faces from the neighborhood that include any border vertex.
    border_faces = mesh.vertex.included.faces(surface_mesh, border_vertices);

    if(expand_inwards > 0L) {
      # We still need to recompute the border edges based on the updated vertices (and derived faces).
      border_edges = face.edges(surface_mesh, border_faces);
    }

    return(list("vertices"=border_vertices, "edges"=border_edges, "faces"=border_faces));
}


#' @title Enumerate all edges of the given faces.
#'
#' @param surface_mesh surface mesh, as loaded by \code{\link[fsbrain]{subject.surface}}
#'
#' @param face_indices integer vector, the face indices
#'
#' @return integer matrix of size (n, 2) where n is the number of edges. The indices (source and target vertex) in each row are **not** sorted.
#'
#' @family surface mesh functions
#'
#' @keywords internal
face.edges <- function(surface_mesh, face_indices) {
    e1 = surface_mesh$faces[face_indices, 1:2];
    e2 = surface_mesh$faces[face_indices, 2:3];
    e3 = surface_mesh$faces[face_indices, c(3,1)];
    return(rbind(e1, e2, e3));
}


#' @keywords internal
test.surface <- function() {
  return(list("vertices"=matrix(c(0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0, 4.0, 4.0, 5.0, 5.0, 5.0, 6.0, 6.0, 6.0), ncol=3, byrow = TRUE), "faces"=matrix(c(1,2,3,1,2,4,2,5,4,2,6,5,2,3,6), ncol=3, byrow = TRUE)));
}



