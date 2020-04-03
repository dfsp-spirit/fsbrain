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
#' @param data, numeric vector. The input data. Can also be a hemi list.
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

    if(is.hemilist(data)) { # treat as a hemi list
      return(lapply(data, clip.data, lower, upper));
    } else {
      quantiles = stats::quantile(data, c(lower, upper), na.rm = TRUE, names = FALSE);
      data[ data < quantiles[1] ] = quantiles[1];
      data[ data > quantiles[2] ] = quantiles[2];
    }
    return(data);
}


#' @title Compute neighborhood of a vertex
#'
#' @description Given a set of query vertex indices and a mesh *m*, compute all vertices which are adjacent to the query vertices in the mesh. A vertex *u* is *adjacent* to another vertex *v* iff there exists an edge *e = (u, v)* in *m*. While you could call this function repeatedly with the old output as its new input to extend the neighborhood, you should maybe use a proper graph library for this.
#'
#' @param surface a surface as returned by functions like \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}.
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
#' @param surface_mesh surface mesh, as loaded by \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}.
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
#' @param surface_mesh brain surface mesh, as returned by functions like \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}.
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
annot.outline <- function(annotdata, surface_mesh, background="white", silent=TRUE, expand_inwards=0L) {
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
#' @description To get a nice path along the surface, pass the vertex indices along a geodesic path. Note: You can first open an interactive brain view (`view='si'`) with a vis* function like \code{\link[fsbrain]{vis.subject.morph.native}}, then run this function to draw into the active plot.
#'
#' @param surface_vertices float matrix of size (n, 3), the surface vertex coordinates, as returned as part of \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}, in the member "vertices".
#'
#' @param path_vertex_indices vector of vertex indices, the path. You will need to have it computed already. (This function does **not** compute geodesic paths. You can use it to visualize such a path though.)
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
#' @description Compute the border of a label (i.e., a subset of the vertices of a mesh). The border thickness can be specified. Useful to draw the outline of a region, e.g., a significant cluster on the surface or a part of a ROI from a brain parcellation.
#'
#' @param surface_mesh surface mesh, as loaded by \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}.
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


#' @title Enumerate all edges of the given faces or mesh.
#'
#' @description Compute edges of a tri-mesh. Can compute all edges, or only a subset, given by the face indices in the mesh.
#'
#' @param surface_mesh surface mesh, as loaded by \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}.
#'
#' @param face_indices integer vector, the face indices. Can also be the character string 'all' to use all faces.
#'
#' @return integer matrix of size (n, 2) where n is the number of edges. The indices (source and target vertex) in each row are **not** sorted, and the edges are **not** unique. I.e., each undirected edge `u, v` (with the exception of edges on the mesh border) will occur twice in the result: once as `u, v` and once as `v, u`.
#'
#' @family surface mesh functions
#'
#' @export
face.edges <- function(surface_mesh, face_indices='all') {
    if(is.character(face_indices)) {
      if(face_indices=='all') {
        face_indices = seq.int(nrow(surface_mesh$faces));
      }
    }
    e1 = surface_mesh$faces[face_indices, 1:2];
    e2 = surface_mesh$faces[face_indices, 2:3];
    e3 = surface_mesh$faces[face_indices, c(3,1)];
    return(rbind(e1, e2, e3));
}


#' @title Return diverging color list
#'
#' @param num_colors integer, the number of colors you want
#'
#' @return vector of colors
#'
#' @importFrom grDevices colorRampPalette rgb
#' @export
colorlist.brain.clusters <- function(num_colors) {
  if(num_colors %% 2 == 1L) {
    num_colors_per_side = num_colors %/% 2L;
    num_central = 1L;
  } else {
    num_colors_per_side = (num_colors %/% 2L) - 1L;
    num_central = 2L;
  }

  blue = grDevices::rgb(0.,0.,1.);
  cyan = grDevices::rgb(0., 1., 1.);
  ramp_bc = grDevices::colorRampPalette(c(cyan, blue))

  red = grDevices::rgb(1., 0., 0.);
  yellow = grDevices::rgb(1., 1., 0.);
  ramp_ry = grDevices::colorRampPalette(c(red, yellow))

  central_value = grDevices::rgb(0.8, 0.8, 0.8); # gray
  return(c(ramp_bc(num_colors_per_side), rep(central_value, num_central), ramp_ry(num_colors_per_side)));
}


#' @title Read colors from CSV file.
#'
#' @param filepath character string, path to a CSV file containing colors
#'
#' @return vector of hex color strings
#'
#' @export
#' @importFrom utils read.table
read.colorcsv <- function(filepath) {
    color_df = read.table(filepath, header = TRUE, stringsAsFactors = FALSE);
    if("rgb_hexcolorstring" %in% names(color_df)) {
        return(color_df$rgb_hexcolorstring);
    } else if("rgbint_red" %in% names(color_df) & "rgbint_green" %in% names(color_df) & "rgbint_blue" %in% names(color_df)) {
        return(grDevices::rgb(color_df$rgbint_red/255., color_df$rgbint_green/255., color_df$rgbint_blue/255.));
    } else if("rgbfloat_red" %in% names(color_df) & "rgbfloat_green" %in% names(color_df) & "rgbfloat_blue" %in% names(color_df)) {
        return(grDevices::rgb(color_df$rgbfloat_red, color_df$rgbfloat_green, color_df$rgbfloat_blue));
    } else {
        stop(sprintf("No valid color definition found in colorcsv file '%s'.", filepath));
    }
}


#' @title Wrap data into a named hemi list.
#'
#' @param data something to wrap, typically some data for a hemisphere, e.g., a vector of morphometry data values. If NULL, the name will not be created.
#'
#' @param hemi character string, one of 'lh' or 'rh'. The name to use for the data in the returned list.
#'
#' @param hemilist optional hemilist, an existing hemilist to add the entry to. If left at the default value `NULL`, a new list will be created.
#'
#' @return named list, with the 'data' in the name given by parameter 'hemi'
#'
#' @export
hemilist.wrap <- function(data, hemi, hemilist=NULL) {
  if(!(hemi %in% c("lh", "rh"))) {
    stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
  }
  if(is.null(hemilist)) {
    ret_list = list();
  } else {
    ret_list = hemilist;
  }
  if(!is.null(data)) {
    ret_list[[hemi]] = data;
  }
  return(ret_list);
}


#' @title Derive 'hemi' string from the data in a hemilist
#'
#' @param hemilist hemilist, an existing hemilist
#'
#' @return character string, one of 'lh', 'rh' or 'both'
#'
#' @export
hemilist.derive.hemi <- function(hemilist) {
  if(!is.hemilist(hemilist)) {
    stop("Parameter 'hemilist' must be a hemilist.");
  }
  if(is.null(hemilist$lh) | is.null(hemilist$rh)) {
    if(is.null(hemilist$lh)) {
      return('rh');
    } else {
      return('lh');
    }
  } else {
    return('both');
  }
}



#' @title Unwrap hemi data from a named hemi list.
#'
#' @param hemi_list named list, can have entries 'lh' and/or 'rh'
#'
#' @param hemi character string, the hemi data name to retrieve from the list. Can be NULL if the list only has a single entry.
#'
#' @param allow_null_list logical, whether to silently return NULL instead of raising an error if 'hemi_list' is NULL
#'
#' @return the data
#'
#' @export
hemilist.unwrap <- function(hemi_list, hemi=NULL, allow_null_list=FALSE) {
  if(is.null(hemi_list)) {
    if(allow_null_list) {
      return(NULL);
    } else {
      stop("Parameter 'hemi_list' must not be NULL unless 'allow_null_list' is TRUE.");
    }
  }
  if(! is.list(hemi_list)) {
    stop("Parameter 'hemi_list' must be a named list.");
  }
  if(length(hemi_list) < 1L) {
    stop("Parameter 'hemi_list' must not be empty.");
  }
  if(is.null(hemi)) {
    if(length(hemi_list) != 1L) {
      stop("Parameter 'hemi' can only be NULL if 'hemi_list' has exactly length 1.");
    }
    if("lh" %in% names(hemi_list)) {
      return(hemi_list$lh);
    } else if("rh" %in% names(hemi_list)) {
      return(hemi_list$rh);
    } else {
      stop("The entry in the 'hemi_list' must be named 'lh' or 'rh'.");
    }
  } else {
    if(!(hemi %in% c("lh", "rh"))) {
      stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh', or NULL but is '%s'.\n", hemi));
    }
    return(hemi_list[[hemi]]);
  }
}


#' @title Get combined data of hemi list
#'
#' @param hemi_list named list, can have entries 'lh' and/or 'rh'
#'
#' @return the data combined with \code{\link[base]{c}}, or NULL if both entries are NULL.
#'
#' @export
hemilist.get.combined.data <- function(hemi_list) {
  lh_data = hemilist.unwrap(hemi_list, 'lh');
  rh_data = hemilist.unwrap(hemi_list, 'rh');
  if(is.null(lh_data) | is.null(rh_data)) {
    if(is.null(lh_data) & is.null(rh_data)) {
      return(NULL);
    } else {
      return(hemilist.unwrap(hemi_list));
    }
  } else {
    return(c(lh_data, rh_data));
  }
}


#' @title Check whether x is a hemilist
#'
#' @description A hemilist is a named list with entries 'lh' and/or 'rh'.
#'
#' @param x any R object
#'
#' @return whether 'x' is a hemilist
#'
#' @export
is.hemilist <- function(x) {
  return(is.list(x) & ("lh" %in% names(x) | "rh" %in% names(x)));
}


#' @title Create final `makecmap_options` list
#'
#' @description Create final makecmap_options to pass to \code{\link[squash]{makecmap}} from existing `makecmap_options` and a colormap function. Used in the vis functions, like \code{\link[fsbrain]{vis.subject.morph.native}}, see the note.
#'
#' @param makecmap_options list of `makecmap_options` or `NULL`
#'
#' @param colormap a colormap function or `NULL`
#'
#' @param default_colormap the colormap function to use in case none is found in the other parameters
#'
#' @return valid `makecmap_options`
#'
#' @note For backwards compatibility, there are currently two different methods (parameters) to specify a colormap in the vis functions. This function merges the information from both methods.
#'
#' @keywords internal
#' @importFrom squash jet
makecmakeopts.merge <- function(makecmap_options, colormap, default_colormap=squash::jet) {
  if(is.null(makecmap_options)) {
    makecmap_options = list();
  }

  if(is.null(makecmap_options$colFn)) {
    if(is.null(colormap)) {
      warning("No valid colormap function found in parameters 'makecmap_options' or 'colormap', using the default colormap.");
      makecmap_options$colFn = default_colormap;
    } else {
      makecmap_options$colFn = colormap;
    }
  } else {
    if(!is.null(colormap)) {
      if(all.equal(makecmap_options$colFn, colormap) != TRUE) {
        warning("Two different colormap functions found in parameters 'makecmap_options' and 'colormap', using the one from 'makecmap_options'.");
      }
    }
  }
  return(makecmap_options);
}


#' @title Retrieve values from nested named lists
#'
#' @param named_list a named list
#'
#' @param listkeys vector of character strings, the nested names of the lists
#'
#' @return the value at the path through the lists, or NULL if no such path exists
#'
#' @examples
#'    data = list("regions"=list("frontal"=list("thickness"=2.3, "area"=2345)));
#'    getIn(data, c("regions", "frontal", "thickness"));       # 2.3
#'    getIn(data, c("regions", "frontal", "nosuchentry"));     # NULL
#'    getIn(data, c("regions", "nosuchregion", "thickness"));  # NULL
#'
#' @export
getIn <- function(named_list, listkeys) {
  num_keys = length(listkeys);
  if(length(named_list) < 1L | num_keys  < 1L) {
    return(NULL);
  }
  nlist = named_list;
  current_key_index = 0L;
  for(lkey in listkeys) {
    current_key_index = current_key_index + 1L;
    if(current_key_index < num_keys) {
      if(!is.list(nlist)) {
        return(NULL);
      }
      if(lkey %in% names(nlist)) {
        nlist = nlist[[lkey]];
      } else {
        return(NULL);
      }
    } else {
      if(lkey %in% names(nlist)) {
        return(nlist[[lkey]]);
      } else {
        return(NULL);
      }
    }
  }
}

#' @title Check for values in nested named lists
#'
#' @param named_list a named list
#'
#' @param listkeys vector of character strings, the nested names of the lists
#'
#' @return whether a non-NULL value exists at the path
#'
#' @examples
#'    data = list("regions"=list("frontal"=list("thickness"=2.3, "area"=2345)));
#'    hasIn(data, c("regions", "nosuchregion"));   # FALSE
#'
#' @export
hasIn <- function(named_list, listkeys) {
  return(! is.null(getIn(named_list, listkeys)));
}


#' @title Find the subject directory containing the fsaverage subject (or others) on disk.
#'
#' @description Try to find directory containing the fsaverage subject (or any other subject) by checking in the following places and returning the first path where it is found: first, the directory given by the environment variable SUBJECTS_DIR, then in the subir 'subjects' of the directory given by the environment variable FREESURFER_HOME, and finally the base dir of the package cache. See the function \code{\link[fsbrain]{download_fsaverage}} if you want to download fsaverage to your package cache and ensure it always gets found, no matter whether the environment variables are set or not.
#'
#' @param subject_id string, the subject id of the subject. Defaults to 'fsaverage'.
#'
#' @param mustWork logical. Whether the function should with an error stop if the directory cannot be found. If this is TRUE, the return value will be only the 'found_at' entry of the list (i.e., only the path of the subjects dir).
#'
#' @return named list with the following entries: "found": logical, whether it was found. "found_at": Only set if found=TRUE, the path to the fsaverage directory (NOT including the fsaverage dir itself). "found_all_locations": list of all locations in which it was found. See 'mustWork' for important information.
#'
#' @export
find.subjectsdir.of <- function(subject_id='fsaverage', mustWork=FALSE) {
  ret = list();
  ret$found = FALSE;
  ret$found_all_locations = NULL;

  guessed_path = get_optional_data_filepath(file.path("subjects_dir", subject_id), mustWork = FALSE);
  if(nchar(guessed_path) > 0L & dir.exists(guessed_path)) {
      ret$found = TRUE;
      ret$found_at = get_optional_data_filepath(file.path("subjects_dir"));
      ret$found_all_locations = c(ret$found_all_locations, ret$found_at);
  }


  fs_home=Sys.getenv("FREESURFER_HOME");
  if(nchar(fs_home) > 0) {
    guessed_path = file.path(fs_home, "subjects", subject_id);
    if(dir.exists(guessed_path)) {
      ret$found = TRUE;
      ret$found_at = file.path(fs_home, "subjects");
      ret$found_all_locations = c(ret$found_all_locations, ret$found_at);
    }
  }

  subj_dir=Sys.getenv("SUBJECTS_DIR");
  if(nchar(subj_dir) > 0) {
    guessed_path = file.path(subj_dir, subject_id);
    if(dir.exists(guessed_path)) {
      ret$found = TRUE;
      ret$found_at = subj_dir;
      ret$found_all_locations = c(ret$found_all_locations, ret$found_at);
    }
  }

  ret$found_all_locations = unique(ret$found_all_locations);

  if(mustWork) {
    if(ret$found) {
      return(ret$found_at);
    } else {
      stop(sprintf("Could not find subjects dir containing subject '%s' and parameter 'mustWork' is TRUE. Checked for directories given by environment variables FREESURFER_HOME and SUBJECTS_DIR and in package cache. Please set the environment variables by installing and configuring FreeSurfer.\n Or, if you want to download fsaverage without installing FreeSurfer, have a look at the 'download_fsaverage' function in this package.\n", subject_id));
    }
  }

  return(ret);
}


#' @title Find the FREESURFER_HOME directory on disk.
#'
#' @description Try to find directory containing the FreeSurfer installation, based on environment variables and *educated guessing*.
#'
#' @param mustWork logical. Whether the function should with an error stop if the directory cannot be found. If this is TRUE, the return value will be only the 'found_at' entry of the list (i.e., only the path of the FreeSurfer installation dir).
#'
#' @return named list with the following entries: "found": logical, whether it was found. "found_at": Only set if found=TRUE, the path to the FreeSurfer installation directory (including the directory itself). See 'mustWork' for important information.
#'
#' @export
find.freesurferhome <- function(mustWork=FALSE) {
  ret = list();
  ret$found = FALSE;

  fs_home=Sys.getenv("FREESURFER_HOME");
  if(nchar(fs_home) > 0) {
    guessed_path = file.path(fs_home);
    if(dir.exists(guessed_path)) {
      ret$found = TRUE;
      ret$found_at = guessed_path;
    }
  }

  # Check in some typical paths
  if(! ret$found) {
    if(tolower(Sys.info()[["sysname"]]) == 'darwin') {
      search_paths = c("/Applications/freesurfer");
    } else if(tolower(Sys.info()[["sysname"]]) == 'linux') {
      search_paths = c("/usr/local/freesurfer", "/opt/freesurfer");
    } else {
      # Windows, needed for AppVeyor
      search_paths = c();
    }

    user_home = Sys.getenv("HOME");
    if(nchar(user_home) > 0) {
      search_paths = c(search_paths, file.path(user_home, 'freesurfer'), file.path(user_home, 'software', 'freesurfer'), file.path(user_home, 'opt', 'freesurfer'));
    }

    for(sp in search_paths) {
      if(dir.exists(sp)) {
        ret$found = TRUE;
        ret$found_at = sp;
      }
    }

  }

  if(mustWork) {
    if(ret$found) {
      return(ret$found_at);
    } else {
      stop(sprintf("Could not find FreeSurfer installation dir and parameter 'mustWork' is TRUE. Please set the environment variables by installing and configuring FreeSurfer.\n"));
    }
  }

  return(ret);
}
