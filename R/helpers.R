#' @title Transform first character of a string to uppercase.
#'
#' @description Transform first character of a string to uppercase. This is useful when labeling plots. Important: this function does not know about different encodings, languages or anything, it just calls \code{\link{toupper}} for the first character.
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


#' @title Show demo visualization to test whether fsbrain is setup correctly.
#'
#' @note This function will try to download optional data from the internet (unless the data have already been downloaded).
#'
#' @keywords internal
demo <- function() {
  fsbrain::download_optional_data();
  sjd = get_optional_data_filepath("subjects_dir");
  sj = "subject1";
  return(invisible(vis.subject.morph.native(sjd, sj, "thickness", cortex_only = T, draw_colorbar = T)));
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
    if(! freesurferformats::is.fs.surface(surface)) {
      stop("Parameter 'surface' must be an fs.surface instance.");
    }
    if(k < 1L) {
      stop("Parameter k must be a positive integer.");
    }
    if(length(source_vertices) < 1L) {
      stop("Parameter 'source_vertices' must not be empty.");
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
    return(list("vertices"=vertex_indices, "faces"=face_indices));
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
#' @param annotdata an annotation, as returned by functions like \code{\link[fsbrain]{subject.annot}}. If a character string, interpreted as a path to a file containing such data, and loaded with \code{freesurferformats::read.fs.annot}
#'
#' @param surface_mesh brain surface mesh, as returned by functions like \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}. If a character string, interpreted as a path to a file containing such data, and loaded with \code{freesurferformats::read.fs.surface}
#'
#' @param background color, the background color to assign to the non-border parts of the regions. Defaults to 'white'.
#'
#' @param silent logical, whether to suppress status messages.
#'
#' @param expand_inwards integer, additional thickness of the borders. Increases computation time, defaults to 0L.
#'
#' @param outline_color NULL or a color string (like 'black' or '#000000'), the color to use for the borders. If left at the default value `NULL`, the colors from the annotation color lookup table will be used.
#'
#' @param limit_to_regions vector of character strings or NULL, a list of regions for which to draw the outline (see \code{\link[fsbrain]{get.atlas.region.names}}). If NULL, all regions will be used. If (and only if) this parameter is used, the 'outline_color' parameter can be a vector of color strings, one color per region.
#'
#' @return vector of colors, one color for each mesh vertex
#'
#' @note Sorry for the computational time, the mesh datastructure is not ideal for neighborhood search.
#'
#' @export
# @importFrom foreach foreach
# @importFrom parallel detectCores
# @importFrom doParallel registerDoParallel
annot.outline <- function(annotdata, surface_mesh, background="white", silent=TRUE, expand_inwards=0L, outline_color=NULL, limit_to_regions=NULL) {

    if(is.character(annotdata)) {
      annotdate = freesurferformats::read.fs.annot(annotdata);
    }
    if(! freesurferformats::is.fs.annot(annotdata)) {
      stop("Parameter 'annotdata' must be an fs.annot instance.");
    }

  if(is.character(surface_mesh)) {
    surface_mesh = freesurferformats::read.fs.surface(surface_mesh);
  }
    if(! freesurferformats::is.fs.surface(surface_mesh)) {
      stop("Parameter 'surface_mesh' must be an fs.surface instance.");
    }

    if(length(annotdata$vertices) != nrow(surface_mesh$vertices)) {
        stop(sprintf("Annotation is for %d vertices but mesh contains %d, vertex counts must match.\n", length(annotdata$vertices), nrow(surface_mesh$vertices)));
    }
    col = rep(background, length(annotdata$vertices));
    #doParallel::registerDoParallel(parallel::detectCores());
    #foreach::foreach(region_idx = seq_len(annotdata$colortable$num_entries)) %dopar% {
    for(region_idx in seq_len(annotdata$colortable$num_entries)) {
        region_name = annotdata$colortable$struct_names[[region_idx]];

        region_index_in_limit_to_regions_parameter = NULL;

        if(! is.null(limit_to_regions)) {
          if(! is.character(limit_to_regions)) {
            stop("Parameter 'limit_to_regions' must be NULL or a vector of character strings.");
          }
          if(! region_name %in% limit_to_regions) {
            next;
          } else {
            region_index_in_limit_to_regions_parameter = which(limit_to_regions == region_name);
            if(length(region_index_in_limit_to_regions_parameter) != 1L) {
              stop("Regions in parameter 'limit_to_regions' must be unique.");
            }
          }
        }

        if(!silent) {
          message(sprintf("Computing outline for region %d of %d: '%s'\n", region_idx, annotdata$colortable$num_entries, region_name));
        }
        label_vertices = label.from.annotdata(annotdata, region_name, error_on_invalid_region = FALSE);
        label_border = label.border(surface_mesh, label_vertices, expand_inwards=expand_inwards);

        if(is.null(outline_color)) {
          col[label_border$vertices] = as.character(annotdata$colortable_df$hex_color_string_rgba[[region_idx]]);
        } else {
          if(length(outline_color) > 1L) {
            if(length(outline_color) != length(limit_to_regions)) {
              stop(sprintf("Number of colors in parameter 'outline_color' must be 1 or exactly the number of regions in parameter 'limit_to_regions' (%d), but is %d.\n", length(limit_to_regions), length(outline_color)));
            }
            if(! is.null(region_index_in_limit_to_regions_parameter)) {
              col[label_border$vertices] = outline_color[region_index_in_limit_to_regions_parameter];
            }
          } else {
            col[label_border$vertices] = outline_color;
          }
        }
    }
    return(col);
}


#' @title Compute annot border vertices.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @inheritParams subject.annot
#'
#' @inheritParams annot.outline
#'
#' @return hemilist of integer vectors, the vertices in the border
subject.annot.border <- function (subjects_dir, subject_id, hemi, atlas, surface="white", expand_inwards=0L, limit_to_regions=NULL) {
  if (!(hemi %in% c("lh", "rh", "both"))) {
    stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
  }
  if (hemi == "both") {
    res = list();
    res$lh = subject.annot.border(subjects_dir, subject_id, hemi="lh", atlas=atlas, surface=surface, expand_inwards=expand_inwards, limit_to_regions=limit_to_regions);
    res$rh = subject.annot.border(subjects_dir, subject_id, hemi="rh", atlas=atlas, surface=surface, expand_inwards=expand_inwards, limit_to_regions=limit_to_regions);
    return(res);
  }
  else {
    annot_file = file.path(subjects_dir, subject_id, "label", sprintf("%s.%s.annot", hemi, atlas));
    if (!file.exists(annot_file)) {
      stop(sprintf("Annotation file '%s' for subject '%s' atlas '%s' hemi '%s' cannot be accessed.\n", annot_file, subject_id, atlas, hemi));
    }
    annot = freesurferformats::read.fs.annot(annot_file);
    surface_file = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s", hemi, surface));
    if (!file.exists(surface_file)) {
      stop(sprintf("Surface file '%s' for subject '%s' surface '%s' hemi '%s' cannot be accessed.\n", surface_file, subject_id, surface, hemi));
    }
    surface = freesurferformats::read.fs.surface(surface_file);
    border_vertices = annot.outline.border.vertices(annot, surface, expand_inwards=expand_inwards, limit_to_regions=limit_to_regions);
    return(border_vertices);
  }
}


#' @title Compute the border vertices for each region in an annot.
#'
#' @inheritParams annot.outline
#'
#' @return named list, the keys are the region names and the values are vectors of integers encoding vertex indices.
#'
#' @export
annot.outline.border.vertices <- function(annotdata, surface_mesh, silent=TRUE, expand_inwards=0L, limit_to_regions=NULL) {

  if(is.character(annotdata)) {
    annotdate = freesurferformats::read.fs.annot(annotdata);
  }
  if(! freesurferformats::is.fs.annot(annotdata)) {
    stop("Parameter 'annotdata' must be an fs.annot instance.");
  }

  if(is.character(surface_mesh)) {
    surface_mesh = freesurferformats::read.fs.surface(surface_mesh);
  }
  if(! freesurferformats::is.fs.surface(surface_mesh)) {
    stop("Parameter 'surface_mesh' must be an fs.surface instance.");
  }

  if(length(annotdata$vertices) != nrow(surface_mesh$vertices)) {
    stop(sprintf("Annotation is for %d vertices but mesh contains %d, vertex counts must match.\n", length(annotdata$vertices), nrow(surface_mesh$vertices)));
  }

  border_vertices = list();

  for(region_idx in seq_len(annotdata$colortable$num_entries)) {
    region_name = annotdata$colortable$struct_names[[region_idx]];

    region_index_in_limit_to_regions_parameter = NULL;

    if(! is.null(limit_to_regions)) {
      if(! is.character(limit_to_regions)) {
        stop("Parameter 'limit_to_regions' must be NULL or a vector of character strings.");
      }
      if(! region_name %in% limit_to_regions) {
        next;
      } else {
        region_index_in_limit_to_regions_parameter = which(limit_to_regions == region_name);
        if(length(region_index_in_limit_to_regions_parameter) != 1L) {
          stop("Regions in parameter 'limit_to_regions' must be unique.");
        }
      }
    }

    if(!silent) {
      message(sprintf("Computing outline for region %d of %d: '%s'\n", region_idx, annotdata$colortable$num_entries, region_name));
    }
    label_vertices = label.from.annotdata(annotdata, region_name, error_on_invalid_region = FALSE);
    label_border = label.border(surface_mesh, label_vertices, expand_inwards=expand_inwards);
    border_vertices[[region_name]] = label_border$vertices;

  }
  return(border_vertices);
}


#' @title Draw a 3D line from vertex to vertex
#'
#' @description To get a nice path along the surface, pass the vertex indices along a geodesic path. Note: You can first open an interactive brain view (`views='si'`) with a vis* function like \code{\link[fsbrain]{vis.subject.morph.native}}, then run this function to draw into the active plot.
#'
#' @param surface_vertices float matrix of size (n, 3), the surface vertex coordinates, as returned as part of \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}, in the member "vertices". Can also be a \code{freesurferformats::fs.surface} or \code{rgl::tmesh3d} instance, in which case the coordinates are extracted automatically.
#'
#' @param path_vertex_indices vector of vertex indices, the path. You will need to have it computed already. (This function does **not** compute geodesic paths, see \code{\link[fsbrain]{geodesic.path}} for that. You can use it to visualize such a path though.) If omitted, the vertex coordinates will be traversed in their given order to create the path.
#'
#' @param do_vis logical, whether to actually draw the path.
#'
#' @param color a color string, like '#FF0000' to color the path.
#'
#' @param no_material logical, whether to use set the custom rendering material properties for path visualization using \code{rgl::material3d} before plotting. If you set this to FALSE, no material will be set and you should set it yourself before calling this function, otherwise the looks of the path are undefined (dependent on the default material on your system, or the last material call). Setting this to TRUE also means that the 'color' argument is ignored of course, as the color is part of the material.
#'
#' @return n x 3 matrix, the coordinates of the path, with appropriate ones duplicated for rgl pair-wise segments3d rendering.
#'
#' @family surface mesh functions
#'
#' @seealso \code{\link[fsbrain]{vis.paths}} if you need to draw many paths, \code{\link[fsbrain]{geodesic.path}} to compute a geodesic path.
#'
#' @examples
#' \dontrun{
#'   sjd = fsaverage.path(TRUE);
#'   surface = subject.surface(sjd, 'fsaverage3',
#'     surface = "white", hemi = "lh");
#'   p = geodesic.path(surface, 5, c(10, 20));
#'   vis.subject.morph.native(sjd, 'fsaverage3', views='si');
#'   vis.path.along.verts(surface$vertices, p[[1]]);
#' }
#'
#' @export
#' @importFrom rgl segments3d material3d
vis.path.along.verts <- function(surface_vertices, path_vertex_indices = NULL, do_vis = TRUE, color='#FF0000', no_material=FALSE) {
    if(freesurferformats::is.fs.surface(surface_vertices)) {
        surface_vertices = surface_vertices$vertices;
    } else if("mesh3d" %in% class(surface_vertices)) {
        surface_vertices = t(surface_vertices$vb[1:3,]);
    } else {
        if(! is.matrix(surface_vertices)) {
            stop("Parameter 'surface_vertices' must be an fs.surface, an rgl::tmesh3d, or an nx3 numeric matrix");
        }
    }

    if(is.null(path_vertex_indices)) {
        path_vertex_indices = seq(1L, nrow(surface_vertices));
    }

  path_vertex_coords = surface_vertices[path_vertex_indices,];

  num_path_points = nrow(path_vertex_coords);
  if(num_path_points < 2L) {
      warning(sprintf("Path won't be visible, it only contains %d vertex/vertices.\n", num_path_points));
  }

  if(num_path_points == 2L) {
      path = path_vertex_coords;
  } else {
      num_drawn_path_points = (2L * (num_path_points - 2L)) + 2L;
      path = matrix(rep(NA, (num_drawn_path_points * 3L)), ncol = 3L);
      path[1,] = path_vertex_coords[1L, ]; # copy 1st value
      path[num_drawn_path_points, ] = path_vertex_coords[num_path_points, ]; # copy last value
      inner_original_values = path_vertex_coords[2L:(num_path_points-1L),];

      target_indices_one = seq(2L, (num_drawn_path_points-1L), by = 2);
      target_indices_two = seq(3L, (num_drawn_path_points-1L), by = 2);
      path[target_indices_one,] = inner_original_values;
      path[target_indices_two,] = inner_original_values;
  }

  if(do_vis) {
      if(! no_material) {
        rgl::material3d(size=2.0, lwd=2.0, color=color, point_antialias=TRUE, line_antialias=TRUE);
      }
      rgl::segments3d(path[,1], path[,2], path[,3]);
  }

  return(invisible(path));
}


#' @title Visualize several paths in different colors.
#'
#' @inheritParams vis.path.along.verts
#'
#' @param paths list of positive integer vectors, the vertex indices of the paths
#'
#' @export
vis.paths.along.verts <- function(surface_vertices, paths, color=viridis::viridis(length(paths))) {
    if(! is.list(paths)) {
        stop("Parameter 'paths' must be a list of integer vectors.");
    }
    color = recycle(color, length(paths));
    for(p_idx in seq_along(paths)) {
        p = paths[[p_idx]];
        vis.path.along.verts(surface_vertices, p, color = color[p_idx]);
    }
}



#' @title Compute slopes of paths relative to axes.
#'
#' @inheritParams vis.paths
#'
#' @param return_angles logical, whether to return angles instead of slopes. Angles are returned in degrees, and will range from \code{-90} to \code{+90}.
#'
#' @return \code{m} x 3 matrix, each row corresponds to a path and describes the 3 slopes of the path against the 3 planes/ x, y, and z axes (in that order).
#'
#' @keywords internal
path.slopes <- function(coords_list, return_angles = FALSE) {
    if(! is.list(coords_list)) {
        stop("Parameter 'coords_list' must be a list.");
    }

    # Compute coords of first and last point of each track, we ignore the intermediate ones.
    fl = flc(coords_list); # fl is an m x 6 matrix, each row contains 6 coords: the xyz of the first and last point of each track.
    path_lengths = sqrt(abs((fl[,1]-fl[,4])*(fl[,1]-fl[,4])) +  abs((fl[,1]-fl[,4])*(fl[,1]-fl[,4])) + abs((fl[,2]-fl[,5])*(fl[,3]-fl[,6])));

    # Compute slopes.
    x_diff = fl[,1]-fl[,4]; # The variable names should maybe rather represent the planes.
    y_diff = fl[,2]-fl[,5];
    z_diff = fl[,3]-fl[,6];

    if(return_angles) {
        # TODO: fix this, see https://math.stackexchange.com/questions/463415/angle-between-two-3d-lines
        num_paths = nrow(fl);
        x_angles = atan2(x_diff, rep(0L, num_paths));
        y_angles = atan2(y_diff, rep(0L, num_paths));
        z_angles = atan2(z_diff, rep(0L, num_paths));
        return(rad2deg(cbind(x_angles, y_angles, z_angles)));
    } else {
        axes_diffs = cbind(x_diff, y_diff, z_diff);
        slopes_xyz = axes_diffs / path_lengths;
        return(slopes_xyz);
    }
}

#' @title Convert raduians to degree
#' @keywords internal
rad2deg <- function(rad) {(rad * 180) / (pi)}

#' @title Convert degree to radians
#' @keywords internal
deg2rad <- function(deg) {(deg * pi) / (180)}


#' @title Compute path color from its orientation.
#'
#' @inheritParams vis.paths
#'
#' @param use_three_colors_only logical, whether to use only three different colors, based on closest axis.
#'
#' @return \code{m} x 3 matrix, each row corresponds to a path and contains its color value (RGB, range 0..255).
#'
#' @keywords internal
path.colors.from.orientation <- function(coords_list, use_three_colors_only = FALSE) {
    if(! is.list(coords_list)) {
        stop("Parameter 'coords_list' must be a list.");
    }

    num_paths = length(coords_list);
    path_colors = matrix(rep(0L, (num_paths * 3L)), ncol = 3L); # init all white.

    if(use_three_colors_only) {
        slopes = abs(path.slopes(coords_list));
        for(path_idx in 1L:num_paths) {
            full_channel = which.min(slopes[path_idx, ]);
            path_colors[path_idx, full_channel] = 255L;
        }
    } else {
        angles = path.slopes(coords_list, return_angles = TRUE); # in degrees, -90..+90
        path_colors = cbind(as.integer(scale.to.range.zero.one(angles[,1])*255), as.integer(scale.to.range.zero.one(angles[,2])*255), as.integer(scale.to.range.zero.one(angles[,3])*255));
    }

    return(path_colors);
}

#' @title Scale given values to range 0..1.
#' @keywords internal
scale.to.range.zero.one <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}


#' @title Scale given values to range 0..1.
#'
#' @param x the numeric data
#'
#' @param ... the numeric data
#'
#' @return the scaled data
#'
#' @export
scale01 <- function(x, ...) { scale.to.range.zero.one(x, ...) }


#' @title Given a list of path coordinates, create matrix containing only the first and last point of each path.
#'
#' @inheritParams vis.paths
#'
#' @return m x 6 numeric matrix, containing the first and last point of a path per row (two 3D xyz-coords, so 6 values per row).
#'
#' @keywords internal
flc <- function(coords_list) {
    if(! is.list(coords_list)) {
        stop("Parameter 'coords_list' must be a list.");
    }
    num_tracks = length(coords_list);
    if(num_tracks < 1L) {
        stop("Empty coords_list, cannot determine first and last points of tracks.");
    }
    fl_coords = matrix(rep(NA, (num_tracks * 6L)), ncol = 6L);
    current_fl_coords_row_idx = 1L;
    for(path_idx in 1L:num_tracks) {
        current_path_coords = coords_list[[path_idx]];
        if(nrow(current_path_coords) < 2L) {
            warning(sprintf("Skipping path # %d: consists only of a single point.\n", path_idx));
            current_fl_coords_row_idx = current_fl_coords_row_idx + 1L;
            next;
        }
        fl_coords[current_fl_coords_row_idx, 1:3] = current_path_coords[1L, ]; # first point of path.
        fl_coords[current_fl_coords_row_idx, 4:6] = current_path_coords[nrow(current_path_coords), ]; # last point of path.
        current_fl_coords_row_idx = current_fl_coords_row_idx + 1L;
    }
    return(fl_coords);
}


#' @title Visualize many paths.
#'
#' @param coords_list list of \code{m} matrices, each \code{n} x 3 matrix must contain the 3D coords for one path.
#'
#' @param path_color a color value, the color in which to plot the paths.
#'
#' @note This function is a lot faster than calling \code{vis.path.along.verts} many times and having it draw each time.
#'
#' @export
vis.paths <- function(coords_list, path_color = "#FF0000") {
    if(! is.list(coords_list)) {
        stop("Parameter 'coords_list' must be a list.");
    }
    path = NULL;
    for(path_coords in coords_list) {
        if(is.null(path)) {
            path = vis.path.along.verts(path_coords, do_vis = FALSE);
        } else {
            path = rbind(path, vis.path.along.verts(path_coords, do_vis = FALSE));
        }
    }
    rgl::material3d(size = 1.0, lwd = 1.0, color = path_color, point_antialias = TRUE, line_antialias = TRUE);
    rgl::segments3d(path[,1], path[,2], path[,3]);
}


#sjd = fsaverage.path(T);
#sj = "fsaverage";
#mesh = subject.surface(sjd, sj, hemi="lh");
#lab = subject.label(sjd, sj, "cortex", hemi = "lh");
#sm = submesh.vertex(mesh, lab);

#' @title Create a submesh including only the given vertices.
#'
#' @param surface_mesh an fs.surface instance, the original mesh
#'
#' @param old_vertex_indices_to_use integer vector, the vertex indices of the 'surface_mesh' that should be used to construct the new mesh.
#'
#' @return the new mesh, made up of the given 'old_vertex_indices_to_use' and all (complete) faces that exist between the query vertices in the source mesh.
#'
#' @note THIS FUNCTION IS NOT IMPLEMENTED YET, THE RETURNED MESH IS BROKEN.
#'
#' @keywords internal
#' @importFrom stats complete.cases
submesh.vertex <- function(surface_mesh, old_vertex_indices_to_use) {

  if(! is.vector(old_vertex_indices_to_use)) {
    stop("Argument 'old_vertex_indices_to_use' must be a vector.");
  }
  old_vertex_indices_to_use = as.integer(old_vertex_indices_to_use);

  nv_old = nrow(surface_mesh$vertices);
  if(min(old_vertex_indices_to_use) < 1L | max(old_vertex_indices_to_use) > nv_old) {
    stop("Invalid 'old_vertex_indices_to_use' parameter: must be integer vector containing values >=1 and <=num_verts(surface_mesh).");
  }

  nv_new = length(old_vertex_indices_to_use);

  vert_mapping = rep(NA, nv_old);

  # Create a map from the old vertex indices to the new ones. Needed to construct faces later.
  mapped_new_vertex_index = 1L;
  vertex_is_retained = rep(FALSE, nv_old);
  vertex_is_retained[old_vertex_indices_to_use] = TRUE;
  for(old_vert_idx in seq(nv_old)) {
    if(vertex_is_retained[old_vert_idx]) {
      vert_mapping[old_vert_idx] = mapped_new_vertex_index;
      mapped_new_vertex_index = mapped_new_vertex_index + 1L;
    } # no 'else' needed, the rest stays at NA.
  }

  # Use the subset of the old vertices (simply grab coords).
  new_vertices = surface_mesh$vertices[old_vertex_indices_to_use, ];
  cat(sprintf("new_vertices has dim %d, %d.\n", dim(new_vertices)[1], dim(new_vertices)[2]));
  if(nv_new != dim(new_vertices)[1]) {
    stop("New vertex count does not match expectation.");
  }

  # Now for the faces.
  nf_old = nrow(surface_mesh$faces);
  new_faces = matrix(rep(NA, nf_old*3), ncol=3, nrow=nf_old); #over-allocate and remove invalid ones later.
  #new_faces = matrix(ncol=3, nrow=0);

  new_face_idx = 0L;
  for(old_face_idx in seq(nf_old)) {
    new_face_idx = new_face_idx + 1L;
    old_face_indices = surface_mesh$faces[old_face_idx, ];
    new_face_indices = vert_mapping[old_face_indices];

    #cat(sprintf("Checking old face %d with old verts %d %d %d and new verts %d %d %d.\n", old_face_idx, old_face_indices[1], old_face_indices[2], old_face_indices[3], new_face_indices[1], new_face_indices[2], new_face_indices[3]));
    new_faces[new_face_idx, ] = new_face_indices;

    #if(! any(is.na(new_face_indices))) {
    #  # All vertices of the old face have a valid mapping in the new mesh, add the new face.
    #  new_faces = rbind(new_faces, new_face_indices);
    #}
  }

  df = data.frame(new_faces);
  cat(sprintf("Full faces Df has %d rows.\n", nrow(df)));
  new_faces = data.matrix(df[stats::complete.cases(df),]); # remove all faces containing an NA vertex
  cat(sprintf("Filtered Face matrix has %d rows.\n", nrow(new_faces)));

  new_mesh = list('vertices'=new_vertices, 'faces'=new_faces); #, 'vert_mapping'=vert_mapping); # the sub mesh
  class(new_mesh) = c(class(new_mesh), 'fs.surface');
  return(new_mesh);
}


#' @keywords internal
label.border.fast <- function(surface_mesh, label) {
  if(freesurferformats::is.fs.label(label)) {
    label_vertices = label$vertexdata$vertex_index;
  } else {
    label_vertices = label;
  }

  #label_mesh = submesh.vertex(surface_mesh, label_vertices);
  return(NULL);
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

    #if(expand_inwards == 0L & derive == FALSE & inner_only == TRUE) {
    #  if(requireNamespace("Rvcg", quietly = TRUE)) {
    #    return(label.border.fast(surface_mesh, label));
    #  }
    #}

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

    label_edges_sorted = as.data.frame(t(apply(label_edges, 1, sort)));    # Sort start and target vertex within edge to count edges (u,v) and (v,u) as 2 occurrences of same edge later.
    edge_dt = data.table::as.data.table(label_edges_sorted);
    edgecount_dt = edge_dt[, .N, by = names(edge_dt)]; # add column 'N' which contains the counts (i.e., how often each edge occurs over all faces).
    border_edges = edgecount_dt[edgecount_dt$N==1][,1:2]; # Border edges occur only once, as the other face they touch is not part of the label.

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


#' @title Check for the given color strings whether they represent gray scale colors.
#'
#' @param col_strings vector of RGB(A) color strings, like \code{c("#FFFFFF", ("#FF00FF"))}.
#'
#' @param accept_col_names logical, whether to accept color names like 'white'. Disables all sanity checks.
#'
#' @return logical vector
#'
#' @examples
#' colors.are.grayscale(c("#FFFFFF", "#FF00FF"));
#' all((colors.are.grayscale(c("#FFFFFF00", "#ABABABAB"))));
#'
#' @export
#' @importFrom grDevices col2rgb
colors.are.grayscale <- function(col_strings, accept_col_names=TRUE) {

  if(! accept_col_names) {
    if(all(nchar(col_strings) == 9)) {
      has_alpha = TRUE;
    } else if(all(nchar(col_strings) == 7)) {
      has_alpha = FALSE;
    } else {
      stop("Invalid input: parameter 'colstring' must contain RBG or RGBA color strings with 7 chars each for RGB or 9 chars each for RGBA.");
    }

    if(has_alpha) {
      col_strings = substr(col_strings, 1, 7); # The alpha is irrelevant for determining whether the color is grayscale, strip it.
    }
  }

  return(unname(unlist(apply(grDevices::col2rgb(col_strings), 2, function(x){length(unique(x)) == 1}))));
}


#' @title Check for the given color strings whether they have transparency, i.e., an alpha channel value != fully opaque.
#'
#' @param col_strings vector of RGB(A) color strings, like \code{c("#FFFFFF", ("#FF00FF"))}.
#'
#' @param accept_col_names logical, whether to accept color names like 'white'. Disables all sanity checks.
#'
#' @return logical vector
#'
#' @examples
#' colors.have.transparency(c("#FFFFFF", "#FF00FF", "#FF00FF00", "red", "#FF00FFDD"));
#' all((colors.have.transparency(c("#FFFFFF00", "#ABABABAB"))));
#'
#' @export
#' @importFrom grDevices col2rgb
colors.have.transparency <- function(col_strings, accept_col_names=TRUE) {

  if(! accept_col_names) {
    if(! (all(nchar(col_strings) == 9) || (all(nchar(col_strings) == 9)))) {
      stop("These strings do not look like RGBA color strings: invalid number of chars (expected 7 or 9 for RGB/RGBA).");
    }
  }
  return(unname(unlist(apply(grDevices::col2rgb(col_strings, alpha=TRUE), 2, function(x){x[4] != 255L}))));
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


#' @title Retrieve values from nested named lists
#'
#' @param named_list a named list
#'
#' @param listkeys vector of character strings, the nested names of the lists
#'
#' @param default the default value to return in case the requested value is `NULL`.
#'
#' @return the value at the path through the lists, or `NULL` (or the 'default') if no such path exists.
#'
#' @examples
#'    data = list("regions"=list("frontal"=list("thickness"=2.3, "area"=2345)));
#'    getIn(data, c("regions", "frontal", "thickness"));       # 2.3
#'    getIn(data, c("regions", "frontal", "nosuchentry"));     # NULL
#'    getIn(data, c("regions", "nosuchregion", "thickness"));  # NULL
#'    getIn(data, c("regions", "nosuchregion", "thickness"), default=14);  # 14
#'
#' @export
getIn <- function(named_list, listkeys, default=NULL) {
  num_keys = length(listkeys);
  if(length(named_list) < 1L | num_keys  < 1L) {
    return(default);
  }
  nlist = named_list;
  current_key_index = 0L;
  for(lkey in listkeys) {
    current_key_index = current_key_index + 1L;
    if(current_key_index < num_keys) {
      if(!is.list(nlist)) {
        return(default);
      }
      if(lkey %in% names(nlist)) {
        nlist = nlist[[lkey]];
      } else {
        return(default);
      }
    } else {
      if(lkey %in% names(nlist)) {
        return(nlist[[lkey]]);
      } else {
        return(default);
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
#' @seealso \code{\link{fsaverage.path}}
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


  fs_home_search_res = find.freesurferhome();
  if(fs_home_search_res$found) {
    fs_home = fs_home_search_res$found_at;
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


#' @title Return path to fsaverage dir.
#'
#' @param allow_fetch logical, whether to allow trying to download it.
#'
#' @return the path to the fsaverage directory (NOT including the 'fsaverage' dir itself).
#'
#' @note This function will stop (i.e., raise an error) if the directory cannot be found. The fsaverage template is part of FreeSurfer, and distributed under the FreeSurfer software license.
#'
#' @export
fsaverage.path <- function(allow_fetch = FALSE) {

  search_res = find.subjectsdir.of(subject_id='fsaverage', mustWork=FALSE);
  if(search_res$found) {
    return(search_res$found_at);
  }

  if(allow_fetch) {
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
  }
  return(find.subjectsdir.of(subject_id='fsaverage', mustWork=TRUE));
}


#' @title Return FreeSurfer path.
#'
#' @return the FreeSurfer path, typically what the environment variable `FREESURFER_HOME` points to.
#'
#' @note This function will stop (i.e., raise an error) if the directory cannot be found.
#'
#' @export
fs.home <- function() {
    return(find.freesurferhome(mustWork=TRUE));
}


#' @title Find the FREESURFER_HOME directory on disk.
#'
#' @description Try to find directory containing the FreeSurfer installation, based on environment variables and *educated guessing*.
#'
#' @param mustWork logical. Whether the function should with an error stop if the directory cannot be found. If this is TRUE, the return value will be only the 'found_at' entry of the list (i.e., only the path of the FreeSurfer installation dir).
#'
#' @return named list with the following entries: "found": logical, whether it was found. "found_at": Only set if found=TRUE, the path to the FreeSurfer installation directory (including the directory itself). See 'mustWork' for important information.
#'
#' @seealso \code{\link{fs.home}}
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


#' @title Get rgloptions for testing.
#'
#' @description This function defines the figure size that is used during the unit tests. Currently \code{list('windowRect' = c(50, 50, 800, 800)}.
#'
#' @return named list, usable as 'rgloptions' parameter for vis functions like \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @export
rglot <- function() {
    return(list('windowRect' = c(50, 50, 800, 800)));
}


#' @title Get rgloptions and consider global options.
#'
#' @description This function retrieves the global rgloptions defined in \code{getOption('fsbrain.rgloptions')}, or, if this is not set, returns the value from \code{\link{rglot}}.
#'
#' @return named list, usable as 'rgloptions' parameter for vis functions like \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @note You can set the default size for all fsbrain figures to 1200x1200 pixels like this: \code{options("fsbrain.rgloptions"=list("windowRect"=c(50,50,1200,1200)))}.
#'
#' @export
rglo <- function() {
  return(getOption('fsbrain.rgloptions', default=rglot()));
}


#' @title Set default figure size for fsbrain visualization functions.
#'
#' @param width integer, default figure width in pixels
#'
#' @param height integer, default figure height in pixels
#'
#' @param xstart integer, default horizontal position of plot window on screen, left border is 0. The max value (right border) depends on your screen resolution.
#'
#' @param ystart integer, default vertical position of plot window on screen, upper border is 0. The max value (lower border) depends on your screen resolution.
#'
#' @note This function overwrites \code{options("fsbrain.rgloptions")}. Output size is limited by your screen resolution. To set your preferred figure size for future R sessions, you could call this function in your \code{'~/.Rprofile'} file.
#'
#' @export
fsbrain.set.default.figsize <- function(width, height, xstart=50L, ystart=50L) {
    options("fsbrain.rgloptions"=list("windowRect"=c(xstart, ystart, width, height)));
}



#' @title Split morph data vector at hemisphere boundary.
#'
#' @description Given a single vector of per-vertex data for a mesh, split it at the hemi boundary. This is achieved by loading the respective surface and checking the number of vertices for the 2 hemispheres.
#'
#' @param vdata numerical vector of data for both hemispheres, one value per vertex
#'
#' @param surface the surface to load to determine the vertex counts
#'
#' @param expand logical, whether to allow input of length 1, and expand (repeat) it to the length of the hemispheres.
#'
#' @inheritParams subject.morph.native
#'
#' @note Instead of calling this function to split the data, you could use the 'split_by_hemi' parameter of \code{\link[fsbrain]{subject.morph.native}}.
#'
#' @return a hemilist, each entry contains the data part of the respective hemisphere.
#' @export
vdata.split.by.hemi <- function(subjects_dir, subject_id, vdata, surface='white', expand=TRUE) {
  nv = subject.num.verts(subjects_dir, subject_id, surface=surface);
  nv_sum = nv$lh + nv$rh;
  if(length(vdata) == 1L && expand) {
    vdata = rep(vdata, nv_sum);
  }
  if(length(vdata) != nv_sum) {
    if(length(vdata) == (163842L*2L)) {
      warning("Hint: The length of 'vdata' matches the number of vertices in the fsaverage template. Wrong 'subject_id' parameter with standard space data?");
    }
    stop(sprintf("Cannot split data: surfaces contain a total of %d vertices (lh=%d, rh=%d), but vdata has length %d. Lengths must match.\n", (nv$lh + nv$rh), nv$lh, nv$rh, length(vdata)));
  }
  return(list('lh'=vdata[1L:nv$lh], 'rh'=vdata[(nv$lh+1L):(nv$lh + nv$rh)]));
}


#' @title Generate test 3D volume of integers. The volume has an outer background area (intensity value 'bg') and an inner foreground areas (intensity value 200L).
#'
#' @param vdim integer vector of length 3, the dimensions
#'
#' @param bg value to use for outer background voxels. Typically `0L` or `NA`.
#'
#' @note This function exists for software testing purposes only, you should not use it in client code.
#'
#' @return a 3d array of integers
#' @export
gen.test.volume <- function(vdim=c(256L, 256L, 256L), bg = NA) {
  data3d = rep(bg, prod(vdim));
  v3d = array(data = data3d, dim = vdim);
  vcenter = vdim %/% 2;
  vcore_start = vcenter %/% 2;
  vcore_end = vdim - vcore_start;
  v3d[vcore_start[1]:vcore_end[1],vcore_start[2]:vcore_end[2],vcore_start[3]:vcore_end[3]] = 200L;
  return(v3d);
}
