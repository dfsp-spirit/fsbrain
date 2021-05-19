

#' @title Highlight vertices given by index on a subject's meshes.
#'
#' @inheritParams vis.color.on.subject
#'
#' @inheritParams mesh.vertex.neighbors
#'
#' @param verts_lh integer vector, the indices of left hemisphere vertices.
#'
#' @param verts_rh integer vector, the indices of right hemisphere vertices.
#'
#' @param color_verts_lh vector of colors to visualize on the left hemisphere surface. Length must match number of vertices in 'verts_lh', or be a single color.
#'
#' @param color_verts_rh vector of colors to visualize on the right hemisphere surface. Length must match number of vertices in 'verts_rh', or be a single color.
#'
#' @param color_bg background color.
#'
#' @param k integer, radius to extend neighborhood (for better visibility).
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @family visualization functions
#' @family surface visualization functions
#'
#' @export
highlight.vertices.on.subject <- function(subjects_dir, vis_subject_id, verts_lh=NULL, verts_rh=NULL, surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), color_bg="#FEFEFE", color_verts_lh="#FF0000", color_verts_rh="#FF4500", k=0L) {

    coloredmeshes = list();
    nv = subject.num.verts(subjects_dir, vis_subject_id);

    color_lh = rep(color_bg, nv$lh);
    if(length(verts_lh) > 0L) {
        if(length(color_verts_lh) != length(verts_lh)) {
            if(length(color_verts_lh) == 1L) {
                color_verts_lh = rep(color_verts_lh, length(verts_lh));
            } else {
                stop(sprintf("Parameter 'color_verts_lh' has length %d, must have length 1 or same length as 'verts_lh', which is %d.", length(color_verts_lh), length(verts_lh)));
            }
        }
        color_lh[verts_lh] = color_verts_lh;
        if(k > 1L) {
            # Grow neighborhood and color it in the color of the central vertex.
            for(vertex_seq_idx in seq(verts_lh)) {
                vertex_mesh_idx = verts_lh[vertex_seq_idx];
                neighborhood = mesh.vertex.neighbors(subject.surface(subjects_dir, vis_subject_id, surface = surface, hemi = "lh"), source_vertices = vertex_mesh_idx, k = k)$vertices;
                color_lh[neighborhood] = color_verts_lh[vertex_seq_idx];
            }
        }
    }
    cmesh_lh = coloredmesh.from.color(subjects_dir, vis_subject_id, color_lh, 'lh', surface=surface);
    coloredmeshes$lh = cmesh_lh;


    color_rh = rep(color_bg, nv$rh);
    if(length(verts_rh) > 0L) {
        if(length(color_verts_rh) != length(verts_rh)) {
            if(length(color_verts_rh) == 1L) {
                color_verts_rh = rep(color_verts_rh, length(verts_rh));
            } else {
                stop(sprintf("Parameter 'color_verts_rh' has length %d, must have length 1 or same length as 'verts_rh', which is %d.", length(color_verts_rh), length(verts_rh)));
            }
        }
        color_rh[verts_rh] = color_verts_rh;
        if(k > 1L) {
            # Grow neighborhood and color it in the color of the central vertex.
            for(vertex_seq_idx in seq(verts_rh)) {
                vertex_mesh_idx = verts_rh[vertex_seq_idx];
                neighborhood = mesh.vertex.neighbors(subject.surface(subjects_dir, vis_subject_id, surface = surface, hemi = "rh"), source_vertices = vertex_mesh_idx, k = k)$vertices;
                color_rh[neighborhood] = color_verts_rh[vertex_seq_idx];
            }
        }
    }
    cmesh_rh = coloredmesh.from.color(subjects_dir, vis_subject_id, color_rh, 'rh', surface=surface);
    coloredmeshes$rh = cmesh_rh;


    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions)));
}


#' @title Draw small 3D spheres at given points.
#'
#' @param coords double vector or nx3 double matrix, the xyz point coordinates.
#'
#' @param color the sphere color, like \code{'#FF0000'} or \code{"red"}.
#'
#' @param radius double, the sphere radius
#'
#' @family 3d utility functions
#'
#' @export
highlight.points.spheres <- function(coords, color = "#FF0000", radius = 1.0) {
    if(is.vector(coords)) {
        coords = matrix(coords, ncol = 3, byrow = TRUE);
    }
    rgl::rgl.spheres(coords[,1], coords[,2], coords[,3], radius = radius, col = color);
}


#' @title Draw small 3D spheres at given brain mesh vertices. Supports full brain (2 meshes) as well.
#'
#' @param surface an fs.surface instance, see \code{\link[fsbrain]{subject.surface}} function. Can also be a hemilist of surfaces, in which case the vertices can be indices over both meshes (in range \code{1..(nv(lh)+nv(rh))}).
#'
#' @param vertices vector of positive integers, the vertex indices. Values which are outside of the valid indices for the surface will be silently ignored, making it easier to work with the two hemispheres.
#'
#' @param ... Parameters passed to \code{\link[fsbrain]{highlight.points.spheres}}.
#'
#' @note This function will draw into the current window and add to the scene, so it can be called after visualizing a mesh. See the example.
#'
#' @family 3d utility functions
#'
#' @examples
#' \dontrun{
#' lh_surf = subject.surface('~/data/study1', 'subject1',
#'  surface = "white", hemi = "lh");
#' vis.fs.surface(lh_surf, style="semitransparent");
#' highlight.vertices.spheres(lh_surf,
#'   vertices = c(3225L, 4300L, 5500L),
#'   color = c("green", "blue", "red"));
#' }
#'
#' @export
highlight.vertices.spheres <- function(surface, vertices, ...) {
    if(is.hemilist(surface)) {
        per_surface = per.hemi.vertex.indices(surface, vertices);
        lh_coords = surface$lh$vertices[per_surface$vertices$lh, ];
        rh_coords = surface$rh$vertices[per_surface$vertices$rh, ];
        coords = rbind(lh_coords, rh_coords);
    } else {
        if(! freesurferformats::is.fs.surface(surface)) {
            stop("Parameter 'surface' must be an fs.surface instance.");
        }
        # Filter invalid vertex indices. Makes it easier to work per hemisphere.
        vertices = vertices[which(vertices > 0L & vertices <= nrow(surface$vertices))];
        coords = surface$vertices[vertices, ];
    }
    highlight.points.spheres(coords, ...);
}

