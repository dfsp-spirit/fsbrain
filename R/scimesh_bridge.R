# Bridge functions between fsbrain and scimesh renderer backend.


#' @title Convert an R color to an RGBA float vector
#'
#' @description Converts an R color specification (hex string like "#FF0000",
#' color name like "white", or "#FF0000FF") to a length-4 RGBA numeric vector
#' with values in the range 0..1.
#'
#' @param color character string, any valid R color specification.
#'
#' @return numeric vector of length 4, RGBA values in 0..1 range.
#'
#' @keywords internal
color_to_rgba <- function(color) {
    if (length(color) != 1L || !is.character(color)) {
        stop("color must be a single character string")
    }
    if (is.na(color)) {
        # NA is used for transparent/missing vertices (e.g., masked cortex or
        # data clipped to NA). Treat it as fully transparent, like rgl does.
        return(c(0, 0, 0, 0))
    }
    if (startsWith(color, "#")) {
        hex <- substring(color, 2L)
        if (nchar(hex) == 6L) {
            r <- strtoi(substring(hex, 1L, 2L), 16L) / 255.0
            g <- strtoi(substring(hex, 3L, 4L), 16L) / 255.0
            b <- strtoi(substring(hex, 5L, 6L), 16L) / 255.0
            return(c(r, g, b, 1.0))
        } else if (nchar(hex) == 8L) {
            r <- strtoi(substring(hex, 1L, 2L), 16L) / 255.0
            g <- strtoi(substring(hex, 3L, 4L), 16L) / 255.0
            b <- strtoi(substring(hex, 5L, 6L), 16L) / 255.0
            a <- strtoi(substring(hex, 7L, 8L), 16L) / 255.0
            return(c(r, g, b, a))
        } else {
            stop("Hex color must be 6 or 8 characters (after '#'), e.g., '#FF0000' or '#FF0000FF'")
        }
    } else {
        rgb_vals <- grDevices::col2rgb(color, alpha = FALSE) / 255.0
        return(c(rgb_vals[1], rgb_vals[2], rgb_vals[3], 1.0))
    }
}

#' @title Convert hex color string to RGBA float vector
#'
#' @description Converts a hex color string like "#FF0000" or "#FF0000FF"
#' to a length-4 RGBA numeric vector with values in the range 0..1.
#'
#' @param hex character string, a hex color code with 6 or 8 digits.
#'
#' @return numeric vector of length 4, RGBA values in 0..1 range.
#'
#' @keywords internal
hex_to_rgba <- function(hex) {
    return(color_to_rgba(hex))
}


#' @title Convert a vector of hex colors to an Nx4 RGBA float matrix
#'
#' @param hex_colors character vector of hex color strings.
#'
#' @return Nx4 numeric matrix of RGBA values in the range 0..1.
#'
#' @keywords internal
hex_colors_to_rgba_matrix <- function(hex_colors) {
    if (!is.character(hex_colors)) {
        stop("hex_colors must be a character vector")
    }
    rgba <- t(vapply(hex_colors, hex_to_rgba, numeric(4L), USE.NAMES = FALSE))
    colnames(rgba) <- c("R", "G", "B", "A")
    return(rgba)
}


#' @title Extract the alpha value from resolved style parameters
#'
#' @param style_params a named list of style parameters (rgl material3d style).
#'
#' @return numeric scalar alpha in 0..1, defaults to 1 (opaque) if not set.
#'
#' @keywords internal
apply.style.alpha <- function(style_params) {
    if (is.list(style_params) && !is.null(style_params$alpha)) {
        return(as.numeric(style_params$alpha)[1L])
    }
    return(1.0)
}


#' @title Convert a single fs.coloredmesh to a scimesh mesh descriptor
#'
#' @param cmesh an fs.coloredmesh instance.
#'
#' @param style a rendering style: a style name, a named list of style
#'   parameters, or 'from_mesh' (use cmesh$style). Only the alpha value is
#'   consumed here (per-mesh vertex alpha).
#'
#' @return a scimesh mesh descriptor list with vertices, triangles, and colors.
#'
#' @keywords internal
coloredmesh_to_scimesh <- function(cmesh, style = "default") {
    if (!requireNamespace("scimesh", quietly = TRUE)) {
        stop("The 'scimesh' package is required for the scimesh renderer backend.")
    }

    if (!is.fs.coloredmesh(cmesh)) {
        stop("Parameter 'cmesh' must be an fs.coloredmesh instance.")
    }

    smesh <- scimesh::mesh_from_rgl(cmesh$mesh)

    hex_colors <- cmesh$col
    if (length(hex_colors) == 1L) {
        hex_colors <- rep(hex_colors, nrow(smesh$vertices))
    }
    rgba <- hex_colors_to_rgba_matrix(hex_colors)

    style_params <- get.rglstyle.parameters(cmesh, style)
    rgba[, "A"] <- apply.style.alpha(style_params)

    smesh$colors <- rgba

    return(smesh)
}


#' @title Convert a hemilist of fs.coloredmeshes to a list of scimesh mesh descriptors
#'
#' @param coloredmeshes a named list with entries "lh" and/or "rh", each an
#'   fs.coloredmesh instance.
#'
#' @param style a rendering style (see \code{\link{get.rglstyle}}), passed
#'   through to \code{coloredmesh_to_scimesh}.
#'
#' @return a named list of scimesh mesh descriptors, with the same hemilist
#'   structure. Only meshes with \code{render=TRUE} are included.
#'
#' @keywords internal
coloredmeshes_to_scimesh <- function(coloredmeshes, style = "default") {
    scene <- list()

    # The scimesh backend can only render 'fs.coloredmesh' instances, while the
    # rgl backend can additionally render misc3d 'Triangles3D' iso-surfaces
    # (e.g., as returned by volvis.contour()). Convert such renderables to
    # coloredmeshes first, so both backends accept the same input. See
    # Triangles3D.to.coloredmesh().
    if(is.Triangles3D(coloredmeshes)) {
        coloredmeshes <- Triangles3D.to.coloredmesh(coloredmeshes);
    } else if(is.list(coloredmeshes) && ! is.fs.coloredmesh(coloredmeshes)) {
        is_tris <- vapply(coloredmeshes, is.Triangles3D, logical(1));
        if(any(is_tris)) {
            coloredmeshes[is_tris] <- lapply(coloredmeshes[is_tris], Triangles3D.to.coloredmesh);
        }
    }

    if (is.fs.coloredmesh(coloredmeshes)) {
        if (isTRUE(coloredmeshes$render)) {
            return(list("single" = coloredmesh_to_scimesh(coloredmeshes, style)))
        } else {
            return(list())
        }
    }

    if (is.list(coloredmeshes)) {
        has_lh <- "lh" %in% names(coloredmeshes)
        has_rh <- "rh" %in% names(coloredmeshes)

        if (has_lh || has_rh) {
            if (has_lh && is.fs.coloredmesh(coloredmeshes$lh) &&
                isTRUE(coloredmeshes$lh$render)) {
                scene$lh <- coloredmesh_to_scimesh(coloredmeshes$lh, style)
            }
            if (has_rh && is.fs.coloredmesh(coloredmeshes$rh) &&
                isTRUE(coloredmeshes$rh$render)) {
                scene$rh <- coloredmesh_to_scimesh(coloredmeshes$rh, style)
            }
        } else {
            for (idx in seq_along(coloredmeshes)) {
                cmesh <- coloredmeshes[[idx]]
                if (is.fs.coloredmesh(cmesh) && isTRUE(cmesh$render)) {
                    scene[[length(scene) + 1L]] <- coloredmesh_to_scimesh(cmesh, style)
                }
            }
        }
    }

    return(scene)
}


#' @title Convert an fs.coloredpaths instance to scimesh line layers
#'
#' @description Line segments have no mesh representation, so they cannot be
#'   passed to the scimesh renderer as meshes. They are converted to scimesh
#'   line layers instead (see \code{scimesh::line_layer}), which the scimesh
#'   rasterizer draws directly, without creating any geometry. This is the
#'   cheap way to draw many thin lines, like the edges of a connectome.
#'
#' @param cpaths an fs.coloredpaths instance.
#'
#' @param style a rendering style, see \code{\link{get.rglstyle}}.
#'
#' @return a list of scimesh line layers (class 'scimesh_lines'). One layer per
#'   distinct line width, because the width is a property of the layer.
#'
#' @keywords internal
coloredpaths_to_scimesh <- function(cpaths, style = "default") {
    if (!requireNamespace("scimesh", quietly = TRUE)) {
        stop("The 'scimesh' package is required for the scimesh renderer backend.")
    }
    if (!is.fs.coloredpaths(cpaths)) {
        stop("Parameter 'cpaths' must be an fs.coloredpaths instance.")
    }
    if (!isTRUE(cpaths$render) || nrow(cpaths$from) < 1L) {
        return(list())
    }

    rgba <- hex_colors_to_rgba_matrix(cpaths$col)
    style_params <- get.rglstyle.parameters(cpaths, style)
    rgba[, "A"] <- apply.style.alpha(style_params)

    layers <- list()
    for (line_width in unique(cpaths$width)) {
        sel <- which(cpaths$width == line_width)
        layers[[length(layers) + 1L]] <- scimesh::line_layer(
            from = cpaths$from[sel, , drop = FALSE],
            to = cpaths$to[sel, , drop = FALSE],
            colors = rgba[sel, , drop = FALSE],
            width = line_width,
            depth_test = isTRUE(cpaths$depth_test),
            lit = isTRUE(cpaths$lit)
        )
    }

    return(layers)
}


#' @title Collect the scimesh line layers of all fs.coloredpaths instances in a renderable list
#'
#' @description Walks a renderable list (a flat list of renderables, a hemilist,
#'   or a single renderable) and converts everything that is an
#'   fs.coloredpaths instance to scimesh line layers. Non-line renderables are
#'   ignored, they are handled by \code{\link{coloredmeshes_to_scimesh}}.
#'
#' @param renderables a renderable, or a (possibly nested) list of renderables.
#'
#' @param style a rendering style, see \code{\link{get.rglstyle}}.
#'
#' @return a list of scimesh line layers, possibly empty.
#'
#' @keywords internal
renderables_to_line_layers <- function(renderables, style = "default") {
    layers <- list()
    bbox <- NULL

    collect <- function(x) {
        if (is.fs.coloredpaths(x)) {
            layers <<- c(layers, coloredpaths_to_scimesh(x, style))
            bbox <<- combine_bboxes(bbox, segment_bbox(x$from, x$to))
        } else if (is.list(x) && !inherits(x, "mesh3d") && !is.fs.coloredmesh(x)) {
            for (entry in x) {
                collect(entry)
            }
        }
        invisible(NULL)
    }

    collect(renderables)
    # The bounding box of all segments is needed to place the camera in scenes
    # which contain line renderables but no mesh, see
    # view_angle_to_scimesh_camera().
    attr(layers, "bbox") <- bbox
    return(layers);
}


#' @title Compute the bounding box of line segments.
#'
#' @param from matrix of segment start points.
#'
#' @param to matrix of segment end points.
#'
#' @return numeric vector of length 6: \code{c(xmin, xmax, ymin, ymax, zmin, zmax)}.
#'
#' @keywords internal
segment_bbox <- function(from, to) {
    points <- rbind(from, to)
    return(c(min(points[, 1L]), max(points[, 1L]),
             min(points[, 2L]), max(points[, 2L]),
             min(points[, 3L]), max(points[, 3L])))
}


#' @title Combine two bounding boxes.
#'
#' @param bbox1 numeric vector of length 6 or NULL, see
#'   \code{\link{segment_bbox}}.
#'
#' @param bbox2 numeric vector of length 6.
#'
#' @return numeric vector of length 6, the box that contains both input boxes.
#'
#' @keywords internal
combine_bboxes <- function(bbox1, bbox2) {
    if (is.null(bbox1)) {
        return(bbox2)
    }
    return(c(min(bbox1[1L], bbox2[1L]), max(bbox1[2L], bbox2[2L]),
             min(bbox1[3L], bbox2[3L]), max(bbox1[4L], bbox2[4L]),
             min(bbox1[5L], bbox2[5L]), max(bbox1[6L], bbox2[6L])))
}


#' @title Filter a scimesh scene to the meshes visible from a given view
#'
#' @param scene a named list of scimesh mesh descriptors (with "lh" and/or "rh" entries).
#' @param hemi_filter character string, one of "lh", "rh", or "both".
#'
#' @return a flat list of scimesh mesh descriptors for the given view.
#'
#' @keywords internal
filter_scene_by_view <- function(scene, hemi_filter) {
    if (!is.list(scene) || length(scene) == 0L) {
        return(list())
    }

    if (!("lh" %in% names(scene) || "rh" %in% names(scene))) {
        return(scene)
    }

    if (hemi_filter == "both") {
        result <- list()
        if ("lh" %in% names(scene)) result[[length(result) + 1L]] <- scene$lh
        if ("rh" %in% names(scene)) result[[length(result) + 1L]] <- scene$rh
        return(result)
    } else if (hemi_filter == "lh") {
        if ("lh" %in% names(scene)) {
            return(list(scene$lh))
        }
    } else if (hemi_filter == "rh") {
        if ("rh" %in% names(scene)) {
            return(list(scene$rh))
        }
    }

    return(list())
}


#' @title Get the hemisphere filter for a view angle
#'
#' @param view_angle character string, a valid view angle (with or without the
#'   'sd_' prefix).
#'
#' @return character string, one of "lh", "rh", or "both".
#'
#' @keywords internal
view.angle.to.hemi.filter <- function(view_angle) {
    if (startsWith(view_angle, "sd_")) {
        view_angle <- substring(view_angle, 4L)
    }
    return(switch(view_angle,
        "lateral_lh" = "lh",
        "medial_lh"  = "lh",
        "lateral_rh" = "rh",
        "medial_rh"  = "rh",
        "dorsal"     = "both",
        "ventral"    = "both",
        "rostral"    = "both",
        "caudal"     = "both",
        stop(sprintf("Invalid view_angle '%s'.", view_angle))))
}


#' @title Map an fsbrain view angle to a scimesh camera
#'
#' @description Translates an fsbrain view_angle string (e.g., "lateral_lh",
#' "dorsal", etc.) to a scimesh camera specification. The returned object
#' includes the camera list and a hemi_filter field indicating which hemispheres
#' to render for this view.
#'
#' @param scene a named list of scimesh mesh descriptors with "lh" and/or "rh"
#'   entries, as returned by \code{coloredmeshes_to_scimesh}.
#' @param view_angle character string, a valid view angle. See
#'   \code{\link{get.view.angle.names}} for all valid options.
#' @param fallback_bbox numeric vector of length 6 or NULL, the bounding box to
#'   compute the camera from if the scene contains no mesh, see
#'   \code{\link{segment_bbox}}. This is needed for scenes that contain only line
#'   renderables, e.g. tracts without a context surface.
#'
#' @return a list with entries: \code{camera} (scimesh camera list from
#'   \code{camera_auto}), and \code{hemi_filter} (one of "lh", "rh", or "both").
#'
#' @keywords internal
view_angle_to_scimesh_camera <- function(scene, view_angle, fallback_bbox = NULL) {
    if (!requireNamespace("scimesh", quietly = TRUE)) {
        stop("The 'scimesh' package is required for the scimesh renderer backend.")
    }

    if (startsWith(view_angle, "sd_")) {
        view_angle <- substring(view_angle, 4L)
    }

    all_meshes <- filter_scene_by_view(scene, "both")
    if (length(all_meshes) == 0L && is.null(fallback_bbox)) {
        stop("No meshes in scene to compute camera position.")
    }

    valid_views <- c("lateral_lh", "dorsal", "lateral_rh", "medial_lh",
                     "ventral", "medial_rh", "rostral", "caudal")

    if (!view_angle %in% valid_views) {
        stop(sprintf("Invalid view_angle '%s'. Must be one of: %s",
                     view_angle, paste(valid_views, collapse = ", ")))
    }

    view_config <- switch(view_angle,
        "lateral_lh" = list(direction = c(-1, 0, 0), up = c(0, 0, 1),
                            hemi_filter = "lh"),
        "medial_lh"  = list(direction = c(1, 0, 0),  up = c(0, 0, 1),
                            hemi_filter = "lh"),
        "lateral_rh" = list(direction = c(1, 0, 0),  up = c(0, 0, 1),
                            hemi_filter = "rh"),
        "medial_rh"  = list(direction = c(-1, 0, 0), up = c(0, 0, 1),
                            hemi_filter = "rh"),
        "dorsal"     = list(direction = c(0, 0, 1),  up = c(0, 1, 0),
                            hemi_filter = "both"),
        "ventral"    = list(direction = c(0, 0, -1), up = c(0, -1, 0),
                            hemi_filter = "both"),
        "rostral"    = list(direction = c(0, 1, 0),  up = c(0, 0, 1),
                            hemi_filter = "both"),
        "caudal"     = list(direction = c(0, -1, 0), up = c(0, 0, 1),
                            hemi_filter = "both")
    )

    hemi_meshes <- filter_scene_by_view(scene, view_config$hemi_filter)
    if (length(hemi_meshes) == 0L) {
        hemi_meshes <- all_meshes
    }

    # Frame the view with the same bounding-sphere convention rgl uses for its
    # orthographic auto-fit (radius = half the AABB diagonal, no extra margin):
    # scimesh's orthographic frustum half-height equals |eye - center|, so
    # dist = sphere_radius yields a framing identical to rgl (see
    # TODO_FSBRAIN_RGL_CAM.md, Step 2).
    if (length(all_meshes) == 0L) {
        # A scene with line renderables but without any mesh: use the bounding box
        # of the lines instead of the mesh geometry.
        bs <- bounding_sphere(rbind(c(fallback_bbox[1L], fallback_bbox[3L], fallback_bbox[5L]),
                                    c(fallback_bbox[2L], fallback_bbox[4L], fallback_bbox[6L])))
    } else {
        bs <- bounding_sphere(hemi_meshes)
    }
    bbox_center <- bs$center

    dir <- view_config$direction / sqrt(sum(view_config$direction^2))
    dist <- bs$radius
    eye <- bbox_center + dir * dist

    cam <- scimesh::camera(
        eye = eye,
        center = bbox_center,
        up = view_config$up,
        projection = "orthographic"
    )

    return(list(camera = cam, hemi_filter = view_config$hemi_filter))
}


#' @title Map an fsbrain rendering style to scimesh render options
#'
#' @description Translates fsbrain style names ("default", "shiny",
#' "semitransparent", "glass", "edges") to scimesh \code{render_options()}
#' parameters.
#'
#' @param style character string, an fsbrain style name. See
#'   \code{\link{get.rglstyle}} for valid options.
#' @param bg_rgba numeric vector of length 4, the background color in RGBA
#'   (0-1 scale).
#' @param width integer, output image width in pixels. Defaults to 800.
#' @param height integer, output image height in pixels. Defaults to 600.
#'
#' @return a scimesh render options list from \code{render_options()}.
#'
#' @keywords internal
fsbrain_style_to_scimesh_options <- function(style = "default",
                                             bg_rgba = c(1, 1, 1, 1),
                                             width = 800L,
                                             height = 600L) {
    if (!requireNamespace("scimesh", quietly = TRUE)) {
        stop("The 'scimesh' package is required for the scimesh renderer backend.")
    }

    rgl_params <- get.rglstyle.parameters(list(), style)

    shading <- "smooth"
    backface_culling <- TRUE
    specular_color <- c(0, 0, 0, 0)
    shininess <- 0
    wireframe <- FALSE
    invert_normals <- FALSE

    if (!is.null(rgl_params$front)) {
        if (rgl_params$front == "lines" && rgl_params$back == "lines") {
            wireframe <- TRUE
        }
    }
    if (!is.null(rgl_params$alpha)) {
        shading <- "smooth"
    }
    if (isTRUE(rgl_params$back == "culled")) {
        backface_culling <- TRUE
    }
    if (!is.null(rgl_params$specular) && is.character(rgl_params$specular)) {
        spec_rgb <- grDevices::col2rgb(rgl_params$specular) / 255.0
        specular_color <- c(spec_rgb[1], spec_rgb[2], spec_rgb[3], 1)
    }
    if (!is.null(rgl_params$shininess)) {
        shininess <- as.numeric(rgl_params$shininess)
    }

    scimesh_opts <- list(
        width = as.integer(width),
        height = as.integer(height),
        shading = shading,
        backface_culling = backface_culling,
        background_color = bg_rgba,
        invert_normals = invert_normals,
        wireframe = wireframe,
        projection = "orthographic",
        specular_color = specular_color,
        shininess = shininess,
        # Anti-aliasing: scimesh defaults to no AA, which shows on thin lines,
        # so fsbrain asks for supersampling. See
        # get.fsbrain.scimesh.aa.samples() for how the factor is determined.
        aa_samples = get.fsbrain.scimesh.aa.samples()
    );

    return(do.call(scimesh::render_options, scimesh_opts));
}


#' @title Convert highlight points (rglactions) to scimesh sphere meshes
#'
#' @param rglactions named list; the entry 'highlight_points' is used if present.
#' @param hemi_filter character string, one of "lh", "rh", or "both".
#'
#' @return list of scimesh mesh descriptors (spheres), possibly empty.
#'
#' @keywords internal
highlight_points_to_scimesh <- function(rglactions, hemi_filter = "both") {
    if (!rglactions.has.key(rglactions, "highlight_points")) {
        return(list())
    }
    hp <- rglactions$highlight_points
    coords <- hp$coords
    color <- hp$color
    radius <- if (is.null(hp$radius)) 1.0 else hp$radius
    if (is.null(color)) {
        color <- "#FF0000"
    }
    if (is.vector(coords)) {
        coords <- matrix(coords, ncol = 3L, byrow = TRUE)
    }
    color <- recycle(color, nrow(coords))
    if (hemi_filter != "both" && !is.null(hp$hemi)) {
        idx <- which(hp$hemi == hemi_filter)
        coords <- coords[idx, , drop = FALSE]
        color <- color[idx]
    }
    if (nrow(coords) == 0L) {
        return(list())
    }
    spheres <- list()
    for (i in seq_len(nrow(coords))) {
        rgba <- color_to_rgba(color[[i]])
        spheres[[length(spheres) + 1L]] <- scimesh::generate_sphere(
            center = coords[i, ], radius = radius, color = rgba)
    }
    return(spheres)
}


#' @title Get the output image dimensions for the scimesh backend
#'
#' @return integer vector of length 2 (width, height), read from the global
#'   option 'fsbrain.scimesh.output_dims'. Defaults to 1920x1080.
#'
#' @keywords internal
get.fsbrain.scimesh.output.dims <- function() {
    dims <- getOption("fsbrain.scimesh.output_dims", default = c(1920L, 1080L))
    if (!is.numeric(dims) || length(dims) != 2L) {
        stop("Option 'fsbrain.scimesh.output_dims' must be a numeric vector of length 2 (width, height).")
    }
    return(as.integer(dims))
}


#' @title The default anti-aliasing factor of the scimesh backend
#'
#' @description The anti-aliasing factor used for scimesh renders when neither
#'   the fsbrain option 'fsbrain.scimesh.aa_samples' nor the scimesh-wide option
#'   'scimesh.aa_samples' is set. scimesh renders without anti-aliasing by
#'   default, which is most visible on thin lines (they show a staircase
#'   pattern, unlike the hardware-drawn lines of the rgl backend), so fsbrain
#'   requests 2x2 supersampling. Set 'fsbrain.scimesh.aa_samples' to 1 to turn
#'   anti-aliasing off, or to 4 for higher quality.
#'
#' @keywords internal
FSBRAIN_SCIMESH_DEFAULT_AA <- 2L


#' @title Get the anti-aliasing factor for the scimesh backend
#'
#' @description Determines the anti-aliasing (supersampling) factor that
#'   fsbrain passes to scimesh. The value is taken from the global option
#'   'fsbrain.scimesh.aa_samples'; when that option is unset, an explicitly set
#'   scimesh-wide option 'scimesh.aa_samples' is used instead, so that a
#'   session-wide scimesh setting is honored. If neither is set, fsbrain uses
#'   \code{FSBRAIN_SCIMESH_DEFAULT_AA} (2, i.e. 2x2 supersampling).
#'
#' @details The order of precedence is:
#'   \code{fsbrain.scimesh.aa_samples} > \code{scimesh.aa_samples} >
#'   \code{2} (the fsbrain default). The option is read for every render call,
#'   so it can be changed at any time with \code{options()}.
#'
#' @return single positive integer.
#'
#' @examples
#' \dontrun{
#'   # Higher quality (4x4 supersampling) for all scimesh renders:
#'   options(fsbrain.scimesh.aa_samples = 4);
#'
#'   # Back to the fsbrain default (2x2), ignoring a scimesh-wide setting:
#'   options(fsbrain.scimesh.aa_samples = 2);
#'
#'   # No anti-aliasing, for fast drafts:
#'   options(fsbrain.scimesh.aa_samples = 1);
#' }
#'
#' @keywords internal
get.fsbrain.scimesh.aa.samples <- function() {
    aa_samples <- getOption("fsbrain.scimesh.aa_samples", default = NULL);
    if (is.null(aa_samples)) {
        # No fsbrain-specific setting: honor an explicit scimesh-wide setting,
        # otherwise fall back to the fsbrain default.
        aa_samples <- getOption("scimesh.aa_samples", default = NULL);
        if (is.null(aa_samples)) {
            aa_samples <- FSBRAIN_SCIMESH_DEFAULT_AA;
        }
    }
    if (!is.numeric(aa_samples) || length(aa_samples) != 1L ||
        is.na(aa_samples) || !is.finite(aa_samples) || aa_samples < 1 ||
        abs(aa_samples - round(aa_samples)) > 1e-8) {
        stop("Option 'fsbrain.scimesh.aa_samples' (or 'scimesh.aa_samples') must be a single positive integer, e.g. 1 (no anti-aliasing), 2 or 4.");
    }
    return(as.integer(round(aa_samples)));
}


#' @title Get the current fsbrain renderer backend
#'
#' @description Returns the current fsbrain renderer backend setting. When set
#' to \code{"scimesh"}, fsbrain will use the scimesh software renderer instead
#' of rgl/OpenGL for image export. The default is \code{"rgl"}. Set it with
#' \code{options(fsbrain.renderer_backend = "scimesh")} at the start of your
#' R session.
#'
#' @return character string, either "rgl" or "scimesh".
#'
#' @note Only functions that produce static PNG output are affected. Interactive
#'   viewers, animations, and the rglwidget (WebGL) always use rgl.
#'
#' @examples
#' \dontrun{
#'   # Switch to the scimesh software renderer for headless environments
#'   options(fsbrain.renderer_backend = "scimesh")
#'
#'   # Check current backend
#'   get.fsbrain.renderer.backend()
#'
#'   # Switch back to rgl
#'   options(fsbrain.renderer_backend = "rgl")
#' }
#'
#' @export
get.fsbrain.renderer.backend <- function() {
    getOption("fsbrain.renderer_backend", default = "rgl")
}
