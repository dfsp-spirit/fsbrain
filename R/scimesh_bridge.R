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
#'
#' @return a list with entries: \code{camera} (scimesh camera list from
#'   \code{camera_auto}), and \code{hemi_filter} (one of "lh", "rh", or "both").
#'
#' @keywords internal
view_angle_to_scimesh_camera <- function(scene, view_angle) {
    if (!requireNamespace("scimesh", quietly = TRUE)) {
        stop("The 'scimesh' package is required for the scimesh renderer backend.")
    }

    if (startsWith(view_angle, "sd_")) {
        view_angle <- substring(view_angle, 4L)
    }

    all_meshes <- filter_scene_by_view(scene, "both")
    if (length(all_meshes) == 0L) {
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

    all_verts <- do.call(rbind, lapply(hemi_meshes, function(m) m$vertices))
    bbox_center <- colMeans(apply(all_verts, 2L, range))
    bbox_extent <- max(apply(all_verts, 2L, function(col) diff(range(col)))) / 2.0

    dir <- view_config$direction / sqrt(sum(view_config$direction^2))
    dist <- bbox_extent * 1.35
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

    scimesh::render_options(
        width = as.integer(width),
        height = as.integer(height),
        shading = shading,
        backface_culling = backface_culling,
        background_color = bg_rgba,
        invert_normals = invert_normals,
        wireframe = wireframe,
        projection = "orthographic",
        specular_color = specular_color,
        shininess = shininess
    )
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
    if (hemi_filter != "both" && !is.null(hp$hemi)) {
        idx <- which(hp$hemi == hemi_filter)
        coords <- coords[idx, , drop = FALSE]
        color <- color[idx]
    }
    if (nrow(coords) == 0L) {
        return(list())
    }
    color <- recycle(color, nrow(coords))
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
