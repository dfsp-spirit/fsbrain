## Functions for smoothing per-vertex data on meshes.
## (These functions do NOT smooth the mesh topology!)


#' @title Ignore, not part of fsbrain, only here temporarily.
#'
#' @description Approximate Gaussian smoothing of surface data by iterative mesh neighborhood averaging.
#'
#' @param surface an \code{fs.surface} instance
#'
#' @param data numerical vector, the per-vertex data for the surface
#'
#' @param fwhm double the target fwhm value. If given, num_iter must be NULL and will be computed to match this fwhm value.
#'
#' @param num_iter positive scalar integer, the number of times to perform the averaging. Only use this if you know what you are doing, prefer specifying the target fwhm instead. The fwhm parameter must be NULL if this is used instead.
#'
#' @note The iteration is currently done in R, which means the performance is not great.
#'
#'  @examples
#'  \dontrun{
#'  sjd = fsaverage.path(T);
#'  surface = subject.surface(sjd, "fsaverage3", hemi = "lh");
#'  th = subject.morph.native(sjd, "fsaverage3", "thickness", hemi="lh")
#'  th_smooth = pervertexdata.smoothnn(surface, th, fwhm = 10.0);
#'  vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = th)
#'  vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = th_smooth)
#'  }
#'
#' @export
pervertexdata.smoothnn <- function(surface, data, fwhm = NULL, num_iter = NULL) {
    if(! freesurferformats::is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an fs.surface instance.");
    }
    if(nrow(surface$vertices) != length(data)) {
        stop("Number of surface vertices must match data length.");
    }

    if(is.null(num_iter)) {
        if(is.null(fwhm)) {
            stop("One of 'num_iter' or 'fwhm' must be given and non-NULL.");
        }
        num_iter = pervertexdata.smoothnn.compute.numiter(surface, fwhm);
        cat(sprintf("Smoothing to reach FWHM %f, using %d iterations of nearest neighbor smoothing.\n", fwhm, num_iter));
    } else {
        if(!is.null(fwhm)) {
            stop("If 'num_iter' is given, the parameter fwhm will be ignored and must be set to NULL.");
        } else {
            cat(sprintf("Smoothing with %d nn iterations, expecting FWHM of about %f.\n", num_iter, pervertexdata.smoothnn.compute.fwhm(surface, num_iter)));
        }
    }

    surface = ensure.fs.surface(surface);
    adj = fs.surface.as.adjacencylist(surface); # Slow, should replace in Rvcg with a yet-to-write function.
    nv = nrow(surface$vertices);
    if(length(data) != nv) {
        stop("Data and vertex count mismatch");
    }

    data_smoothed = rep(NA, nv);
    for(iteration in seq(num_iter)) {
        if(iteration == 1L) {
            source_data = data;
        } else {
            source_data = data_smoothed;
        }
        for(vidx in seq(nv)) {
            neigh = c(adj[[vidx]], vidx);
            data_smoothed[vidx] = mean(source_data[neigh]);
        }
    }
    return(data_smoothed);
}


#' @title Compute number of neighborhood smoothing iterations to reach requested fwhm.
#'
#' @inheritParams pervertexdata.smoothnn
#'
#' @return integer, the iteration count
#'
#' @keywords internal
pervertexdata.smoothnn.compute.numiter<- function(surface, fwhm=5.0) {

    message("Something is wrong with this, FS seems to use a different computation for the mesh area.");

    if(! freesurferformats::is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an fs.surface instance.");
    }
    mesh = fsbrain:::ensure.tmesh3d(surface);
    avgvtxarea = Rvcg::vcgArea(mesh) / nrow(surface$vertices);
    gstd = fwhm / sqrt(log(256.0));
    niters = floor(1.14 * (4 * pi * (gstd * gstd)) / (7 * avgvtxarea) + 0.5);
    return(as.integer(niters));
}


#' @title Compute expected FWHM from given number of neighborhood smoothing iterations.
#'
#' @inheritParams pervertexdata.smoothnn
#'
#' @param niters positive integer, the nn iteration count
#'
#' @return double, the expected FWHM
#'
#' @keywords internal
pervertexdata.smoothnn.compute.fwhm <- function(surface, niters) {

    message("Something is wrong with this, FS seems to use a different computation for the mesh area.");

    if(! freesurferformats::is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an fs.surface instance.");
    }
    mesh = fsbrain:::ensure.tmesh3d(surface);
    avgvtxarea = Rvcg::vcgArea(mesh) / nrow(surface$vertices);
    gstd = sqrt(7 * avgvtxarea * niters / (1.14 * 4 * pi));
    fwhm = gstd * sqrt(log(256.0));
    return(fwhm);
}






