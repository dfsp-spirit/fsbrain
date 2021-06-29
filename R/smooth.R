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
#' @note This function has been adapted from FreeSurfer and it is subject to the FreeSurfer software license.
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
#' @note This function has been adapted from FreeSurfer and it is subject to the FreeSurfer software license.
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
#' @note This function has been adapted from FreeSurfer and it is subject to the FreeSurfer software license.
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



#' @title Perform Gaussian smoothing
#'
#' @note This function has been adapted from FreeSurfer and it is subject to the FreeSurfer software license.
#'
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
#' data = subject.morph.native(fsaverage.path(), "fsaverage3", "thickness", hemi="lh");
#' data_smoothed = pervertexdata.smoothgaussian(spherical_surface, data);
#' }
#'
#' @export
pervertexdata.smoothgaussian <- function(spherical_surface, data, maxdist = 5.0, fwhm = 5.0) {
    sphere_dists = surf.sphere.dist(spherical_surface, maxdist = maxdist);
    gstd = fwhm / sqrt(log(256.0));
    gaussian_weights = surf.sphere.gaussianweights(spherical_surface, sphere_dists, gstd);
    smoothed_data = surf.sphere.spatialfilter(data, sphere_dists, gaussian_weights);
    return(smoothed_data);
}


#' @title Compute average distance from the origin to each vertex.
#'
#' @param surface an fs.surface instance, and for the typical use case of this function, a spherical surface.
#'
#' @return scalar double, the average distance.
#'
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
#' vr = fsbrain:::surf.avg.vertexradius(spherical_surface);
#' # Show histogram to verify that the surface is a sphere centered at 0,0,0:
#' hist(freesurferformats:::vertexdists.to.point(spherical_surface, c(0,0,0)));
#' }
#'
#' @keywords internal
surf.avg.vertexradius <- function(surface) {
    surface = fsbrain:::ensure.fs.surface(surface);
    nv = nrow(surface$vertices);
    return(sum(freesurferformats:::vertexdists.to.point(surface, c(0,0,0))) / nv);
}



#' @title Compute all vertices in given distance on a sphere and their distances.
#'
#' @param spherical_surface an fs.surface instance representing the spherical version (\code{lh.sphere} or \code{rh.sphere} of the subject).
#'
#' @param maxdist double, the neighborhood size
#'
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
#' dist = surf.sphere.dist(spherical_surface, 5.0);
#' highlight.vertices.on.subject(fsaverage.path(), "fsaverage3", verts_lh = dist$neigh[[500]], surface="sphere")
#' }
#'
#' @note see MRISgaussianWeights to get Gaussian weights for the vertex neighborhoods
#'
#' @export
surf.sphere.dist <- function(spherical_surface, maxdist = 5.0) {

    warning("This does not work yet");

    spherical_surface = fsbrain:::ensure.fs.surface(spherical_surface);
    nv = nrow(spherical_surface$vertices);

    radius = fsbrain:::surf.avg.vertexradius(spherical_surface);
    radius2 = radius * radius;
    min_dotp_thresh = radius2 * cos(maxdist / radius) * (1.0001);

    neigh = list();
    neigh_dist_dotproduct = list();
    neigh_dist_surface = list();
    for(vidx in seq(nv)) {
        pd_dists = rowSums((spherical_surface$vertices[vidx,] * spherical_surface$vertices));
        neigh[[vidx]] = which(pd_dists < min_dotp_thresh);
        neigh_dist_dotproduct[[vidx]] = pd_dists[neigh[[vidx]]];

        costheta = neigh_dist_dotproduct[[vidx]] / radius2;
        costheta[costheta > 1.0] = 1.0;
        costheta[costheta < -1.0] = -1.0;
        theta = acos(costheta);
        neigh_dist_surface[[vidx]] = radius * theta;
    }
    return(list('neigh'=neigh, 'neigh_dist_dotproduct' = neigh_dist_dotproduct, 'neigh_dist_surface' = neigh_dist_surface));
}


#' @title Compute Gaussian weights
#'
#' @param gstd double, Gaussian standard deviation, can be computed from the FWHM as \code{gstd = fwhm / sqrt(log(256.0))}.
#'
#' @return vector of Gaussian weights for vertices
#'  see int MRISgaussianWeights(MRIS *surf, MRI *dist, double GStd) in util/mrisurf_mri.cpp
surf.sphere.gaussianweights <- function(spherical_surface, sphere_dists, gstd) {

    gvar2 = 2 * (gstd * gstd); # twice the variance
    f = 1.0 / (sqrt(2 * pi) * gstd);

    num_neighbors = unlist(lapply(sphere_dists$neigh, length));
    nv = nrow(spherical_surface$vertices);
    weights = list();
    for(vidx in seq(nv)) {
        gsum = 0.0;
        vert_weights = rep(NA, num_neighbors[vidx]);
        local_idx = 1L;
        #cat(sprintf("Vertex %d has %d neighbors: %s\n", vidx, num_neighbors[vidx], paste(sphere_dists$neigh[[vidx]], collapse = " ")));
        for(neigh_vidx in sphere_dists$neigh[[vidx]]) {
            d = sphere_dists$neigh[[neigh_vidx]];
            g = f * exp(-(d * d) / (gvar2));
            vert_weights[local_idx] = g;
            gsum = gsum + g;
            local_idx = local_idx + 1L;
        }
        vert_weights = vert_weights / gsum; # Normalize
        weights[[vidx]] = vert_weights;
    }
    return(weights);
}



#' @title Apply spatial filter to surface data.
# see MRISspatialFilter
surf.sphere.spatialfilter <- function(source_data, sphere_dists, gaussian_weights) {
    smoothed_data = rep(NA, length(source_data));
    if(! is.list(sphere_dists$neigh)) {
        stop("Parameter 'sphere_dists' member 'neigh' must be a list.");
    }
    for(vidx in seq_along(source_data)) {
        num_neigh = length(sphere_dists$neigh[[vidx]]);
        # smoothed_data[vidx] = sum(source_data[sphere_dists$neigh[[vidx]]] * gaussian_weights[sphere_dists$neigh[[vidx]]]) / num_neigh;
        smoothed_data[vidx] = sum(source_data[sphere_dists$neigh[[vidx]]] * gaussian_weights[[vidx]]) / num_neigh;
    }
    return(smoothed_data);
}


