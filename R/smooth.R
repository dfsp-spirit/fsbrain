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
#'  th = subject.morph.native(sjd, "fsaverage3", "thickness", hemi="lh");
#'  th_smooth = pervertexdata.smoothnn(surface, th, fwhm = 10.0);
#'  vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = th);
#'  vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = th_smooth);
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

    message("Something is wrong with this, FS seems to use a different computation for the mesh area. Maybe the issue is specific to group template surfaces like fsaverage (because of group scaling).");
    surface = ensure.fs.surface(surface);
    #mesh = fsbrain:::ensure.tmesh3d(surface);
    #total_area = Rvcg::vcgArea(mesh);
    total_area = surf.metric.properties(surface)$mesh_total_area;
    avgvtxarea = total_area / nrow(surface$vertices);
    gstd = fwhm / sqrt(log(256.0));
    niters = floor((1.14 * (4 * pi * (gstd * gstd)) / (7 * avgvtxarea)) + 0.5);
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
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
#' fsbrain:::pervertexdata.smoothnn.compute.fwhm(spherical_surface, 5L);
#' }
#'
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
#' @inheritParams surf.sphere.gaussianweights
#'
#' @param data numerical vector of per-vertex data for the surface
#'
#' @param fwhm double, the full width at half maximum for the Gaussian kernel
#'
#' @param truncfactor the factor after how many stddevs to truncate the Gaussian kernel
#'
#' @note This function has been adapted from FreeSurfer and it is subject to the FreeSurfer software license.
#'
#' @return the smoothed data
#'
#' @examples
#' \dontrun{
#' sjd = fsaverage.path();
#' spherical_surface = subject.surface(sjd, "fsaverage3", surface="sphere", hemi="lh");
#' vdata = subject.morph.native(fsaverage.path(), "fsaverage3", "thickness", hemi="lh");
#' vdata_smoothed = pervertexdata.smoothgaussian(spherical_surface, vdata, fwhm = 15);
#' vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = vdata);
#' vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = vdata_smoothed);
#' }
#'
#' @export
pervertexdata.smoothgaussian <- function(spherical_surface, data, fwhm = 15.0, truncfactor = 3.5) {
    gstd = fwhm / sqrt(log(256.0));
    maxdist = truncfactor * gstd;
    sphere_dists = surf.sphere.dist(spherical_surface, maxdist = maxdist);

    gaussian_weights = fsbrain:::surf.sphere.gaussianweights(spherical_surface, sphere_dists, gstd);
    smoothed_data = surf.sphere.spatialfilter(data, sphere_dists, gaussian_weights);
    return(smoothed_data);
}


#' @title Compute average distance from the origin to each vertex.
#'
#' @param surface an fs.surface instance, and for the typical use case of this function, a spherical surface.
#'
#' @return scalar double, the average distance.
#'
#' @note This is used to determine the sphere radius for spherical surfaces. It is assumed that the sphere is centered at the origin \code{(0,0,0)}.
#'
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
#' vr = fsbrain:::surf.avg.vertexradius(spherical_surface);
#' # Show histogram to verify that the surface is a sphere centered at 0,0,0:
#' hist(freesurferformats:::vertexdists.to.point(spherical_surface, c(0,0,0)));
#' # Plot the coords and a point at the origin:
#' fsbrain::highlight.points.spheres(rbind(spherical_surface$vertices, c(0,0,0)));
#' }
#'
#' @keywords internal
surf.avg.vertexradius <- function(surface, with_stddev=FALSE) {
    surface = fsbrain:::ensure.fs.surface(surface);
    nv = nrow(surface$vertices);

    dists = freesurferformats:::vertexdists.to.point(surface, c(0,0,0));
    avg = sum(dists) / nv;
    if(with_stddev) {
        ds = sum(dists * dists);
        stddev = sqrt(nv * (ds / nv - avg * avg) / (nv - 1));
        return(list("avg"=avg, "stddev"=stddev));
    }
    return(avg);
}


setRefClass("IntVecReference",
            fields=list(
                vec="integer"
            )
);
setRefClass("DoubleVecReference",
            fields=list(
                vec="numeric"
            )
);


#' @title Compute vertex neighborhoods on a sphere based on the given max distance along the sphere.
#'
#' @param spherical_surface an fs.surface instance representing the spherical version (\code{lh.sphere} or \code{rh.sphere} of the subject).
#'
#' @param maxdist double, the neighborhood size along the sphere, or to be more precise the maximal distance to travel along the sphere (using mesh edges) when searching for neighbors. The maxdist value can be computed from the definition of the Gaussian kernel parameters, i.e., its FWHM and truncation factor. See \code{pervertexdata.smoothgaussian} for an example of how to do that. For a kernel with FWHM of 5 and a truncation factor of 3.5, the resulting maxdist setting is 20.
#'
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
#' sphere_dist = surf.sphere.dist(spherical_surface, 20.0);
#' highlight.vertices.on.subject(fsaverage.path(), "fsaverage3", verts_lh = sphere_dist$neigh[[500]], surface="sphere");
#' }
#'
#' @return named list with 3 entries. Each is a vector with neighborhood information: 'neigh' is an int vector of the neighbor vertices, 'neigh_dist_dotproduct' a numerical vector of dp distances for these neighbors, and 'neigh_dist_surface' the same for along-the-surface-distances instead of dp distances.
#'
#' @export
surf.sphere.dist <- function(spherical_surface, maxdist) {


    spherical_surface = fsbrain:::ensure.fs.surface(spherical_surface);
    nv = nrow(spherical_surface$vertices);

    radius = fsbrain:::surf.avg.vertexradius(spherical_surface);
    radius2 = radius * radius;
    min_dotp_thresh = radius2 * cos(maxdist / radius) * (1.0001);

    neigh = list();
    neigh_dist_dotproduct = list();
    neigh_dist_surface = list();


    ## The following implementation only considers the dotproduct distance, but it does not follow the
    ## mesh edges. The FreeSurfer implementation follows the k-ring neighborhoods for increasing k and
    ## adds vertices while they are still under the dotproduct threshold.
    ## See utils/mrisurf_metricProperties.cpp l 12747, MRISextendedNeighbors
    min_global_costheta = 1e6;
    max_global_costheta = -1e6;

    for(vidx in seq(nv)) {

        ref_visited <- new("IntVecReference", vec=rep(0L, nv));
        ref_neighbors <- new("IntVecReference", vec=rep(0L, nv));
        ref_neighbor_dpdists <- new("DoubleVecReference", vec=rep(-1.0, nv));
        fsbrain:::extend_neighbors(spherical_surface, vidx, vidx, min_dotp_thresh, ref_visited, ref_neighbors, ref_neighbor_dpdists);

        neigh[[vidx]] = which(ref_neighbors$vec == 1L);
        neigh_dist_dotproduct[[vidx]] = ref_neighbor_dpdists$vec[neigh[[vidx]]];

        # We do this in a vectorized R fashion instead of looping over the neighbors C++-style.
        costheta = (neigh_dist_dotproduct[[vidx]] / radius2);
        range_costheta = range(costheta);
        if(range_costheta[1] < min_global_costheta) {
            min_global_costheta = range_costheta[1];
        }
        if(range_costheta[2] > max_global_costheta) {
            max_global_costheta = range_costheta[2];
        }
        #cat(sprintf("Costheta range is %f to %f.\n", range_costheta[1], range_costheta[2]));

        costheta[costheta > 1.0] = 1.0;
        costheta[costheta < -1.0] = -1.0;
        theta = acos(costheta);
        neigh_dist_surface[[vidx]] = radius * theta;
    }
    # Costheta mst be in range -1.0 to +1.0.
    #cat(sprintf("Global costheta range is %f to %f (expected -1.0 to 1.0).\n", min_global_costheta, max_global_costheta));
    return(list('neigh'=neigh, 'neigh_dist_dotproduct' = neigh_dist_dotproduct, 'neigh_dist_surface' = neigh_dist_surface));
}


#' @title Recursive computation of neighborhoods
#'
#' @keywords internal
extend_neighbors <- function(spherical_surface, targetvidx, currentvidx, min_dotp_thresh, ref_visited, ref_neighbors, ref_neighbor_dpdists) {

    do_checks = FALSE;
    if(do_checks) {
        if(length(targetvidx) != 1L) {
            stop(sprintf("Parameter 'targetvidx' must be a scalar integer but has length %d.\n", length(targetvidx)));
        }
        if(! is.integer(targetvidx)) {
            stop("Parameter 'targetvidx' must be a scalar integer, but is not an integer.");
        }
        if(length(currentvidx) != 1L) {
            stop(sprintf("Parameter 'currentvidx' must be a scalar integer but has length %d.\n", length(currentvidx)));
        }
        if(! is.integer(currentvidx)) {
            stop("Parameter 'currentvidx' must be a scalar integer, but is not an integer.");
        }
    }

    num_verts = nrow(spherical_surface$vertices);
    #cat(sprintf("Running for targetvidx=%d, currentvidx=%d, found %d neighbors so far (got %d dists for them).\n", targetvidx, currentvidx, length(which(ref_neighbors$vec == 1L)), length(which(ref_neighbor_dpdists$vec >= 0))));
    #cat(sprintf("* Surface with %d vertices, length(ref_visited$vec)=%d.\n", num_verts, length(ref_visited$vec)));

    if(ref_visited$vec[currentvidx] == targetvidx) {
        return(invisible(0L)); # Abort recursion.
    }
    dotprod = abs(sum(spherical_surface$vertices[targetvidx,] * spherical_surface$vertices[currentvidx,]));
    if(dotprod < min_dotp_thresh) {
        return(invisible(0L)); # Abort recursion.
    }

    # Add current vertex to neighborhood with distance.
    ref_neighbors$vec[currentvidx] = 1L;
    ref_neighbor_dpdists$vec[currentvidx] = dotprod;

    if(length(which(ref_neighbors$vec == 1L)) == num_verts) {
        return(invisible(1L));
    }

    ref_visited$vec[currentvidx] = targetvidx; # Record hit.

    # Iterate over structural neighbors
    mesh_neighbors = fs.surface.vertex.neighbors(spherical_surface, currentvidx);
    #cat(sprintf("Vertex currentvidx=%d has %d neighbors.\n", currentvidx, length(mesh_neighbors)));
    for(mesh_neigh_idx in mesh_neighbors) {
        res = extend_neighbors(spherical_surface, targetvidx, mesh_neigh_idx, min_dotp_thresh, ref_visited, ref_neighbors, ref_neighbor_dpdists);
        if(res == 1L) {
            return(res);
        }
    }
    return(invisible(0L));
}

#' @title Compute Gaussian weights
#'
#' @inheritParams surf.sphere.dist
#'
#' @param sphere_dists list of vectors, as returned by surf.sphere.dist
#'
#' @param gstd double, Gaussian standard deviation, can be computed from the FWHM as \code{gstd = fwhm / sqrt(log(256.0))}.
#'
#' @examples
#' \dontrun{
#' fwhm = 5.0; truncfactor = 3.5;
#' gstd = fwhm / sqrt(log(256.0)); maxdist = truncfactor * gstd;
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
#' sphere_dists = surf.sphere.dist(spherical_surface, maxdist = maxdist);
#' gaussian_weights = fsbrain:::surf.sphere.gaussianweights(spherical_surface, sphere_dists, gstd);
#' }
#'
#' @return vector of Gaussian weights for vertices
#'  see int MRISgaussianWeights(MRIS *surf, MRI *dist, double GStd) in util/mrisurf_mri.cpp
surf.sphere.gaussianweights <- function(spherical_surface, sphere_dists, gstd) {

    gvar2 = 2 * (gstd * gstd); # twice the variance
    f = 1.0 / (sqrt(2 * pi) * gstd);

    num_neighbors = unlist(lapply(sphere_dists$neigh, length));
    nv = nrow(spherical_surface$vertices);
    if(length(num_neighbors) != nv) {
        stop("Data from parameter 'sphere_dists' does not match 'spherical_surface' vertex count");
    }
    weights = list();
    for(vidx in seq(nv)) {
        gsum = 0.0;
        vert_weights = rep(NA, num_neighbors[vidx]);
        local_idx = 1L;

        for(neigh_vidx in sphere_dists$neigh[[vidx]]) {
            d = sphere_dists$neigh_dist_surface[[vidx]][local_idx];
            g = f * exp(-(d * d) / (gvar2));
            vert_weights[local_idx] = g;
            gsum = gsum + g;
            local_idx = local_idx + 1L;
        }
        vert_weights = vert_weights / gsum; # Normalize
        weights[[vidx]] = vert_weights;
        #cat(sprintf("Vertex %d has %d neighbors: %s. weights=%s\n", vidx, num_neighbors[vidx], paste(sphere_dists$neigh[[vidx]], collapse = " "), paste(weights[[vidx]], collapse = " ")));
    }
    return(weights);
}


#' @title Compute metric surface properties.
#'
#' @inheritParams surf.avg.vertexradius
#'
#' @examples
#' \dontrun{
#' surface = subject.surface(fsaverage.path(), "fsaverage3", hemi="lh");
#' mp = surf.metric.properties(surface);
#' }
#'
#' @return named list of metric surface properties.
#'
#' @keywords internal
surf.metric.properties <- function(surface) {
    mp = list();
    mesh = fsbrain:::ensure.tmesh3d(surface);

    mesh = rgl::addNormals(mesh); # Adds per-vertex normals.
    mp$vertex_normals = t(mesh$normals)[,1:3];

    face_normals = t(Rvcg::vcgFaceNormals(mesh));
    mp$face_normals = face_normals;

    is_negative = face_normals[,2] < 0.0;
    face_areas = unlist(Rvcg::vcgArea(mesh, perface = TRUE)$pertriangle);
    mp$face_areas = face_areas;

    mp$mesh_total_area = sum(face_areas[!is_negative]);
    mp$mesh_neg_area = -sum(face_areas[is_negative]);
    return(mp);
}



#' @title Apply spatial filter to surface data.
# see MRISspatialFilter
surf.sphere.spatialfilter <- function(source_data, sphere_dists, gaussian_weights) {
    smoothed_data = rep(NA, length(source_data));
    if(! is.list(sphere_dists$neigh)) {
        stop("Parameter 'sphere_dists' member 'neigh' must be a list.");
    }
    for(vidx in seq_along(source_data)) {
        #num_neigh = length(sphere_dists$neigh[[vidx]]);
        # smoothed_data[vidx] = sum(source_data[sphere_dists$neigh[[vidx]]] * gaussian_weights[sphere_dists$neigh[[vidx]]]) / num_neigh;
        smoothed_data[vidx] = sum(source_data[sphere_dists$neigh[[vidx]]] * gaussian_weights[[vidx]]);
    }
    return(smoothed_data);
}


