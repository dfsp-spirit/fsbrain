## Functions for smoothing per-vertex data on meshes.
## (These functions do NOT smooth the mesh topology!)


#' @title Perform iterative nearest-neighbor smoothing of per-vertex data.
#'
#' @description Smoothing of surface data by iterative mesh neighborhood averaging. Assigns to each vertex the average of the values in its 1-ring neighborhood (based on the mesh edges).
#'
#' @param surface an \code{fs.surface} instance
#'
#' @param data numerical vector, the per-vertex data for the surface. Values set to \code{NA} will be ignore, so you can apply a mask before this operation (e.g., by setting the values for all vertices of the medial mask to \code{NA}).
#'
#' @param num_iter positive scalar integer, the number of times to perform the averaging. Depends on the mesh resolution and desired smoothing, higher resolution meshes will need more passes. Typical values are in range 20 to 150, but also depend on the setting of parameter 'k', of course.
#'
#' @param k positive scalar integer, the neighborhood size in hops along the mesh edges (the k for the k-ring neighborhood). For higher resolution meshes it makes sense to increase this, typical values are roughly in range 1 to 10.
#'
#' @note The iteration is currently done in R, which means the performance is not great.
#' @note This does NOT smooth the mesh, it smoothes per-vertex values assigned to mesh vertices.
#' @note To see relevant smoothing for full-resolution FreeSurfer meshes, try setting \code{num_iter=50, k=8} for a start.
#'
#' @examples
#' \dontrun{
#' sjd = fsaverage.path(T);
#' sj = "fsaverage3";
#' surface = subject.surface(sjd, sj, hemi = "lh");
#' th = subject.morph.native(sjd, sj, "thickness", hemi="lh", cortex_only=T);
#' th_smooth = pervertexdata.smoothnn(surface, th, num_iter=15L);
#' vis.data.on.subject(sjd, sj, morph_data_lh = th);
#' vis.data.on.subject(sjd, sj, morph_data_lh = th_smooth);
#' }
#'
#' @keywords internal
pervertexdata.smoothnn <- function(surface, data, num_iter, k=1L) {
    if(! freesurferformats::is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an fs.surface instance.");
    }
    if(nrow(surface$vertices) != length(data)) {
        stop("Number of surface vertices must match data length.");
    }

    surface = ensure.fs.surface(surface);
    if(k==1) {
        adj = fs.surface.as.adjacencylist(surface);
    } else {
        if(requireNamespace("Rvcg", quietly = TRUE)) {
            if(exists('vcgVertexNeighbors', where=asNamespace('Rvcg'), mode='function')) {
                tmesh = ensure.tmesh3d(surface);
                adj = Rvcg::vcgVertexNeighbors(tmesh, vi = NULL, numstep = k, include_self = FALSE);
            } else {
                stop("The installed version of the  'Rvcg' package is too old, please re-install it from Github.");
            }
        } else {
            stop("The 'Rvcg' package must be installed to use this functionality.");
        }
    }
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
            if(is.na(data[vidx])) { next; }
            neigh = c(adj[[vidx]], vidx);
            data_smoothed[vidx] = mean(source_data[neigh], na.rm = TRUE);
        }
    }
    return(data_smoothed);
}


#' @title Compute number of neighborhood smoothing iterations to reach requested fwhm.
#'
#' @inheritParams pervertexdata.smoothnn
#'
#' @param is_template logical, whether the surface belongs to a template subject
#'
#' @return integer, the iteration count
#'
#' @note This function has been adapted from FreeSurfer and it is subject to the FreeSurfer software license.
#'
#' @keywords internal
pervertexdata.smoothnn.compute.numiter<- function(surface, fwhm, is_template) {

    if(! is.logical(is_template)) {
        stop("Parameter 'is_template' must be logical");
    }

    message("Something is wrong with this, FS seems to use a different computation for the mesh area. Maybe the issue is specific to group template surfaces like fsaverage (because of group scaling).");
    surface = ensure.fs.surface(surface);
    total_area = surf.metric.properties(surface, is_template = is_template)$mesh_total_area;
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
#' @param is_template logical, whether the surface belongs to a template subject
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
pervertexdata.smoothnn.compute.fwhm <- function(surface, niters, is_template) {

    message("Something is wrong with this, FS seems to use a different computation for the mesh area. It works for non-template brain it seems.");

    if(! is.integer(niters)) {
        warning("Parameter 'niters' must be an integer, trying conversion.");
        niters = as.integer(niters);
    }

    if(! is.logical(is_template)) {
        stop("Parameter 'is_template' must be logical");
    }

    if(! freesurferformats::is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an fs.surface instance.");
    }
    mesh = ensure.tmesh3d(surface);
    total_area = surf.metric.properties(surface, is_template = is_template)$mesh_total_area;
    avgvtxarea = total_area / nrow(surface$vertices);
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
#' spherical_surface = subject.surface(sjd, "fsaverage3",
#'   surface="sphere", hemi="lh");
#' vdata = subject.morph.native(sjd, "fsaverage3", "thickness", hemi="lh");
#' vdata_smoothed = pervertexdata.smoothgaussian(spherical_surface,
#'  vdata, fwhm = 15);
#' vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = vdata);
#' vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = vdata_smoothed);
#' }
#'
#' @keywords internal
pervertexdata.smoothgaussian <- function(spherical_surface, data, fwhm = 15.0, truncfactor = 3.5) {
    gstd = fwhm / sqrt(log(256.0));
    maxdist = truncfactor * gstd;
    sphere_dists = surf.sphere.dist(spherical_surface, maxdist = maxdist);

    gaussian_weights = surf.sphere.gaussianweights(spherical_surface, sphere_dists, gstd);
    smoothed_data = surf.sphere.spatialfilter(data, sphere_dists, gaussian_weights);
    return(smoothed_data);
}


#' @title Compute average distance from the origin to each vertex.
#'
#' @param surface an fs.surface instance, and for the typical use case of this function, a spherical surface.
#'
#' @param with_stddev logical, whether to compute the standard deviation as well and return a named list with the 'avg' and the 'stddev' instead.
#'
#' @return scalar double, the average distance.
#'
#' @note This is used to determine the sphere radius for spherical surfaces. It is assumed that the sphere is centered at the origin \code{(0,0,0)}.
#'
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3",
#'   surface="sphere", hemi="lh");
#' vr = fsbrain:::surf.avg.vertexradius(spherical_surface);
#' # Show histogram to verify that the surface is a sphere centered at 0,0,0:
#' hist(freesurferformats:::vertexdists.to.point(spherical_surface, c(0,0,0)));
#' # Plot the coords and a point at the origin:
#' fsbrain::highlight.points.spheres(rbind(spherical_surface$vertices, c(0,0,0)));
#' }
#'
#' @keywords internal
surf.avg.vertexradius <- function(surface, with_stddev=FALSE) {
    surface = ensure.fs.surface(surface);
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
#' @description Compute neighborhood of the current vertex (=target vertex). The computation follows the mesh edges while there are still vertices which fullfil the dotproduct distance threshold. Alternatively, one could compute geodesic neighborhoods on the original mesh, but that is a lot slower. If no spherical surface is available, it has to be done though.
#'
#' @param spherical_surface an fs.surface instance representing the spherical version (\code{lh.sphere} or \code{rh.sphere} of the subject).
#'
#' @param maxdist double, the neighborhood size along the sphere, or to be more precise the maximal distance to travel along the sphere (using mesh edges) when searching for neighbors. The maxdist value can be computed from the definition of the Gaussian kernel parameters, i.e., its FWHM and truncation factor. See \code{pervertexdata.smoothgaussian} for an example of how to do that. Note that if the distance is smaller than the edge length, the neighborhoods for the vertices will only contain the vertex itself.
#'
#' @examples
#' \dontrun{
#' spherical_surface = subject.surface(fsaverage.path(), "fsaverage3",
#'   surface="sphere", hemi="lh");
#' sphere_dist = surf.sphere.dist(spherical_surface, 20.0);
#' highlight.vertices.on.subject(fsaverage.path(), "fsaverage3",
#'   verts_lh = sphere_dist$neigh[[500]], surface="sphere");
#' }
#'
#' @return named list with 3 entries. Each is a vector with neighborhood information: 'neigh' is an int vector of the neighbor vertices, 'neigh_dist_dotproduct' a numerical vector of dp distances for these neighbors, and 'neigh_dist_surface' the same for along-the-surface-distances instead of dp distances.
#'
#' @keywords internal
#'
#' @importFrom methods new
surf.sphere.dist <- function(spherical_surface, maxdist) {


    spherical_surface = ensure.fs.surface(spherical_surface);
    nv = nrow(spherical_surface$vertices);

    radius = surf.avg.vertexradius(spherical_surface);
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

        ref_visited <- methods::new("IntVecReference", vec=rep(0L, nv));
        ref_neighbors <- methods::new("IntVecReference", vec=rep(0L, nv));
        ref_neighbor_dpdists <- methods::new("DoubleVecReference", vec=rep(-1.0, nv));
        extend_neighbors(spherical_surface, vidx, vidx, min_dotp_thresh, ref_visited, ref_neighbors, ref_neighbor_dpdists);

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


#' @title Recursive computation of neighborhoods, see surf.sphere.dist
#'
#' @description Compute neighborhood of the current vertex (=target vertex). The computation follows the mesh edges while there are still vertices which fullfil the dotproduct distance threshold.
#'
#' @inheritParams surf.sphere.dist
#'
#' @param targetvidx positive integer, initial target vertex. The vertex for which to compute the neighborhood.
#'
#' @param currentvidx positive integer, initial current vertex. Pass identical value as in targetvidx, this is changed later in the recursion.
#'
#' @param min_dotp_threshold double, the minimal dotproduct distance threshold to use. Only vertices along the structural mesh neighborhood with values greater this will be included in the neighborhood. Yes, greater.
#'
#' @param ref_visited pass-by-reference (via RefClasses) integer vector of length \code{num_mesh_vertices}: whether the respective vertex has been visited already.
#'
#' @param ref_neighbors pass-by-reference (via RefClasses) integer vector of length \code{num_mesh_vertices}: whether the respective vertex is part of the current neighborhood.
#'
#' @param ref_neighbor_dpdists pass-by-reference (via RefClasses) double vector of length \code{num_mesh_vertices}: the dotproduct distance to the respective vertex (if it is part of the neighborhood).
#'
#' @return integer, invisible. Either \code{0L} or \code{1L}. Used in the recursion only, ignore. The return values of interest are in the 3 \code{ref_*} parameters.
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
#' fwhm = 20.0; truncfactor = 3.5; sjd = fsaverage.path();
#' gstd = fwhm / sqrt(log(256.0)); maxdist = truncfactor * gstd;
#' spherical_surface = subject.surface(sjd, "fsaverage3", surface="sphere", hemi="lh");
#' sphere_dists = surf.sphere.dist(spherical_surface, maxdist = maxdist);
#' gaussian_weights = fsbrain:::surf.sphere.gaussianweights(spherical_surface,
#'  sphere_dists, gstd);
#' morph_data = rep(NA, nrow(spherical_surface$vertices));
#' morph_data[sphere_dists$neigh[[500]]] = gaussian_weights[[500]];
#' vis.data.on.subject(sjd, "fsaverage3", morph_data_lh=morph_data);
#' }
#'
#' @return vector of Gaussian weights for vertices
#'
#' @keywords internal
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
#' @param is_template logical, whether the surface comes from a template subject.
#'
#' @param template_scale_factor double, the template scale factor
#'
#' @examples
#' \dontrun{
#' surface = subject.surface(fsaverage.path(), "fsaverage3", hemi="lh");
#' mp = surf.metric.properties(surface, is_template = TRUE);
#' }
#'
#' @return named list of metric surface properties.
#'
#' @keywords internal
surf.metric.properties <- function(surface, is_template, template_scale_factor=1.56) {
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        mp = list();
        mesh = ensure.tmesh3d(surface);
        if(! is.logical(is_template)) {
            stop("Parameter 'is_template' must be logical.");
        }


        mesh = rgl::addNormals(mesh); # Adds per-vertex normals.
        mp$vertex_normals = t(mesh$normals)[,1:3];

        face_normals = t(Rvcg::vcgFaceNormals(mesh));
        mp$face_normals = face_normals;

        is_negative = face_normals[,2] < 0.0;
        face_areas = unlist(Rvcg::vcgArea(mesh, perface = TRUE)$pertriangle);
        mp$face_areas = face_areas;

        mp$mesh_total_area = sum(face_areas[!is_negative]);
        mp$mesh_neg_area = -sum(face_areas[is_negative]);

        if(is_template) {
            # The scale factor exists to make the mesh area of the surface equal to the average mesh area of the surfaces used to crteate the template. It should be save in the surface file in a tag at the end?
            warning("This function does not work for template surfaces: we currently do not know about a way to obtain the scale factor that must be applied to surface areas.");

            mp$mesh_total_area = mp$mesh_total_area * template_scale_factor;
            mp$mesh_neg_area = mp$mesh_neg_area * template_scale_factor;
        }

        return(mp);
    } else {
        stop("This functionality requires the 'Rvcg' package to be installed.");
    }
}



#' @title Apply spatial filter to surface data.
#'
#' @param source_data numerical vector, per-vertex data for a surface.
#'
#' @param sphere_dists named list with 3 entries, as returned by \code{surf.sphere.dist}
#'
#' @param gaussian_weight list of double vectors, the Gaussian weights for all neighbors of the respective vertex. As returned by \code{surf.sphere.gaussianweights}.
#'
#' @return numerical vector, the spatially filtered per-vertex data.
#'
#' @keywords internal
surf.sphere.spatialfilter <- function(source_data, sphere_dists, gaussian_weights) {
    smoothed_data = rep(NA, length(source_data));
    if(! is.list(sphere_dists$neigh)) {
        stop("Parameter 'sphere_dists' member 'neigh' must be a list.");
    }
    if(! is.list(gaussian_weights)) {
        stop("Parameter 'gaussian_weights' must be a list.");
    }
    for(vidx in seq_along(source_data)) {
        smoothed_data[vidx] = sum(source_data[sphere_dists$neigh[[vidx]]] * gaussian_weights[[vidx]]);
    }
    return(smoothed_data);
}


