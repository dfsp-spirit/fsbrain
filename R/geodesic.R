# Functions for computing approx geodesic distance on brain meshes using Rvcg.



#' @title Compute all vertices within given geodesic distance on the mesh.
#'
#' @param mesh an instance of \code{rgl::tmesh3d} or \code{freesurferformats::fs.surface}.
#'
#' @param vertex positive integer (or vector of the latter), the index of the source vertex in the mesh. If a vector, the neighborhoods for all vertices will be computed separately.
#'
#' @param max_distance double, the neighborhood size. All mesh vertices in geodesic distance smaller than / up to this distance will be returned.
#'
#' @param include_max logical, whether the max_distance value is inclusive.
#'
#' @param return_distances logical, whether to compute the 'distances' entry in the returned list. Doing so is a little bit slower, so it can be turned off if not needed.
#'
#' @return named list with the following entries: 'vertices': integer vector, the indices of all vertices in the neigborhood. 'distances': double vector, the distances to the respective vertices (unless 'return_distances' is FALSE).
#'
#' @note This function uses the pseudo-geodesic distance along the mesh edges.
#'
#' @examples
#' \dontrun{
#'   sjd = fsaverage.path(TRUE);
#'   surface = subject.surface(sjd, 'fsaverage', surface = "white", hemi = "lh");
#'   res = geod.vert.neighborhood(surface, 12345L, max_distance = 10.0);
#'   res$vertices;
#' }
#'
#' @keywords internal
geod.vert.neighborhood <- function(mesh, vertex, max_distance=5.0, include_max = TRUE, return_distances = TRUE) {
    mesh = ensure.tmesh3d(mesh);
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        neighborhood = vertex;
        neighborhood_distances = rep(0.0, length(vertex));
        for (v in vertex) {
            geodesic_dists_to_vertex = geodesic.dists.to.vertex(mesh, v);
            if(include_max) {
                this_neighborhood_indices = which(geodesic_dists_to_vertex <= max_distance);
            } else {
                this_neighborhood_indices = which(geodesic_dists_to_vertex < max_distance);
            }
            neighborhood = c(neighborhood, this_neighborhood_indices);
            neighborhood_distances = c(neighborhood_distances, geodesic_dists_to_vertex[this_neighborhood_indices]);
        }

        if(! return_distances) {
            return(list("vertices" = unique(neighborhood)));
        } else {
            # Make neighborhood unique, and also remove the corresponding duplicated distances (so we cannot simply use base::unique).
            verts_unique = c();
            dists_unique = c();
            for(neigh_idx in seq_len(length(neighborhood))) {
                vert_idx = neighborhood[neigh_idx];
                if(! (vert_idx %in% verts_unique)) {
                    verts_unique = c(verts_unique, vert_idx);
                    dists_unique = c(dists_unique, neighborhood_distances[neigh_idx]);
                }
            }
            return(list('vertices' = verts_unique, 'distances' = dists_unique));
        }
    } else {
        stop("The 'Rvcg' package must be installed to use this functionality.");
    }
}


#' @title Generate color overlay from geodesic patches around several vertices.
#'
#' @description Works across hemispheres (for a whole brain) if you pass a \code{\link[fsbrain]{hemilist}} of meshes as parameter 'mesh', see below.
#'
#' @inheritParams geod.vert.neighborhood
#'
#' @param mesh a single \code{fs.surface} instance, or a \code{\link[fsbrain]{hemilist}} of two such meshes. If a hemilist, the vertex indices can go from 1 to the sum of vertices in both meshes, and the proper hemisphere will be used automatically.
#'
#' @param color single color string like \code{'#FF0000'} or vector of such strings. If a vector, the length should match the number of vertices in parameter 'vertex'.
#'
#' @param bg_color character string, the background color.
#'
#' @param ... extra arguments passed to \code{geod.vert.neighborhood}.
#'
#' @return vector of color strings (or a \code{\link[fsbrain]{hemilist}} of 2 such vectors if 'mesh' is a hemilist), an overlay suitable for visualization using \code{vis.color.on.subject}.
#'
#' @examples
#' \dontrun{
#'   sjd = fsaverage.path(TRUE);
#'   surfaces = subject.surface(sjd, 'fsaverage', surface = "white", hemi = "both");
#'   colors = geod.patches.color.overlay(surfaces, vertex = c(12345L, 45L),
#'     color = c("#FF0000", "#00FF00"), max_distance = 45.0);
#'   vis.color.on.subject(sjd, 'fsaverage', color_lh=colors$lh, color_rh=colors$rh);
#' }
#'
#' @keywords internal
geod.patches.color.overlay <- function(mesh, vertex, color = "#FF0000", bg_color = "#FEFEFE", ...) {
    color = recycle(color, length(vertex));
    if(is.hemilist(mesh)) {
        if(! is.fs.surface(mesh$lh)) {
            stop("Paramter 'mesh$lh' must be an fs.surface instance if a hemilist is passed.");
        }
        if(! is.fs.surface(mesh$rh)) {
            stop("Paramter 'mesh$rh' must be an fs.surface instance if a hemilist is passed.");
        }
        lh_nv = nrow(mesh$lh$vertices);
        rh_nv = nrow(mesh$rh$vertices);
        lh_vertex = vertex[which(vertex <= lh_nv)];
        lh_color = color[which(vertex <= lh_nv)];
        rh_vertex = vertex[which(vertex > lh_nv)];
        rh_vertex = rh_vertex - lh_nv;
        rh_color = color[which(vertex > lh_nv)];

        lh_overlay = geod.patches.color.overlay.singlehemi(mesh$lh, lh_vertex, lh_color, bg_color = bg_color, ...);
        rh_overlay = geod.patches.color.overlay.singlehemi(mesh$rh, rh_vertex, rh_color, bg_color = bg_color, ...);
        res = list('lh'=lh_overlay, 'rh'=rh_overlay);
        return(res);
    } else {
        return(geod.patches.color.overlay.singlehemi(mesh, vertex, color, bg_color = bg_color, ...));
    }
}

#' @title Generate color overlay from geodesic patches around several vertices for a single hemi.
#'
#' @inheritParams geod.patches.color.overlay
#'
#' @param mesh a single \code{fs.surface} instance.
#'
#' @keywords internal
geod.patches.color.overlay.singlehemi <- function(mesh, vertex, color = "#FF0000", bg_color = "#FEFEFE", ...) {
    if(! is.fs.surface(mesh)) {
        stop("Parameter 'mesh' must be an fs.surface instance.");
    }
    color = recycle(color, length(vertex));
    nv = nrow(mesh$vertices);
    color_overlay = rep(bg_color, nv);

    query_v_idx = 1L;
    for (v in vertex) {
        neighborhood = geod.vert.neighborhood(mesh, v, ...)$vertices;
        color_overlay[neighborhood] = color[query_v_idx];
        query_v_idx = query_v_idx + 1L;
    }
    return(color_overlay);
}

#' @title Generate per-vertex distance data from geodesic patches around several vertices.
#'
#' @description Works across hemispheres (for a whole brain) if you pass a \code{\link[fsbrain]{hemilist}} of meshes as parameter 'mesh', see below.
#'
#' @inheritParams geod.patches.color.overlay
#'
#' @return vector of doubles (or a \code{\link[fsbrain]{hemilist}} of 2 such vectors if 'mesh' is a hemilist), the per-vertex distance data. Data for vertices outside neighborhoods will be NA.
#'
#' @examples
#' \dontrun{
#'   sjd = fsaverage.path(TRUE);
#'   surfaces = subject.surface(sjd, 'fsaverage',
#'     surface = "white", hemi = "both");
#'   res = geod.patches.pervertexdata(surfaces,
#'     vertex = c(12345L, 45L),
#'     max_distance = 25.0);
#'   # res$lh and res$rh now hold the per-vertex data.
#' }
#'
#' @keywords internal
geod.patches.pervertexdata <- function(mesh, vertex, ...) {
    if(is.hemilist(mesh)) {
        if(! is.fs.surface(mesh$lh)) {
            stop("Paramter 'mesh$lh' must be an fs.surface instance if a hemilist is passed.");
        }
        if(! is.fs.surface(mesh$rh)) {
            stop("Paramter 'mesh$rh' must be an fs.surface instance if a hemilist is passed.");
        }
        lh_nv = nrow(mesh$lh$vertices);
        rh_nv = nrow(mesh$rh$vertices);
        lh_vertex = vertex[which(vertex <= lh_nv)];
        rh_vertex = vertex[which(vertex > lh_nv)];
        rh_vertex = rh_vertex - lh_nv;

        lh_overlay = geod.patches.pervertexdata.singlehemi(mesh$lh, lh_vertex, ...);
        rh_overlay = geod.patches.pervertexdata.singlehemi(mesh$rh, rh_vertex, ...);
        res = list('lh'=lh_overlay, 'rh'=rh_overlay);
        return(res);
    } else {
        return(geod.patches.pervertexdata.singlehemi(mesh, vertex, ...));
    }
}


#' @title Get vertex data for a single fs.surface or a hemilist of surfaces.
#'
#' @param surfaces an fs.surface instance or a \code{\link[fsbrain]{hemilist}} of the latter
#'
#' @param value the morphometry data value you want.
#'
#' @return a vector or hemilist of vectors of values
#'
#' @keywords internal
constant.pervertexdata <- function(surfaces, value = NA) {
    mesh = surfaces;
    if(is.hemilist(mesh)) {
        if(! is.fs.surface(mesh$lh)) {
            stop("Paramter 'mesh$lh' must be an fs.surface instance if a hemilist is passed.");
        }
        if(! is.fs.surface(mesh$rh)) {
            stop("Paramter 'mesh$rh' must be an fs.surface instance if a hemilist is passed.");
        }
        lh_nv = nrow(mesh$lh$vertices);
        rh_nv = nrow(mesh$rh$vertices);
        return(list('lh'=rep(value, lh_nv), 'rh'=rep(value, rh_nv)));
    } else {
        if(! is.fs.surface(mesh)) {
            stop("Paramter 'mesh' must be an fs.surface instance (unless a hemilist is passed).");
        } else {
            return(rep(value, nrow(mesh$vertices)));
        }
    }
}



#' @title Generate per-vertex distance data from geodesic patches around several vertices for a single hemi.
#'
#' @inheritParams geod.patches.pervertexdata
#'
#' @param mesh a single \code{fs.surface} instance.
#'
#' @seealso geod.patches.pervertexdata
#'
#' @keywords internal
geod.patches.pervertexdata.singlehemi <- function(mesh, vertex, ...) {
    if(! is.fs.surface(mesh)) {
        stop("Paramter 'mesh' must be an fs.surface instance.");
    }
    nv = nrow(mesh$vertices);
    data_overlay = rep(NA, nv);

    query_v_idx = 1L;
    for (v in vertex) {
        neighborhood = geod.vert.neighborhood(mesh, v, ...);
        data_overlay[neighborhood$vertices] = neighborhood$distances;
        query_v_idx = query_v_idx + 1L;
    }
    return(data_overlay);
}



#' @title Simple internal wrapper around \code{Rvcg::vcgDijkstra} with function check.
#'
#' @param mesh a tmesh3d instance.
#'
#' @param v positive integer, a vertex index in the mesh.
#'
#' @return double vector with length equal to num vertices in the mesh, the geodesic distances from all other vertices to the query vertex \code{v}.
#'
#' @keywords internal
geodesic.dists.to.vertex <- function(mesh, v) {
    if(! exists('vcgDijkstra', where=asNamespace('Rvcg'), mode='function')) {
        stop("Your Rvcg version does not export the vcgDijkstra function, which means it is too old. You need to install Rvcg from GitHub for this this functionality to be available. Try 'devtools::install_github('zarquon42b/Rvcg')'.");
    } else {
        return(Rvcg::vcgDijkstra(mesh, v));
    }
}



#' @title Ensure the mesh is a tmesh3d instance. Will convert fs.surfaces to one automatically.
#'
#' @param mesh whatever, but hopefully an \code{rgl::tmesh3d} or \code{freesurferformats::fs.surface} instance. Can be a character string, which will be loaded as a surface file if it exists.
#'
#' @return tmesh3d instance, the input or converted from the input.
#'
#' @note This function will stop if the mesh cannot be converted to tmesh3d.
#'
#' @keywords internal
ensure.tmesh3d <- function(mesh) {
    if(is.character(mesh) && length(mesh) == 1L) { # treat as filename}
        if(file.exists(mesh)) {
            mesh = freesurferformats::read.fs.surface(mesh);
        }
    }

    if(freesurferformats::is.fs.surface(mesh)) {
        return(fs.surface.to.tmesh3d(mesh));
    } else if ("mesh3d" %in% class(mesh)) {
        return(mesh);
    } else {
        stop("Cannot convert value in parameter 'mesh' to tmesh3d instance, invalid mesh.");
    }
}


#' @title Compute the average (pseudo-) geodesic distance on the mesh from each vertex to all other vertices.
#'
#' @param surfaces fs.surface instance or a \code{\link[fsbrain]{hemilist}} of the latter.
#'
#' @note This may take a while. It requires the 'Rvcg' package.
#'
#' @keywords internal
geodesic.average.distance <- function(surfaces) {

    if(!requireNamespace("Rvcg", quietly = TRUE)) {

        if(! exists('vcgDijkstra', where=asNamespace('Rvcg'), mode='function')) {
            stop("Your Rvcg version does not export the vcgDijkstra function. You need to install Rvcg from GitHub for this this functionality to be available. Try 'devtools::install_github('zarquon42b/Rvcg')'.");
        }

        if(is.hemilist(surfaces)) {
            return(lapply(surfaces, geodesic.average.distance));
        } else {
            if(is.null(surfaces)) {  # hemilist with empty entry
                return(NULL);
            }
            num_verts = nrow(surfaces$vertices);
            mesh = ensure.tmesh3d(surfaces);
            geodesic_mean_distances = rep(NA, num_verts);
            for(vert_idx in seq_len(num_verts)) {
                geodesic_mean_distances[vert_idx] = mean(Rvcg::vcgDijkstra(mesh, vert_idx));
            }
            return(geodesic_mean_distances);
        }
    } else {
        stop("The 'Rvcg' package must be installed to use this functionality.");
    }
}


#' @title Compute mean geodesic distance descriptor for a subject.
#'
#' @description For all vertices: compute the mean pseudo-geodesic distance from this vertex to all others in the same hemisphere. Computes \code{|V|^2} geodesic distances.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param ... extra parameters passed on to \code{geodesic.average.distance}. Ignored if 'cortex_only' is TRUE.
#'
#' @return a \code{\link[fsbrain]{hemilist}} containing vectors with the descriptor data for the requested hemisphere(s). The length of the vectors is the number of vertices in the surface, and the value for a vertex is the mean geodesic distance to all other vertices for this vertex.
#'
#' @note This takes quite a while for full-resolution meshes. Use down-sampled versions to quickly try it (see example).
#'
#' @examples
#' \dontrun{
#'   download_fsaverage3(TRUE);
#'   sjd = fsaverage.path();
#'   dist = subject.descriptor.geodesic.average.distance(sjd,
#'     "fsaverage3", surface = "white", hemi = "both");
#'   vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = dist$lh);
#' }
#'
#' @keywords internal
subject.descriptor.geodesic.average.distance <- function(subjects_dir, subject_id, surface = "white", hemi = "both", ...) {
    surfaces = subject.surface(subjects_dir, subject_id, surface = surface, hemi = hemi, force_hemilist = TRUE);
    return(geodesic.average.distance(surfaces, ...));
}


# #' @title Testing stuff, ignore this.
# #'
# #' @param direction 1 or -1, direction along axis for normals/rays
# #'
# #' @examples
# #' \dontrun{
# #' surface.outline.2d(fsaverage.path(TRUE), "fsaverage");
# #' }
# #'
# #' @keywords internal
# surface.outline.2d <- function(subjects_dir, subject_id, coord_x = 10.0, direction=1L) {
#     tm = subject.surface(subjects_dir, subject_id, hemi = "lh", as_tm = T);
#     lf = coord_x; # look from
#     search_points = matrix(c(lf, 1.0, 1.0, lf, 1, 0, lf, 1, 1, lf, -1, -1, lf, -1, 0), ncol=3, byrow = T);
#     search_point_normals = search_points;
#     search_point_normals[,1] = search_points[,1] + direction;
#
#     grid = Rvcg::setRays(search_points, search_point_normals);
#     matches = Rvcg::vcgRaySearch(grid, tm);
#
#     # Visualize and mark hit points with small red spheres.
#     rgl::open3d();
#     rgl::shade3d(tm, col="white");
#     rgl::spheres3d(t(matches$vb[1:3,]), color = "red");
#
#     # Mark source grid
#     rgl::spheres3d(search_points, color = "green");
#     rgl::spheres3d(search_point_normals, color = "blue");
# }


#' @title Compute geodesic path from a source vertex to one or more target vertices.
#'
#' @param surface an \code{rgl::tmesh3d} or \code{freesurferformats::fs.surface} instance. Can be a character string, which will be loaded as a surface file if it exists.
#'
#' @param source_vertex a scalar positive integer, the source vertex index in the mesh
#'
#' @param target_vertices single integer or vector of integers, the target vertices to which to compute the paths from the source_vertex.
#'
#' @note This can take a bit for very large graphs. This requires the optional dependency package 'Rvcg'. The backtracking is currently done in R, which is not optimal from a performance perspective. If you have a recent Rvcg version with the Rvcg::vcgGeodesicPath function, that one will be used instead, and the performance will be better.
#'
#' @examples
#' \dontrun{
#'   sjd = fsaverage.path(TRUE);
#'   surface = subject.surface(sjd, 'fsaverage3',
#'     surface = "white", hemi = "lh");
#'   p = geodesic.path(surface, 5, c(10, 20));
#'   vis.subject.morph.native(sjd, 'fsaverage3', 'thickness', views='si');
#'   vis.paths.along.verts(surface$vertices, p$paths, color=c("red", "yellow"));
#' }
#'
#' @return list of integer vectors, the paths
#'
#' @export
geodesic.path <- function(surface, source_vertex, target_vertices) {
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        tmesh = ensure.tmesh3d(surface);

        if(length(source_vertex) != 1L) {
            stop("Must give exactly 1 vertex index as parameter 'source_vertex'.");
        }

        if(exists('vcgGeodesicPath', where=asNamespace('Rvcg'), mode='function')) {
            return(Rvcg::vcgGeodesicPath(tmesh, source_vertex, target_vertices));
        }

        dists = geodesic.dists.to.vertex(tmesh, source_vertex);
        paths = list();
        for(target_idx in seq_along(target_vertices)) {
            target_vertex = target_vertices[target_idx]; # Backtracking part of Dijkstra algo to obtain the path from the dist map.
            current_vertex = target_vertex;
            path = current_vertex;
            visited = c();
            while(current_vertex != source_vertex) {
                visited = c(visited, current_vertex);
                neigh = mesh.vertex.neighbors(surface, source_vertices = current_vertex)$vertices;
                neigh_unvisited = neigh[which(!(neigh %in% visited))];
                neigh_source_dists = dists[neigh_unvisited];     # geodesic distance of neighbors to source vertex
                closest_to_source = neigh_unvisited[which.min(neigh_source_dists)]; # greedily jump to closest one
                path = c(path, closest_to_source);
                current_vertex = closest_to_source;
            }
            paths[[target_idx]] = rev(path);
        }
        return(paths);
    } else {
        stop("The 'Rvcg' pcakge must be installed to use this.");
    }
}


#' @title Compute geodesic circles and ball stats for given vertices.
#'
#' @inheritParams geodesic.path
#'
#' @param vertices positive integer vector, the vertex indices for which to compute the stats. If NULL, it is computed for all vertices.
#'
#' @param scale double, surface area to be covered by patch in percent
#'
#' @note This takes a while for large meshes, try it with single vertices or with a surface like fsaverage3 if you want it for all vertices. This requires the optional dependency package 'pracma'.
#'
#' @examples
#' \dontrun{
#'   sjd = fsaverage.path(TRUE);
#'   surface = subject.surface(sjd, 'fsaverage3', hemi='lh');
#'   gc = geodesic.circles(surface);
#'   vis.data.on.subject(sjd, 'fsaverage3', morph_data_lh = gc$radius);
#'   vis.data.on.subject(sjd, 'fsaverage3', morph_data_lh = gc$perimeter);
#' }
#'
#' @keywords interal
geodesic.circles <- function(surface, vertices=NULL, scale=5.0) {

    warning("This function has not been tested yet, do not use.");

    if(requireNamespace("pracma", quietly = TRUE)) {

        tstart = Sys.time();

        if(scale <= 0.0 | scale > 100) {
            stop("Parameter 'scale' must be in range ]0..100].");
        }
        mesh = ensure.tmesh3d(surface);
        num_verts = ncol(mesh$vb);
        if(any(vertices < 1L) | any(vertices > num_verts)) {
            stop(sprintf("Parameter 'vertices' must be an integer vector containing values from 1 to %d.\n", num_verts));
        }
        if(is.null(vertices)) {
            vertices = seq(1L, num_verts);
        }
        total_area = Rvcg::vcgArea(mesh);
        area_scale = (scale * total_area) / 100.0;
        ball_radius = sqrt(area_scale/pi);

        sampling = 10.0;
        max_dist_extra = 10.0; # Arbitrary setting, the value depends on the mesh scale. The default should depend on the radius value, or be a parameter that can be set by the user.
        maxdist = ball_radius + max_dist_extra;

        res = list('radius'=rep(NA, length(vertices)), 'perimeter'=rep(NA, length(vertices)));
        for(vertex_idx in seq_along(vertices)) {
            vertex = vertices[vertex_idx];
            geodists = geodesic.dists.to.vertex(mesh, vertex);
            sample_at_radii = seq(ball_radius-10, ball_radius+10, length.out=sampling);
            bs = geodesic.ballstats(mesh, geodists, sample_at_radii);

            ## Perform spline interpolation.
            ## See https://www.rdocumentation.org/packages/pracma/versions/1.9.9/topics/interp1, especially the
            ## section at the bottom named '## Difference between spline (Matlab) and spline (R).'
            x = seq(1, 10);
            xx = seq(1, 10, by = 0.1);
            AA = pracma::interp1(x, bs$ball_area, xx, method = "spline");
            RR = pracma::interp1(x, sample_at_radii, xx, method = "spline");
            PP = pracma::interp1(x, bs$ball_perimeter, xx, method = "spline");

            index = which.min(abs(area_scale - AA));
            res$radius[vertex_idx] = RR[index];
            res$perimeter[vertex_idx] = PP[index];
        }

        tend = Sys.time();
        tdiff = tend - tstart;

        return(res);
    } else {
        stop("The 'pracma' package must be installed to use this functionality.");
    }
}


#' @title Compute geodesic ball area and perimeter at location defined by geodists for all radii.
#'
#' @param mesh tmesh3d instance
#'
#' @param geodist vector of geodesic distance for current mesh vertex under consideration (no need for the index)
#'
#' @param sample_at_radii double vector, the different ball radii for which to perform the computation
#'
#' @note This is called from \code{geodesic.circles}, there should be no need to call it directly.
#'
#' @return named list with entries: 'ball_area': vector of double, ball area at each sample radius. 'ball_perimeter': vector of double, ball perimeter at each sample radius.
#'
#' @keywords internal
geodesic.ballstats <- function(mesh, geodist, sample_at_radii) {
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        face_area = Rvcg::vcgArea(mesh, perface = TRUE)$pertriangle;
        vertex_faces = Rvcg::vcgVFadj(mesh);
        num_radii = length(sample_at_radii);
        res = list('ball_area'=rep(NA, num_radii), 'ball_perimeter'=rep(NA, num_radii));
        for(radius_idx in seq_along(sample_at_radii)) {
            radius = sample_at_radii[radius_idx];

            # Find all faces which are in radius.
            in_radius = as.integer(geodist < radius);           # I: whether vertex is in radius (logical)
            face_in_radius_vertices = rowSums(matrix(in_radius[mesh$it], ncol=3, byrow=T)); # C: count how many vertices per face are in radius

            total_area_in_radius = sum(face_area[face_in_radius_vertices==3]);   # A: total area in radius. Currently it is the area of all faces which are fully (all 3 verts) in radius.
            total_perimeter = 0; # P

            # Now compute partial area for faces which are only partly in range.
            partial_face_indices = which(face_in_radius_vertices == 1 | face_in_radius_vertices == 2);
            for(pf_idx in partial_face_indices) {
                face_verts = mesh$it[,pf_idx]; # int vector of length 3

                if(face_in_radius_vertices[pf_idx] == 2L) { # 2 verts in, 1 out
                    k = which(in_radius[face_verts] == FALSE);
                } else { # one vert in, 2 out
                    k = which(in_radius[face_verts] == TRUE);
                }
                reorder_indices = (c(k-1, k, k+1) %% 3) + 1L; # Reorder verts (k=1: reorder to 0,1,2     k=2: to 1,2,0   k=3: 2,0,1)
                face_verts = face_verts[reorder_indices];
                face_vert_dists = geodist[face_verts] - radius; # d, double vec of length 3
                face_vert_coords = mesh$vb[1:3, face_verts]; # v, the 3x3 matrix if vertex coords for the 3 verts of the face.
                v = face_vert_coords;
                # Compute the crossing positions along [f(1) f(2)] using linear interpolation.
                alpha1 = face_vert_dists[2]/(face_vert_dists[2]-face_vert_dists[1]); # scalar double
                v1 = alpha1*v[,1] + (1-alpha1)*v[,2];
                alpha2 = face_vert_dists[3]/(face_vert_dists[3]-face_vert_dists[1]); # scalar double
                v2 = alpha2*v[,1] + (1-alpha2)*v[,3];
                # Compute triangle area
                b = pracma::Norm( pracma::cross(v[,1]-v1,v[,1]-v2))/2;
                # add it positively or negatively to the area
                if(face_in_radius_vertices[pf_idx] == 2L) { # 2 verts in, 1 out
                    total_area_in_radius = total_area_in_radius + face_area[pf_idx] - b;
                } else { # one vert in, 2 out
                    total_area_in_radius = total_area_in_radius + b;
                }
                # add to the perimeter
                total_perimeter = total_perimeter + pracma::Norm(v1-v2);
            }
            res$ball_area[radius_idx] = total_area_in_radius;
            res$ball_perimeter[radius_idx] = total_perimeter;
        }
        return(res);
    } else {
        stop("The 'Rvcg' package must be installed to use this functionality.");
    }
}


# @keywords internal
#
# @importFrom stats cor
#test_ballstats <- function() {
#    # Load expected data from running fastmarching matlab toolbox
#    lh_perim = freesurferformats::read.fs.curv("~/develop/neuroimaging/stuff_by_others/toolbox_geodesic/output_fsaverage3/lh.PerimeterFunction.5.w")
#    rh_perim = freesurferformats::read.fs.curv("~/develop/neuroimaging/stuff_by_others/toolbox_geodesic/output_fsaverage3/rh.PerimeterFunction.5.w")
#    lh_rad = freesurferformats::read.fs.curv("~/develop/neuroimaging/stuff_by_others/toolbox_geodesic/output_fsaverage3/lh.RadiusFunction.5.w")
#    rh_rad = freesurferformats::read.fs.curv("~/develop/neuroimaging/stuff_by_others/toolbox_geodesic/output_fsaverage3/rh.RadiusFunction.5.w")
#    expected = list("lh_perim"=lh_perim, "rh_perim"=rh_perim, "lh_rad"=lh_rad, "rh_rad"=rh_rad);
#
#    # Compute data in R
#    sjd = fsaverage.path(TRUE);
#    surfaces = subject.surface(sjd, 'fsaverage3', hemi='both');
#    lh_gc = geodesic.circles(surfaces$lh);
#    rh_gc = geodesic.circles(surfaces$rh);
#    found = list("lh_perim"=lh_gc$perimeter, "rh_perim"=rh_gc$perimeter, "lh_rad"=lh_gc$radius, "rh_rad"=rh_gc$radius);
#
#    # Check similarity
#    stats::cor(expected$lh_perim, found$lh_perim);
#    stats::cor(expected$rh_perim, found$rh_perim);
#    stats::cor(expected$lh_rad, found$lh_rad);
#    stats::cor(expected$rh_rad, found$rh_rad);
#}

