# Implement a surface spotlight, i.e., a sliding window-like structure moving over the brain surface
# that returns, for each vertex, all vertices in a certain geodesic distance.


## We need a fast Rvcg function to pre-compute the neighborhoods for this.
## UPDATE: That function is now available from https://github.com/dfsp-spirit/Rvcg@geodesic_extra_functions

#' @title Pre-compute geodesic neighborhood for all vertices of the surfaces.
#'
#' @inheritParams geodesic.average.distance
#'
#' @param distance the distance defining the neighborhood. The neighborhoods never cross hemispheres.
#'
#' @note This function requires a Github version of the optional dependency package 'Rvcg'. It is highly experimental and should not be considered part of the official API.
#'
#' @examples
#' \dontrun{
#'   download_fsaverage3(TRUE);
#'   sjd = fsaverage.path();
#'   sf = subject.surface(sjd, "fsaverage3", "white", "both");
#'   neigh = spotlight.neighborhoods.gd(sf, distance = 15.0);
#'   highlight.vertices.on.subject(sjd, "fsaverage3",
#'     verts_lh = neigh$lh[[638]]); # show vertex 638 neighborhood
#' }
#'
#' @export
spotlight.neighborhoods.gd <- function(surfaces, distance = 5.0) {
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        if(is.hemilist(surfaces)) {
            return(lapply(surfaces, spotlight.neighborhoods.gd, distance = distance));
        } else {
            if(! exists('vcgGeodesicNeigh', where=asNamespace('Rvcg'), mode='function')) {
                stop("Your Rvcg version does not export the required 'vcgGeodesicNeigh' function. You need to install Rvcg from GitHub for this this functionality to be available. Try 'devtools::install_github('dfsp-spirit/Rvcg', ref='geodesic_extra_functions')'.");
            } else {
                tmesh = ensure.tmesh3d(surfaces);
                return(Rvcg::vcgGeodesicNeigh(tmesh, dist = distance));
            }
        }
    } else {
        stop("This functionality requires the 'Rvcg' package to be installed.");
    }
}

