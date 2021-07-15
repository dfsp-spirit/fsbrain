

#' @title Downsample a FreeSurfer mesh, e.g. from ico7 to ico6 resolution.
#'
#' @description Reduce the number of vertices and faces in a mesh. Only works if the mesh has been created in the special way FreeSurfer does it. This will NOT work for arbitrary meshes.
#'
#' @param surface an fs.surface instance, must originate from a FreeSurfer brain mesh.
#'
#' @param ntarget scalar integer in range 2..6, the ico order. Must be smaller than the ico order of the input 'surface'.
#'
#' @references See the Brainder.org blog article downsampling-decimating-a-brain-surface by AM Winkler.
#'
#' @note This does NOT downsample per-vertex data for a mesh, but the mesh itself.
downsample.fs.surface <- function(surface, ntarget=6L) {
    if(! is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an fs.surface instance representing a mesh created with FreeSurfer.");
    }
    V0 = 12;
    F0 = 20;
    n = round(log((nV-2)/(V0-2))/log(4));

    if (nV != 4^n*(V0-2)+2) {
        stop("The source mesh is not from icosahedron.");
    }
    if(ntarget > n) {
        stop("Upsampling not possible: parameter 'ntarget' is %d must be smaller then '%d' for the current input mesh with ico order '%d'.\n", ntarget, n, n);
    }
    if(ntarget = n) {
        message(sprintf("Parameter 'ntarget' equal to determined input mesh ico order, returning unchanged input mesh.\n"));
        return(surface);
    }

}
