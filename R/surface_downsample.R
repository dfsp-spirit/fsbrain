

#' @title Downsample a FreeSurfer template mesh, e.g. from ico7 to ico6 resolution.
#'
#' @description Reduce the number of vertices and faces in a mesh. Only works if the mesh has been created in the special way FreeSurfer does it for templates like fsaverage. This will NOT work for arbitrary meshes.
#'
#' @param surface an fs.surface instance, must originate from a FreeSurfer template brain mesh.
#'
#' @param ntarget scalar integer in range 2..6, the ico order. Must be smaller than the ico order of the input 'surface'.
#'
#' @return the downsampled surface
#'
#' @references See the Brainder.org blog article downsampling-decimating-a-brain-surface by AM Winkler. Also see Winkler AM, Sabuncu MR, Yeo BT, Fischl B, Greve DN, Kochunov P, Nichols TE, Blangero J, Glahn DC. Measuring and comparing brain cortical surface area and other areal quantities. Neuroimage. 2012 Jul 16;61(4):1428-43.
#'
#' @note This function is published under the GPL-2.0 license. It has been translated from MATLAB code which is part of Anderson Winkler's  Areal Toolbox that is published under the GPL-2.0 license.
#'
#' @examples
#' \dontrun{
#'   sjd=fsaverage.path(T);
#'   sf_ico7 = subject.surface(sjd, "fsaverage", hemi="lh");
#'   sf_ico6 = downsample.fs.surface(sf_ico7, 6L);
#' }
#'
#' @export
downsample.fs.surface <- function(surface, ntarget=6L) {

    warning(sprintf("This is WIP and currently broken.\n"));

    if(! is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an fs.surface instance representing a mesh created with FreeSurfer.");
    }
    V0 = 12;
    F0 = 20;
    nv = nrow(surface$vertices);
    n = round(log((nv-2)/(V0-2))/log(4));

    if (nv != 4^n*(V0-2)+2) {
        stop("The source mesh is not from icosahedron.");
    }
    if(ntarget > n) {
        stop("Upsampling not possible: parameter 'ntarget' is %d must be smaller then '%d' for the current input mesh with ico order '%d'.\n", ntarget, n, n);
    }
    if(ntarget == n) {
        message(sprintf("Parameter 'ntarget' equal to determined input mesh ico order, returning unchanged input mesh.\n"));
        return(surface);
    }

    # Select the subset of vertices for downsampled mesh
    ds_vertices = surface$vertices[1:(4^ntarget*(V0-2)+2),];

    fac = surface$faces;
    for(current_ico in seq.int(n-1L,ntarget,by=-1)) {
        nVj = 4^current_ico*(V0-2)+2;
        num_val = 4^current_ico*F0 * 3; # number of entries in new faces matrix (new num_faces x 3)
        facnew = matrix(rep(0, num_val), ncol=3); # the new faces matrix, num_faces x 3
        fout = which(rowSums(fac > nVj) == 3); # get faces which contain only vertices which are not in the new vertices
        for (f in fout) {
            vidx = fac[fout[f,]];
            ftomerge = fac[rowSums(which(fac == vidx[1] | fac == vidx[2] | fac == vidx[3])) == 2, ];
            facnew[f,] = colSums(ftomerge * (ftomerge <= nVj));
        }
        fac = facnew;
    }

    surf_ds = list('vertices'=ds_vertices, 'faces'=fac);
    class(surf_ds) <- c(class(surf_ds), 'fs.surface');
    return(surf_ds);
}
