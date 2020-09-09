

#' @title Transform spherical coordinates to FreeSurfer surface space to plot things around a brain.
#'
#' @param lon numerical vector, the longitudes, passed to \code{sphereplot::sph2car}. See 'deg' for unit information.
#'
#' @param lat numerical vector, the latitudes, passed to \code{sphereplot::sph2car}. See 'deg' for unit information.
#'
#' @param radius numerical vector, the radii, passed to \code{sphereplot::sph2car}. Defaults to the radius of the combined mesh from the fsaverage lh and rh surfaces.
#'
#' @param center numerical vector of length 3, the x, y, and z coordinates of the target center. The \code{sphereplot::sph2car} function operates on the unit sphere, and this parameter is used to translate the resulting cartesian coordinates to a new center, typically the center of the surface meshes or MRI volume or substructures. If you want no translation, pass \code{c(0,0,0)}.
#'
#' @param deg logical, whether to use degrees (as opposed to radians) as the unit for 'lat' and 'lon'. Passed to \code{sphereplot::sph2car}.
#'
#' @note This function can be used to plot things in FreeSurfer space using spherical coordinates, as commonly used in EEG to define electrode positions. Requires the 'sphereplot' package.
#'
#' @examples
#' \dontrun{
#'     # Draw voxels on a sphere around fsaverage:
#'     lat = seq.int(from=0, to=360, by=30);
#'     lon = rep(0, length(lat));
#'     vis.fs.surface('~/software/freesurfer/subjects/fsaverage/surf/lh.white');
#'     fsbrain::rglvoxels(sph2fs(lat, lon), voxelcol = 'red');
#'     fsbrain::rglvoxels(sph2fs(lon, lat), voxelcol = 'green');
#' }
#'
#' @importFrom rgl translate3d
#'
#' @keywords internal
sph2fs <- function(lon, lat, radius = surf.radius.fsaverage(), center=surf.center.fsaverage(), deg = TRUE) {
    if(requireNamespace('sphereplot', quietly = TRUE)) {
        cartesian_coords = sphereplot::sph2car(lon, lat, radius=radius, deg=deg);
        return(rgl::translate3d(data.matrix(cartesian_coords), center[1], center[2], center[3]));
    } else {
        stop("The 'sphereplot' package is required to use this functionality.");
    }
}


#' @title Internal function to get some demo EEG electrode coordinates. Will be removed from public API. Do not use this.
#'
#' @param label_subset vector of character strings, electrode names like 'Nz' or 'RPA'. ÖLeave at the default value `NULL` to get all available ones.
#'
#' @return data.frame with one row per electrode, and the following 3 columns: 'label': the electrode name, 'theta': the azimuth in degrees, 'phi': the latitude in degrees.
#'
#' @references See http://wiki.besa.de/index.php?title=Electrodes_and_Surface_Locations
#'
#' @note There are lots of different naming conventions for spherical coordinates, see https://en.wikipedia.org/wiki/Spherical_coordinate_system.
#'
#' @keywords internal
eeg_coords <- function(label_subset=NULL) {

    # Fp1 and Fp2 are near the left and right eyebrow
    # A1 and A2 are near left and right ear
    # Cz is at the tip (upper center) of the head
    # Nz is near the nose
    # see http://wiki.besa.de/index.php?title=Electrodes_and_Surface_Locations
    #
    # On the theta and phi values, quoted from that wiki page:
    # 'The spherical coordinates are defined in degrees by the azimuth (from Cz, positive = right, negative = left hemisphere) and the latitude (counterclockwise from T7/T3 for left and from T8/T4 for right hemisphere) of each electrode.'
    # Note: T3 is just another name for T7 (same for T4 nad T8).

    c_label = c("Fp1", "Fp2", "A1", "A2", "Cz", "Nz", "LPA", "RPA", "T3", "T4");
    theta = c(-92, 92, -128, 128, 0, 112, -115, 115, -92, 92);
    phi = c(-72, 72, 3, -3, 0, 90, 0, 0, 0, 0);

    df = data.frame('label'=c_label, 'theta'=theta, 'phi'=phi, stringsAsFactors = FALSE);
    if(is.null(label_subset)) {
        return(df);
    } else {
        return(subset(df, df$label %in% label_subset));
    }
}


#' @title Plot x, y and z axes in R,G,B.
#'
#' @description Plot positive x, y, and z axes from the center to 'len'. Gets added to current plot. Useful for debugging and understanding the `rgl` coordinate system.
#'
#' @param len numeric scalar or vector of length 3, length of axes. You can specify a negative value to see the negative directions.
#'
#' @return `NULL`, called for the plotting side effect.
#'
#' @note The x, y and z axes are plotted in red, green, and blue, respectively.
#'
#' @importFrom rgl rgl.lines
#' @keywords internal
rgl.coord.lines <- function(len = 100) {
    len = recycle(len, 3L);
    rgl::rgl.lines(x=c(0, len[1]), y=c(0,0), z=c(0,0), col="red");
    rgl::rgl.lines(x=c(0, 0), y=c(0,len[2]), z=c(0,0), col="green");
    rgl::rgl.lines(x=c(0, 0), y=c(0,0), z=c(0,len[3]), col="blue");
    return(invisible(NULL));
}


#' @title Get pre-computed radius for fsaverage white surface.
#'
#' @param maxr logical, whether to return the maximum of the x, y, and z radii.
#'
#' @return If 'maxr' is TRUE, a numeric scalar representing the max of the x, y, and z radii, otherwise, a numerical vector of length 3 with all radii.
#'
#' @note The coordinates are for the white surface and in surface space, i.e., based on the raw values stored in the `fsaverae/surf/lh.white` and `fsaverage/surf/rh.white` files, without applying any transformation.
#'
#' @seealso \code{\link{surfs.props}}, which was used to compute the returned values.
#'
#' @keywords internal
surf.radius.fsaverage <- function(maxr = TRUE) {
    radii_xyz = c(66.22490, 84.01643, 59.90088);
    if(maxr) {
        return(max(radii_xyz));
    }
    return(radii_xyz);
}


#' @title Get pre-computed center for fsaverage white surface.
#'
#' @return A numerical vector of length 3 with the x, y, and z coordinates of the center. The center was computed as the point halfway between the min and max mesh coordinates, on each axis separately.
#'
#' @note The coordinates are for the white surface and in surface space, i.e., based on the raw values stored in the `fsaverae/surf/lh.white` and `fsaverage/surf/rh.white` files, without applying any transformation.
#'
#' @seealso \code{\link{surfs.props}}, which was used to compute the returned values.
#' @keywords internal
surf.center.fsaverage <- function() {
    return(c(0.5622063, -18.4871483, 16.3729954));
}


#' @title Compute simple version of center and radius of 2 meshes.
#'
#' @param lh `fs.surface` instance, the mesh for the left hemisphere. If a string, assumed to be the path to a surface mesh file that should be loaded.
#'
#' @param rh `fs.surface` instance, the mesh for the right hemisphere. If a string, assumed to be the path to a surface mesh file that should be loaded.
#'
#' @return named list with entries 'center', 'radius', 'min' and 'max', each of which are numerical vectors of length 3, holding the x, y and z coordinates or values.
#'
#' @note This function treats the 'lh' and 'rh' meshes as a single mesh, and computes the properties for this combined mesh.
#'
#' @keywords internal
surfs.props <- function(lh, rh) {
    if(is.character(lh)) {
        lh = freesurferformats::read.fs.surface(lh);
    }
    if(is.character(rh)) {
        rh = freesurferformats::read.fs.surface(rh);
    }
    min_x = min(min(lh$vertices[,1]), min(rh$vertices[,1]));
    min_y = min(min(lh$vertices[,2]), min(rh$vertices[,2]));
    min_z = min(min(lh$vertices[,3]), min(rh$vertices[,3]));

    max_x = max(max(lh$vertices[,1]), max(rh$vertices[,1]));
    max_y = max(max(lh$vertices[,2]), max(rh$vertices[,2]));
    max_z = max(max(lh$vertices[,3]), max(rh$vertices[,3]));

    center = c(mean(c(min_x, max_x)), mean(c(min_y, max_y)), mean(c(min_z, max_z)))
    radius = c((max_x - min_x) / 2.0, (max_y - min_y) / 2.0, (max_z - min_z) / 2.0);
    return(list('center'=center, 'radius'=radius, 'min'=c(min_x, min_y, min_z), 'max'=c(max_x, max_y, max_z)));
}
