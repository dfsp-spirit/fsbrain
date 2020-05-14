# export functions


#' @title Export a coloredmeshes with vertexcolors in PLY format.
#'
#' @description Exports coloredmeshes with vertex coloring to standard mesh files in Stanford Triangle (PLY) format. This is very hand for rendering in external standard 3D modeling software like \href{http://blender.org}{Blender}.
#'
#' @param filepath The export filepath, including file name and extension.
#'
#' @param coloredmesh an 'fs.coloredmesh' instance, as returned (silently) by all surface visualization functions, like \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    coloredmeshes = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness');
#'    export.coloredmesh.ply('~/subject1_thickness_lh.ply', coloredmeshed$lh);
#' }
#'
#' @export
#' @importFrom grDevices col2rgb
export.coloredmesh.ply <- function(filepath, coloredmesh) {

    if(is.hemilist(coloredmesh)) {
        stop("Parameter 'coloredmesh': expected 'fs.coloredmesh' instance but received hemilist. Did you mean 'coloredmesh$lh' or 'coloredmesh$rh'?");
    }

    if(is.fs.coloredmesh(coloredmesh)) {
        if(hasIn(coloredmesh, c('metadata', 'fs_mesh'))) {
            mesh = coloredmesh$metadata$fs_mesh;
            freesurferformats::write.fs.surface.ply(filepath, mesh$vertices, mesh$faces, vertex_colors = t(grDevices::col2rgb(coloredmesh$col, alpha=TRUE)));
        } else {
            stop("Parameter 'coloredmesh': instance is missing fs_mesh metadata, cannot export surface.");
        }
    } else {
        stop("Parameter 'coloredmesh' must be an 'fs.coloredmesh' instance.");
    }
}
