# Functions related to cortical parcellations.


#' @title Extract a region from an annotation as a label.
#'
#' @description The returned label can be used to mask morphometry data, e.g., to set the values of a certain region to NaN or to extract only values from a certain region.
#'
#' @param annot, annotation. An annotation for one hemisphere, as returned by subject.annot().
#'
#' @param region, string. A valid region name for the annotation, i.e., one of the regions of the atlas used to create the annotation.
#'
#' @param return_one_based_indices, logical. Whether the indices should be 1-based. Indices are stored zero-based in label files, but R uses 1-based indices. Defaults to TRUE.
#'
#' @param invert, logical. If TRUE, return the indices of all vertices which are NOT part of the region. Defaults to FALSE.
#'
#' @return integer vector with label data: the list of vertex indices in the label. See 'return_one_based_indices' for important information.
#'
#' @export
label.from.annot <- function(annot, region, return_one_based_indices=TRUE, invert=FALSE) {

    if(invert) {
        indices = which(annot$label_names != region);
    } else {
        indices = which(annot$label_names == region);
    }

    if(!return_one_based_indices) {
        indices = indices - 1;
    }

    return(indices);
}


#' @title Extract a region from an atlas annotation as a label for a subject.
#'
#' @description The returned label can be used to mask morphometry data, e.g., to set the values of a certain region to NaN or to extract only values from a certain region.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param region, string. A valid region name for the annotation, i.e., one of the regions of the atlas.
#'
#' @param return_one_based_indices, logical. Whether the indices should be 1-based. Indices are stored zero-based in label files, but R uses 1-based indices. Defaults to TRUE.
#'
#' @param invert, logical. If TRUE, return the indices of all vertices which are NOT part of the region. Defaults to FALSE.
#'
#' @return integer vector with label data: the list of vertex indices in the label.
#'
#' @export
subject.label.from.annot <- function(subjects_dir, subject_id, hemi, atlas, region, return_one_based_indices=TRUE, invert=FALSE) {
    annot = subject.annot(subjects_dir, subject_id, hemi, atlas);
    return(label.from.annot(annot, region, return_one_based_indices=return_one_based_indices, invert=invert));
}


