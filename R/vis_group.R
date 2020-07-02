# group vis functions

#' @title Recycle paramters.
#'
#' @param x a vector of whatever
#'
#' @param times positive integer, the required length of the output
#'
#' @return a vector of length 'times', with the recycled 'x'
#'
#' @note Todo: most likely there is an R function for this already, find it.
#' @keywords internal
recycle <- function(x, times) {
    if(length(x) == times) {
        return(x);
    } else {
        if(length(x) == 1L) {
            return(rep(x, times));
        } else {
            maybe_excessive = as.integer(ceiling(times/length(x)));
            return(maybe_excessive[1:times]);
        }
    }
}


#' @title Plot atlas annotations for a group of subjects.
#'
#' @description Plot atlas annotations for a group of subjects and combine them into a single large image.
#'
#' @inheritParams vis.subject.annot
#'
#' @param subject_id vector of subject identifiers
#'
#' @param view_angles see \code{\link{get.view.angle.names}}.
#'
#' @param output_img character string, the file path for the output image. Should end with '.png'.
#'
#' @param num_per_row positive integer, the number of tiles per row.
#'
#' @param captions optional vector of character strings, the short text annotations for the individual tiles. Typically used to plot the subject identifier.
#'
#' @note The subjects are plotted row-wise, in the order in which they appear in the 'subject_id' parameter.
#'
#' @family group visualization functions
#'
#' @export
vis.group.annot <- function(subjects_dir, subject_id, atlas, view_angles = 'sd_dorsal', output_img='fsbrain_group_annot.png', num_per_row = 5L, captions = subject_id) {
    num_subjects = length(subject_id);
    subjects_dir = recycle(subjects_dir, num_subjects);
    atlas = recycle(atlas, num_subjects);
    rglactions = list('no_vis'=TRUE);
    coloredmeshes = list();
    for(subject_idx in seq.int(num_subjects)) {
        coloredmeshes[[subject_idx]] = vis.subject.annot(subjects_dir[subject_idx], subject_id[subject_idx], atlas[subject_idx], rglactions = rglactions);
    }
    return(invisible(vis.group.coloredmeshes(coloredmeshes, view_angles = view_angles, output_img = output_img, num_per_row = num_per_row, captions = captions)));
}


#' @title Plot coloredmeshes for a group of subjects.
#'
#' @description Plot coloredmeshes for a group of subjects into a single image.
#'
#' @inheritParams vis.group.annot
#'
#' @param coloredmeshes a list of coloredmeshes lists, each entry in the outer list contains the hemilist of coloredmeshes (lefgt and right hemisphere mesh) for one subject.
#'
#' @note This is a mid-level function, end users may want to call high-level functions like \code{\link{vis.group.annot}} instead.
#'
#' @family group visualization functions
#'
#' @export
vis.group.coloredmeshes <- function(coloredmeshes, view_angles = 'sd_dorsal', output_img='fsbrain_group_annot.png', num_per_row = 5L, captions = NULL) {
    num_subjects = length(coloredmeshes);
    rglactions = list('no_vis'=TRUE);
    tdir = tempdir();
    all_output_images = rep(NA, num_subjects);
    for(subject_idx in seq.int(num_subjects)) {
        cm = coloredmeshes[[subject_idx]];
        subject_output_img = file.path(tdir, sprintf("coloredmesh_num_%d.png", subject_idx));
        vislayout.from.coloredmeshes(cm, view_angles=view_angles, output_img=subject_output_img, silent=TRUE, grid_like=TRUE);
        all_output_images[subject_idx] = subject_output_img;
    }
    return(invisible(arrange.brainview.images.grid(all_output_images, output_img=output_img, num_per_row = num_per_row, captions = captions)));
}
