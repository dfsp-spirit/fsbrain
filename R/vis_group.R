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
#' @param subject_id vector of character strings, the subject identifiers
#'
#' @param atlas vector of character strings, the atlas names. Example: \code{c('aparc', 'aparc.a2009s')}
#'
#' @param view_angles see \code{\link{get.view.angle.names}}.
#'
#' @param output_img character string, the file path for the output image. Should end with '.png'.
#'
#' @param num_per_row positive integer, the number of tiles per row.
#'
#' @param captions optional vector of character strings, the short text annotations for the individual tiles. Typically used to plot the subject identifier.
#'
#' @param ... extra parameters passed to the subject level visualization function. Not all may make sense in this context. Example: \code{surface='pial'}.
#'
#' @note The subjects are plotted row-wise, in the order in which they appear in the 'subject_id' parameter. This function is vectorized over 'subject_id' and 'atlas'.
#'
#' @family group visualization functions
#'
#' @export
vis.group.annot <- function(subjects_dir, subject_id, atlas, view_angles = 'sd_dorsal', output_img='fsbrain_group_annot.png', num_per_row = 5L, captions = subject_id, rglactions = list('no_vis'=TRUE), ...) {
    num_plots = max(length(subject_id), length(atlas));
    if(num_plots > length(subject_id)) {
        subject_id = recycle(subject_id, num_plots);
        captions = subject_id;
    }
    subjects_dir = recycle(subjects_dir, num_plots);
    atlas = recycle(atlas, num_plots);
    coloredmeshes = list();
    for(plot_idx in seq.int(num_plots)) {
        coloredmeshes[[plot_idx]] = vis.subject.annot(subjects_dir[plot_idx], subject_id[plot_idx], atlas[plot_idx], views='si', rglactions = rglactions, ...);
    }
    return(invisible(vis.group.coloredmeshes(coloredmeshes, view_angles = view_angles, output_img = output_img, num_per_row = num_per_row, captions = captions)));
}



#' @title Plot native space morphometry data for a group of subjects.
#'
#' @description Plot native space morphometry data for a group of subjects and combine them into a single large image.
#'
#' @inheritParams vis.group.annot
#'
#' @param measure vector of character strings, the morphometry measures, e.g., \code{c('thickness', 'area')}
#'
#' @note The subjects are plotted row-wise, in the order in which they appear in the 'subject_id' parameter. This function is vectorized over 'subject_id' and 'measure'.
#'
#' @family group visualization functions
#'
#' @export
vis.group.morph.native <- function(subjects_dir, subject_id, measure, view_angles = 'sd_dorsal', output_img='fsbrain_group_morph.png', num_per_row = 5L, captions = subject_id, rglactions = list('no_vis'=TRUE), ...) {
    num_plots = max(length(subject_id), length(measure));
    if(num_plots > length(subject_id)) {
        subject_id = recycle(subject_id, num_plots);
        captions = subject_id;
    }
    subjects_dir = recycle(subjects_dir, num_plots);
    measure = recycle(measure, num_plots);
    coloredmeshes = list();
    for(plot_idx in seq.int(num_plots)) {
        coloredmeshes[[plot_idx]] = vis.subject.morph.native(subjects_dir[plot_idx], subject_id[plot_idx], measure[plot_idx], views='si', rglactions = rglactions, ...);
    }
    return(invisible(vis.group.coloredmeshes(coloredmeshes, view_angles = view_angles, output_img = output_img, num_per_row = num_per_row, captions = captions)));
}


#' @title Plot standard space morphometry data for a group of subjects.
#'
#' @description Plot standard space morphometry data for a group of subjects and combine them into a single large image.
#'
#' @inheritParams vis.group.morph.native
#'
#' @param fwhm vector of character strings, the smoothing kernel FWHM strings, e.g., \code{c('0', '10', '15')}
#'
#' @note The subjects are plotted row-wise, in the order in which they appear in the 'subject_id' parameter. This function is vectorized over 'subject_id', 'measure' and 'fwhm'.
#'
#' @family group visualization functions
#'
#' @export
vis.group.morph.standard <- function(subjects_dir, subject_id, measure, fwhm = "10", view_angles = 'sd_dorsal', output_img='fsbrain_group_morph.png', num_per_row = 5L, captions = subject_id, rglactions = list('no_vis'=TRUE), ...) {
    num_plots = max(length(subject_id), length(measure),  length(fwhm));
    if(num_plots > length(subject_id)) {
        subject_id = recycle(subject_id, num_plots);
        captions = subject_id;
    }
    subjects_dir = recycle(subjects_dir, num_plots);
    fwhm = recycle(fwhm, num_plots);
    measure = recycle(measure, num_plots);
    coloredmeshes = list();
    for(plot_idx in seq.int(num_plots)) {
        coloredmeshes[[plot_idx]] = vis.subject.morph.standard(subjects_dir[plot_idx], subject_id[plot_idx], measure[plot_idx], fwhm=fwhm[plot_idx], views='si', rglactions = rglactions, ...);
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
