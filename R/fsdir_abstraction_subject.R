# Functions for accessing FreeSurfer subject data, with knowledge on the directory structure.
# This is an abstraction layer over the freesurferformats package on subject level.


#' @title Retrieve native space morphometry data for a single subject.
#'
#' @description Load native space morphometry data (like 'surf/lh.area') for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.
#'
#' @return vector with native space morph data
#'
#' @family morphometry data functions
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    thickness_lh = subject.morph.native(subjects_dir, "subject1", "thickness", "lh");
#' }
#'
#' @export
subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, format='curv') {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, "lh", error_if_nonexistent=TRUE);
        lh_morph_data = freesurferformats::read.fs.morph(lh_curvfile);

        rh_curvfile = lh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, "rh", error_if_nonexistent=TRUE);
        rh_morph_data = freesurferformats::read.fs.morph(rh_curvfile);

        merged_morph_data = c(lh_morph_data, rh_morph_data);
        return(merged_morph_data);

    } else {
        curvfile = lh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, hemi, error_if_nonexistent=TRUE);
        return(freesurferformats::read.fs.morph(curvfile));
    }
}


#' @title Retrieve standard space morphometry data for a single subject.
#'
#' @description Load standard space morphometry data (like 'surf/lh.area.fwhm10.fsaverage.mgh') for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param fwhm, string. Smoothing as string, e.g. '10' or '25'.
#'
#' @param template_subject, string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @return vector with standard space morph data
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    thickness_lh = subject.morph.standard(subjects_dir, "subject1", "thickness", "lh", fwhm='10');
#' }
#'
#' @family morphometry data functions
#'
#' @export
subject.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm='10', template_subject='fsaverage', format='mgh') {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, "lh", fwhm, error_if_nonexistent=TRUE);
        lh_morph_data = freesurferformats::read.fs.morph(lh_curvfile);

        rh_curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, "rh", fwhm, error_if_nonexistent=TRUE);
        rh_morph_data = freesurferformats::read.fs.morph(rh_curvfile);

        merged_morph_data = c(lh_morph_data, rh_morph_data);
        return(merged_morph_data);

    } else {
        curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm, error_if_nonexistent=TRUE);
        return(freesurferformats::read.fs.morph(curvfile));
    }
}


#' @title Construct filepath of standard space morphometry data file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name.
#'
#' @param fwhm, string. Smoothing as string, e.g. '10' or '25'. Defaults to '10'.
#'
#' @param template_subject, string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @param warn_if_nonexistent, logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @param error_if_nonexistent, logical. Whether to raise an error if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @return string, the file path.
#'
#' @examples
#'    filepath_lh_thickness_std =
#'    subject.filepath.morph.standard("/media/ext/data/study1", "subject1",
#'     "thickness", "lh", fwhm="25");
#'
#' @export
subject.filepath.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm='10', template_subject='fsaverage', format='mgh', warn_if_nonexistent=FALSE, error_if_nonexistent=FALSE) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(nchar(fwhm) > 0) {
        fwhm_tag = sprintf(".fwhm%s", fwhm)
    } else {
        fwhm_tag = "" # Support opening files without any FWHM part
    }

    curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s.%s%s", hemi, measure, fwhm_tag, template_subject, freesurferformats::fs.get.morph.file.ext.for.format(format)));

    if(!file.exists(curvfile)) {
        msg = sprintf("Standard space morphometry file '%s' for subject '%s' measure '%s' hemi '%s' fwhm '%s' cannot be accessed.\n", curvfile, subject_id, measure, hemi, fwhm);
        if((warn_if_nonexistent)) {
            warning(msg);
        }
        if((error_if_nonexistent)) {
            stop(msg);
        }
    }

    return(curvfile);
}


#' @title Construct filepath of native space morphometry data file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.
#'
#' @param warn_if_nonexistent, logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @param error_if_nonexistent, logical. Whether to raise an error if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @return string, the file path.
#'
#' @examples
#'    filepath_lh_thickness =
#'    subject.filepath.morph.native("/media/ext/data/study1", "subject1", "thickness", "lh");
#'
#' @export
subject.filepath.morph.native <- function(subjects_dir, subject_id, measure, hemi, format='curv', warn_if_nonexistent=FALSE, error_if_nonexistent=FALSE) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s", hemi, measure, freesurferformats::fs.get.morph.file.ext.for.format(format)));

    if(!file.exists(curvfile)) {
        msg = sprintf("Standard space morphometry file '%s' for subject '%s' measure '%s' hemi '%s' cannot be accessed.\n", curvfile, subject_id, measure, hemi);
        if((warn_if_nonexistent)) {
            warning(msg);
        }
        if((error_if_nonexistent)) {
            stop(msg);
        }
    }

    return(curvfile);
}



#' @title Construct filepath of any freesurfer file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param relative_path_parts, vector of strings. THe path to the file, e.g., c("surf", "lh.area").
#'
#' @param hemi, string, one of 'lh', 'rh', or NULL. Defaults to NULL. If a hemisphere name is given, it is added as a prefix to the last entry in relative_path_parts, separated by a dot.
#'
#' @param file_tag, string. A one-word description of the file type that will show up in the error message to describe the file if it is missing. Leads to a better error message. Examples: 'morphometry' or 'label'. Only relevant if warn_if_nonexistent is TRUE. Defaults to the empty string.
#'
#' @param warn_if_nonexistent, logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#'
#' @return string, the file path.
#'
#' @keywords internal
subject.filepath.any <- function(subjects_dir, subject_id, relative_path_parts, hemi=NULL, file_tag="", warn_if_nonexistent=FALSE) {

    if(nchar(file_tag) > 0) {
        file_tag = sprintf("%s ", file_tag);    # Just for the format of the warning message: avoid missing/duplicate space.
    }

    if( ! is.null(hemi)) {
        if(!(hemi %in% c("lh", "rh"))) {
            stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or NULL if given, but is '%s'.\n", hemi));
        }
        hemi_prefix = sprintf("%s.", hemi);
        # Add hemi prefix to last part, the file name.
        last_part_mod = sprintf("%s%s", hemi_prefix, relative_path_parts[length(relative_path_parts)]);
        relative_path_parts[length(relative_path_parts)] = last_part_mod;
    }

    rel_part = do.call(file.path, as.list(relative_path_parts));
    somefile = file.path(subjects_dir, subject_id, rel_part);

    if((!file.exists(somefile)) && (warn_if_nonexistent) ) {
        if(is.null(hemi)) {
            warning(sprintf("The %sfile '%s' for subject '%s' cannot be accessed.\n", file_tag, somefile, subject_id));
        } else {
            warning(sprintf("The %sfile '%s' for subject '%s' hemi '%s' cannot be accessed.\n", file_tag, somefile, subject_id, hemi));
        }
    }

    return(somefile);
}


#' @title Retrieve label data for a single subject.
#'
#' @description Load a label (like 'label/lh.cortex.label') for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param label, string. Name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param return_one_based_indices, logical. Whether the indices should be 1-based. Indices are stored zero-based in the file, but R uses 1-based indices. Defaults to TRUE, which means that 1 will be added to all indices read from the file before returning them.
#'
#' @return integer vector with label data: the list of vertex indices in the label. See 'return_one_based_indices' for important information.
#'
#' @family label data functions
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    cortex_lh = subject.label(subjects_dir, "subject1", "cortex.label", "lh");
#' }
#'
#' @export
subject.label <- function(subjects_dir, subject_id, label, hemi, return_one_based_indices=TRUE) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    labelfile = subject.filepath.any(subjects_dir, subject_id, c("label", label), hemi=hemi, file_tag="label", warn_if_nonexistent=TRUE);
    return(freesurferformats::read.fs.label(labelfile, return_one_based_indices=return_one_based_indices));
}

#' @title Create a binary mask from labels.
#'
#' @description Create a binary mask for the data of a single hemisphere from one or more labels. A label contains the vertex indices which are part of it, but often having a mask in more convenient.
#'
#' @param labels, list of labels. A label is just a vector of vertex indices. It can be created manually, but is typically loaded from a label file using [fsbrain::subject.label].
#'
#' @param num_vertices_in_hemi, integer. The number of vertices of the surface for which the mask is created. This must be for a single hemisphere.
#'
#' @param invert_labels logical, whether to invert the label data.
#'
#' @param existing_mask an existing mask to modify or NULL. If it is NULL, a new mask will be created before applying any labels, and the values set during initialization of this new mask are the negation of the 'invert_label' parameter. Defaults to NULL.
#'
#' @return logical vector. The mask. It contains a logical value for each vertex. By default, the vertex indices from the labels are FALSE and the rest are TRUE, but this can be changed with the parameter 'invert_labels'.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'
#'   # Define the data to use:
#'   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'   subject_id = 'subject1';
#'   surface = 'white';
#'   hemi = 'both';
#'   atlas = 'aparc';
#'   region = 'bankssts';
#'
#'   # Create a mask from a region of an annotation:
#'   lh_annot = subject.annot(subjects_dir, subject_id, 'lh', atlas);
#'   rh_annot = subject.annot(subjects_dir, subject_id, 'rh', atlas);
#'   lh_label = label.from.annotdata(lh_annot, region);
#'   rh_label = label.from.annotdata(rh_annot, region);
#'   lh_mask = mask.from.labeldata.for.hemi(lh_label, length(lh_annot$vertices));
#'   rh_mask = mask.from.labeldata.for.hemi(rh_label, length(rh_annot$vertices));
#'
#'   # Edit the mask: add the vertices from another region to it:
#'   region2 = 'medialorbitofrontal';
#'   lh_label2 = label.from.annotdata(lh_annot, region2);
#'   rh_label2 = label.from.annotdata(rh_annot, region2);
#'   lh_mask2 = mask.from.labeldata.for.hemi(lh_label2, length(lh_annot$vertices),
#'    existing_mask = lh_mask);
#'   rh_mask2 = mask.from.labeldata.for.hemi(rh_label2, length(rh_annot$vertices),
#'    existing_mask = rh_mask);
#' }
#'
#' @family label data functions
#' @family mask functions
#'
#' @export
mask.from.labeldata.for.hemi <- function(labels, num_vertices_in_hemi, invert_labels=FALSE, existing_mask=NULL) {
    if(is.null(existing_mask)) {
        mask = rep(!invert_labels, num_vertices_in_hemi);
    } else {
        if(! is.logical(existing_mask)) {
            stop("Parameter 'existing_mask' must be logical vector if given.");
        }
        if(length(existing_mask) != num_vertices_in_hemi) {
            stop(sprintf("The mask supplied in parameter 'existing_mask' has %d entries but parameter 'num_vertices_in_hemi' is %d. Numbers must match.\n", length(existing_mask), num_vertices_in_hemi));
        }
        mask = existing_mask;
    }
    for(label_idx in seq_len(length(labels))) {
        label = unlist(labels[label_idx]);
        if(max(label) > num_vertices_in_hemi) {
            stop(sprintf("Label #%d contains vertex index %d, but parameter 'num_vertices_in_hemi' is only %d. Vertex index must no exceed vertex count.\n", label_idx, max(label), num_vertices_in_hemi));
        }
        mask[label] = invert_labels;
    }
    return(mask);
}

#' @title Create labeldata from a mask.
#'
#' @description Create labeldata from a mask. This function is trivial and only calls [base::which] after performing basic sanity checks.
#'
#' @param mask a logical vector
#'
#' @param invert Whether to report the inverse the mask before determining the indices. Defaults to FALSE.
#'
#' @return labeldata. The list of indices which are TRUE in the mask (or the ones which FALSE if 'invert' is TRUE).
#'
#' @family label data functions
#'
#' @export
labeldata.from.mask <- function(mask, invert=FALSE) {
    if(! is.logical(mask)) {
        stop("Parameter 'mask' must be logical vector.");
    }
    return(which(mask != invert));
}


#'@title Load an annotation for a subject.
#'
#' @description Load a brain surface annotation, i.e., a cortical parcellation based on an atlas, for a subject.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @return the annotation, as returned by [freesurferformats::read.fs.annot()]. It is a named list, enties are: "vertices" vector of n vertex indices, starting with 0. "label_codes": vector of n integers, each entry is a color code, i.e., a value from the 5th column in the table structure included in the "colortable" entry (see below). "label_names": the n brain structure names for the vertices, already retrieved from the colortable using the code. "hex_colors_rgb": Vector of hex color for each vertex.
#'      The "colortable" is another named list with 3 entries: "num_entries": int, number of brain structures. "struct_names": vector of strings, the brain structure names. "table": numeric matrix with num_entries rows and 5 colums. The 5 columns are: 1 = color red channel, 2=color blue channel, 3=color green channel, 4=color alpha channel, 5=unique color code. "colortable_df": The same information as a dataframe. Contains the extra columns "hex_color_string_rgb" and "hex_color_string_rgba" that hold the color as an RGB(A) hex string, like "#rrggbbaa".
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    annot_lh = subject.annot(subjects_dir, "subject1", "lh", "aparc");
#' }
#'
#' @family atlas functions
#'
#' @export
subject.annot <- function(subjects_dir, subject_id, hemi, atlas) {
    if(hemi == "both") {
        lh_annot_file = file.path(subjects_dir, subject_id, "label", sprintf("%s.%s.annot", "lh", atlas));
        if(!file.exists(lh_annot_file)) {
            stop(sprintf("Annotation lh file '%s' for subject '%s' atlas '%s' hemi '%s' cannot be accessed.\n", lh_annot_file, subject_id, atlas, "lh"));
        }
        lh_annot = freesurferformats::read.fs.annot(lh_annot_file);

        rh_annot_file = file.path(subjects_dir, subject_id, "label", sprintf("%s.%s.annot", "rh", atlas));
        if(!file.exists(rh_annot_file)) {
            stop(sprintf("Annotation rh file '%s' for subject '%s' atlas '%s' hemi '%s' cannot be accessed.\n", rh_annot_file, subject_id, atlas, "rh"));
        }
        rh_annot = freesurferformats::read.fs.annot(rh_annot_file);

        merged_annot = merge.hemi.annots(lh_annot, rh_annot);
        return(merged_annot);
    }
    else {
        annot_file = file.path(subjects_dir, subject_id, "label", sprintf("%s.%s.annot", hemi, atlas));
        if(!file.exists(annot_file)) {
            stop(sprintf("Annotation file '%s' for subject '%s' atlas '%s' hemi '%s' cannot be accessed.\n", annot_file, subject_id, atlas, hemi));
        }
        return(freesurferformats::read.fs.annot(annot_file));
    }
}

#'@title Load a surface for a subject.
#'
#' @description Load a brain surface for a subject.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param surface, string. The surface name. E.g., "white", or "pial". Used to construct the name of the surface file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the surface file to be loaded.
#'
#' @return the surface, as returned by freesurferformats::read.fs.surface(). A named list containing entries 'vertices' and 'faces'.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    lh_white = subject.surface(subjects_dir, "subject1", "white", "lh");
#' }
#'
#' @export
subject.surface <- function(subjects_dir, subject_id, surface, hemi) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    surface_file = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s", hemi, surface));
    if(!file.exists(surface_file)) {
        stop(sprintf("Surface file '%s' for subject '%s' surface '%s' hemi '%s' cannot be accessed.\n", surface_file, subject_id, surface, hemi));
    }
    return(freesurferformats::read.fs.surface(surface_file));
}



