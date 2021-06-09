# Functions for accessing FreeSurfer subject data, with knowledge on the directory structure.
# This is an abstraction layer over the freesurferformats package on subject level.


#' @title Retrieve native space morphometry data for a single subject.
#'
#' @description Load native space morphometry data (like 'surf/lh.area') for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param measure string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param format string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the subject. Defaults to FALSE.
#'
#' @param split_by_hemi logical, whether the returned data should be encapsulated in a named list, where the names are from 'lh' and 'rh', and the values are the respective data.
#'
#' @return vector with native space morph data, as returned by \code{\link[freesurferformats]{read.fs.morph}}.
#'
#' @family morphometry data functions
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'
#'    # Load the full data:
#'    thickness_lh = subject.morph.native(subjects_dir, "subject1", "thickness", "lh");
#'    mean(thickness_lh);  # prints 2.437466
#'
#'    # Load the data again, but this time exclude the medial wall:
#'    thickness_lh_cortex = subject.morph.native(subjects_dir, "subject1", "thickness",
#'     "lh", cortex_only=TRUE);
#'    mean(thickness_lh_cortex, na.rm=TRUE);     # prints 2.544132
#'    vis.data.on.subject(subjects_dir, "subject1", thickness_lh_cortex, NULL);
#' }
#'
#' @export
subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, format='curv', cortex_only=FALSE, split_by_hemi=FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, "lh", error_if_nonexistent=TRUE);
        lh_morph_data = freesurferformats::read.fs.morph(lh_curvfile);
        if(cortex_only) {
            lh_morph_data = apply.label.to.morphdata(lh_morph_data, subjects_dir, subject_id, "lh", 'cortex');
        }

        rh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, "rh", error_if_nonexistent=TRUE);
        rh_morph_data = freesurferformats::read.fs.morph(rh_curvfile);
        if(cortex_only) {
            rh_morph_data = apply.label.to.morphdata(rh_morph_data, subjects_dir, subject_id, "rh", 'cortex');
        }

        if(split_by_hemi) {
            merged_morph_data = list("lh"=lh_morph_data, "rh"=rh_morph_data);
        } else {
            merged_morph_data = c(lh_morph_data, rh_morph_data);
        }
        return(merged_morph_data);

    } else {
        curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, hemi, error_if_nonexistent=TRUE);
        morph_data = freesurferformats::read.fs.morph(curvfile);
        if(cortex_only) {
            morph_data = apply.label.to.morphdata(morph_data, subjects_dir, subject_id, hemi, 'cortex');
        }

        if(split_by_hemi) {
            return(hemilist.wrap(morph_data, hemi));
        } else {
            return(morph_data);
        }
    }
}


#' @title Compute a mask for a subject.
#'
#' @description Compute a binary vertex mask for the surface vertices of a subject. By defaults, the medial wall is masked.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param hemi string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param from_label string, the label file to use. Defaults to 'cortex', which will result in a mask of the medial wall versus cortex vertices.
#'
#' @param surf_num_verts string or integer. If an integer, interpreted as the number of vertices in the respective surface (lh or rh). If a character string, interpreted as a surface name, (e.g.,`white` or `pial`), and the respective surface will be loaded to determine the number of vertices in it. If parameter `hemi` is set to `both` and you supply the vertex count as an integer, this can be a vector of length 2 if the surfaces have different vertex counts (the first entry for `lh`, the second for `rh`).
#'
#' @param invert_mask logical, whether to invert the mask. E.g., when the mask is loaded from the cortex labels, if this is set to FALSE, the cortex would be masked (set to 0 in the final mask). If you want **everything but the cortex** to be masked (set to 0), you should set this to `TRUE`. Defaults to `TRUE`.
#'
#' @return the mask, a logical vector with the length of the vertices in the surface. If parameter `hemi` is set to `both`, a named list with entries `lh` and `rh` is returned, and the values of are the respective masks.
#'
#' @family label functions
#'
#' @examples
#' \donttest{
#'    # Generate a binary mask of the medial wall. Wall vertices will
#'    #  be set to 0, cortex vertices will be set to 1.
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    mask = subject.mask(subjects_dir, "subject1");
#'    # Print some information on the mask:
#'    #cat(sprintf("lh: %d verts, %d in cortex, %d medial wall.\n", length(mask$lh),
#'    # sum(mask$lh), (length(mask$lh)- sum(mask$lh))))
#'    # Output: lh: 149244 verts, 140891 in cortex, 8353 medial wall.
#'    # Now visualize the mask to illustrate that it is correct:
#'    vis.mask.on.subject(subjects_dir, "subject1", mask$lh, mask$rh);
#' }
#'
#' @export
subject.mask <- function(subjects_dir, subject_id, hemi="both", from_label="cortex", surf_num_verts="white", invert_mask = TRUE) {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        labels = subject.label(subjects_dir, subject_id, from_label, hemi);

        if(is.numeric(surf_num_verts) & length(surf_num_verts) == 1) {
            surf_num_verts = rep(surf_num_verts, 2L);
        }

        if(is.character(surf_num_verts)) { # Load the surfaces and retrieve vertex count.
            if(length(surf_num_verts) > 1) {
                surf_num_verts = surf_num_verts[1];
                warning(sprintf("Using first entry '%s' of parameter 'surf_num_verts' for both hemispheres. Ignoring others.\n", surf_num_verts));
            }
            surfaces = subject.surface(subjects_dir, subject_id, surf_num_verts, hemi);
            surf_num_verts = c(nrow(surfaces$lh$vertices), nrow(surfaces$rh$vertices));
        }

        ret_list = list();
        ret_list$lh = mask.from.labeldata.for.hemi(labels$lh, surf_num_verts[1], invert_labels = invert_mask);
        ret_list$rh = mask.from.labeldata.for.hemi(labels$rh, surf_num_verts[2], invert_labels = invert_mask);
        return(ret_list);

    } else {
        label = subject.label(subjects_dir, subject_id, from_label, hemi);

        if(length(surf_num_verts) > 1) {
            surf_num_verts = surf_num_verts[1];
            warning(sprintf("Using first entry of parameter 'surf_num_verts' only: single hemisphere requested. Ignoring others.\n"));
        }

        if(is.character(surf_num_verts)) { # Load the surface and retrieve vertex count.
            surface = subject.surface(subjects_dir, subject_id, surf_num_verts, hemi);
            surf_num_verts = nrow(surface$vertices);
        }

        return(mask.from.labeldata.for.hemi(label, surf_num_verts, invert_labels = invert_mask));
    }

}


#' @title Load a label from file and apply it to morphometry data.
#'
#' @description This function will set all values in morphdata which are *not* part of the label loaded from the file to NA (or whatever is specified by 'masked_data_value'). This is typically used to ignore values which are not part of the cortex (or any other label) during your analysis.
#'
#' @param morphdata numerical vector, the morphometry data for one hemisphere
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param hemi string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param label string, `fs.label` instance, or label vertex data. If a string, interpreted as the file name of the label file, without the hemi part (if any), optionally including the '.label' suffix. E.g., 'cortex.label' or 'cortex' for '?h.cortex.label'.
#'
#' @param masked_data_value numerical, the value to set for all morphometry data values of vertices which are *not* part of the label. Defaults to NA.
#'
#' @return numerical vector, the masked data.
#'
#' @family label functions
#' @family morphometry data functions
#'
#' @export
apply.label.to.morphdata <- function(morphdata, subjects_dir, subject_id, hemi, label, masked_data_value=NA) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(is.character(label)) {
        labeldata = subject.label(subjects_dir, subject_id, label, hemi);
    } else if (freesurferformats::is.fs.label(label)) {
        labeldata = label$vertexdata;
    } else {
        labeldata = label;
    }


    return(apply.labeldata.to.morphdata(morphdata, labeldata, masked_data_value=masked_data_value));
}


#' @title Apply a label to morphometry data.
#'
#' @description This function will set all values in morphdata which are *not* part of the labeldata to NA (or whatever is specified by 'masked_data_value'). This is typically used to ignore values which are not part of the cortex (or any other label) during your analysis.
#'
#' @param morphdata numerical vector, the morphometry data for one hemisphere
#'
#' @param labeldata integer vector or `fs.label` instance. A label as returned by \code{\link[fsbrain]{subject.label}}.
#'
#' @param masked_data_value numerical, the value to set for all morphometry data values of vertices which are *not* part of the label. Defaults to NA.
#'
#' @return numerical vector, the masked data.
#'
#' @family label functions
#' @family morphometry data functions
#'
#' @export
apply.labeldata.to.morphdata <- function(morphdata, labeldata, masked_data_value=NA) {

    if(freesurferformats::is.fs.label(labeldata)) {
        labeldata = labeldata$vertexdata;
    }

    if(max(labeldata) > length(morphdata)) {
        stop(sprintf("The largest vertex index in the labeldata is %d, but the morphdata contains only %d entries. Label does not fit to morpometry data.\n", max(labeldata), length(morphdata)));
    }

    mask = mask.from.labeldata.for.hemi(labeldata, length(morphdata));
    morphdata[mask] = masked_data_value;
    return(morphdata);
}


#' @title Retrieve standard space morphometry data for a single subject.
#'
#' @description Load standard space morphometry data (like 'surf/lh.area.fwhm10.fsaverage.mgh') for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param measure string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param fwhm string. Smoothing as string, e.g. '10' or '25'.
#'
#' @param template_subject string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the template subject. Defaults to FALSE.
#'
#' @param split_by_hemi logical, whether the returned data should be encapsulated in a named list, where the names are from 'lh' and 'rh', and the values are the respective data.
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
subject.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm='10', template_subject='fsaverage', format='mgh', cortex_only=FALSE, split_by_hemi=FALSE) {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, "lh", fwhm, template_subject = template_subject, error_if_nonexistent=TRUE);
        lh_morph_data = freesurferformats::read.fs.morph(lh_curvfile);
        if(cortex_only) {
            lh_morph_data = apply.label.to.morphdata(lh_morph_data, subjects_dir, template_subject, "lh", 'cortex');
        }

        rh_curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, "rh", fwhm, template_subject = template_subject, error_if_nonexistent=TRUE);
        rh_morph_data = freesurferformats::read.fs.morph(rh_curvfile);
        if(cortex_only) {
            rh_morph_data = apply.label.to.morphdata(rh_morph_data, subjects_dir, template_subject, "rh", 'cortex');
        }

        if(split_by_hemi) {
            merged_morph_data = list("lh"=lh_morph_data, "rh"=rh_morph_data);
        } else {
            merged_morph_data = c(lh_morph_data, rh_morph_data);
        }
        return(merged_morph_data);

    } else {
        curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm, template_subject = template_subject, error_if_nonexistent=TRUE);
        morph_data = freesurferformats::read.fs.morph(curvfile);
        if(cortex_only) {
            morph_data = apply.label.to.morphdata(morph_data, subjects_dir, template_subject, hemi, 'cortex');
        }

        if(split_by_hemi) {
            ret_list = list();
            ret_list[[hemi]] = morph_data;
            return(ret_list);
        } else {
            return(morph_data);
        }
    }
}


#' @title Construct filepath of standard space morphometry data file.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier. Can be a vector.
#'
#' @param measure string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi string, one of 'lh' or 'rh'. The hemisphere name.
#'
#' @param fwhm string. Smoothing as string, e.g. '10' or '25'. Defaults to '10'.
#'
#' @param template_subject string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @param warn_if_nonexistent logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @param error_if_nonexistent logical. Whether to raise an error if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @return string, the file path. (Or a vector if 'subject_id' is a vector.)
#'
#' @export
subject.filepath.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm='10', template_subject='fsaverage', format='auto', warn_if_nonexistent=FALSE, error_if_nonexistent=FALSE) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(length(subject_id) > 1L) {
        filenames = rep("", length(subject_id));
        for(sj_idx in seq_along(subject_id)) {
            filenames[sj_idx] = subject.filepath.morph.standard(subjects_dir, subject_id[sj_idx], measure, hemi = hemi, fwhm = fwhm,  template_subject = template_subject, format = format, warn_if_nonexistent = warn_if_nonexistent, error_if_nonexistent = error_if_nonexistent);
        }
        return(filenames);
    }

    if(! is.null(fwhm)) {
        fwhm_tag = sprintf(".fwhm%s", fwhm)
    } else {
        fwhm_tag = "" # Support opening files without any FWHM part
    }

    if(tolower(format) == 'auto') {
        curvfile_base = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s.%s", hemi, measure, fwhm_tag, template_subject));
        curvfile = freesurferformats::readable.files(curvfile_base, precedence=c('.mgh', '.mgz', ''));
    } else {
        curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s.%s%s", hemi, measure, fwhm_tag, template_subject, freesurferformats::fs.get.morph.file.ext.for.format(format)));
    }

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
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param measure string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi string, one of 'lh' or 'rh'. The hemisphere name.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.
#'
#' @param warn_if_nonexistent logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @param error_if_nonexistent logical. Whether to raise an error if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @return string, the file path.
#'
#' @export
subject.filepath.morph.native <- function(subjects_dir, subject_id, measure, hemi, format='curv', warn_if_nonexistent=FALSE, error_if_nonexistent=FALSE) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s", hemi, measure, freesurferformats::fs.get.morph.file.ext.for.format(format)));

    if(!file.exists(curvfile)) {
        msg = sprintf("Native space morphometry file '%s' for subject '%s' measure '%s' hemi '%s' cannot be accessed.\n", curvfile, subject_id, measure, hemi);
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
#' @param subjects_dir character string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id character string. The subject identifier. Can be a vector of subject identifiers.
#'
#' @param relative_path_parts vector of strings. The path to the file, e.g., c("surf", "lh.area").
#'
#' @param hemi string, one of 'lh', 'rh', or NULL. Defaults to NULL. If a hemisphere name is given, it is added as a prefix to the last entry in relative_path_parts, separated by a dot.
#'
#' @param file_tag string. A one-word description of the file type that will show up in the error message to describe the file if it is missing. Leads to a better error message. Examples: 'morphometry' or 'label'. Only relevant if warn_if_nonexistent is TRUE. Defaults to the empty string.
#'
#' @param warn_if_nonexistent, logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @return string, the file path. (Or a vector of strings if 'subject_id' is a vector).
#'
#' @examples
#' \dontrun{
#' fsbrain:::subject.filepath.any("/data/study1", "subject1",
#'   c("surf", "area"), hemi="lh");
#' fsbrain:::subject.filepath.any("/data/study1", c("subject1", "subject2"),
#'   c("surf", "area"), hemi="lh");
#' }
#'
#' @keywords internal
subject.filepath.any <- function(subjects_dir, subject_id, relative_path_parts, hemi=NULL, file_tag="", warn_if_nonexistent=FALSE) {

    if(length(subject_id) > 1L) {
        filenames = rep("", length(subject_id));
        for(sj_idx in seq_along(subject_id)) {
            filenames[sj_idx] = subject.filepath.any(subjects_dir, subject_id[sj_idx], relative_path_parts, hemi = hemi, file_tag = file_tag, warn_if_nonexistent = warn_if_nonexistent);
        }
        return(filenames);
    }

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
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param label string. Name of the label file, without the hemi part. You can include the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'. You can also pass just the label (e.g., 'cortex'): if the string does not end with the suffix '.label', that suffix gets added auomatically.
#'
#' @param hemi string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the label data files to be loaded. For 'both', see the information on the return value.
#'
#' @param return_one_based_indices logical. Whether the indices should be 1-based. Indices are stored zero-based in the file, but R uses 1-based indices. Defaults to TRUE, which means that 1 will be added to all indices read from the file before returning them.
#'
#' @param full logical, whether to return the full label structure instead of only the vertex indices.
#'
#' @return integer vector with label data: the list of vertex indices in the label. See 'return_one_based_indices' for important information. If parameter `hemi` is set to `both`, a named list with entries `lh` and `rh` is returned, and the values of are the respective labels.
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
subject.label <- function(subjects_dir, subject_id, label, hemi, return_one_based_indices=TRUE, full = FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh', or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        ret_list = list();
        ret_list$lh = subject.label(subjects_dir, subject_id, label, 'lh', return_one_based_indices=return_one_based_indices);
        ret_list$rh = subject.label(subjects_dir, subject_id, label, 'rh', return_one_based_indices=return_one_based_indices);
        return(ret_list);
    }

    if(! is.character(label)) {
        stop("Parameter 'label' must be a character string.");
    }

    if(! endsWith(label, '.label')) {
        label = paste(label, ".label", sep="");
    }

    labelfile = subject.filepath.any(subjects_dir, subject_id, c("label", label), hemi=hemi, file_tag="label", warn_if_nonexistent=TRUE);
    return(freesurferformats::read.fs.label(labelfile, return_one_based_indices=return_one_based_indices, full = full));
}

#' @title Create a binary mask from labels.
#'
#' @description Create a binary mask for the data of a single hemisphere from one or more labels. A label contains the vertex indices which are part of it, but often having a mask in more convenient.
#'
#' @param labels list of labels. A label is just a vector of vertex indices. It can be created manually, but is typically loaded from a label file using \code{\link[fsbrain]{subject.label}}.
#'
#' @param num_vertices_in_hemi integer. The number of vertices of the surface for which the mask is created. This must be for a single hemisphere.
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
#' @description Create labeldata from a mask. This function is trivial and only calls \code{\link{which}} after performing basic sanity checks.
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
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param hemi string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param atlas string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @return the annotation, as returned by \code{\link[freesurferformats]{read.fs.annot}}. It is a named list, enties are: "vertices" vector of n vertex indices, starting with 0. "label_codes": vector of n integers, each entry is a color code, i.e., a value from the 5th column in the table structure included in the "colortable" entry (see below). "label_names": the n brain structure names for the vertices, already retrieved from the colortable using the code. "hex_colors_rgb": Vector of hex color for each vertex.
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

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

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

#' @title Load a surface for a subject.
#'
#' @description Load a brain surface mesh for a subject.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier
#'
#' @param surface string. The surface name. E.g., "white", or "pial". Used to construct the name of the surface file to be loaded.
#'
#' @param hemi string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the surface file to be loaded. For 'both', see the information on the return value.
#'
#' @param force_hemilist logical, whether to return a hemilist even if the 'hemi' parameter is not set to 'both'
#'
#' @param as_tm logical, whether to return an \code{rgl::tmesh3d} instead of an \code{fs.surface} instance by applying the \code{fs.surface.to.tmesh3d} function.
#'
#' @return the `fs.surface` instance, as returned by \code{\link[freesurferformats]{read.fs.surface}}. If parameter `hemi` is set to `both`, a named list with entries `lh` and `rh` is returned, and the values of are the respective surfaces. The mesh data structure used in `fs.surface` is a *face index set*.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    lh_white = subject.surface(subjects_dir, "subject1", "white", "lh");
#' }
#'
#' @family surface mesh functions
#'
#' @export
subject.surface <- function(subjects_dir, subject_id, surface = "white", hemi = "both", force_hemilist = FALSE, as_tm = FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        ret_list = list();
        ret_list$lh = subject.surface(subjects_dir, subject_id, surface, 'lh');
        ret_list$rh = subject.surface(subjects_dir, subject_id, surface, 'rh');
        if(as_tm) {
            ret_list$lh = fs.surface.to.tmesh3d(ret_list$lh);
            ret_list$rh = fs.surface.to.tmesh3d(ret_list$rh);
        }
        return(ret_list);
    }

    surface_file = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s", hemi, surface));
    if(!file.exists(surface_file)) {
        stop(sprintf("Surface file '%s' for subject '%s' surface '%s' hemi '%s' cannot be accessed.\n", surface_file, subject_id, surface, hemi));
    }
    sf = freesurferformats::read.fs.surface(surface_file);
    if(as_tm) {
        sf = fs.surface.to.tmesh3d(sf);
    }
    if(force_hemilist) {
        return(hemilist.wrap(sf, hemi));
    } else {
        return(sf);
    }
}


#' @title Load labels representing brain lobes.
#'
#' @description This gives you labels that represent brain lobes for a subject. The lobe definition is based on the Desikan-Killiany atlas (Desikan *et al.*, 2010) as suggested on the FreeSurfer website at https://surfer.nmr.mgh.harvard.edu/fswiki/CorticalParcellation.
#'
#' @inheritParams subject.surface
#'
#' @param include_cingulate logical, whether to include the vertices of the cingulate in the lobes
#'
#' @param as_annot return a hemilist of annotations instead of the return value described in the *value* section
#'
#' @return hemilist of integer vectors, the vectors represent vertex indices of the hemispheres, and each vertex is assigned one of the following values: `0`=no_lobe, `1`=frontal, `2`=parietal, `3`=temporal, `4`=occipital.
#'
#' @family atlas functions
#' @family label functions
#'
#' @export
subject.lobes <- function(subjects_dir, subject_id, hemi='both', include_cingulate=TRUE, as_annot=FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    ret_list = list();

    if(hemi %in% c("lh", "both")) {
        ret_list$lh = hemi.lobe.labels(subjects_dir, subject_id, 'lh', include_cingulate=include_cingulate, as_annot=as_annot);
    }
    if(hemi %in% c("rh", "both")) {
        ret_list$rh = hemi.lobe.labels(subjects_dir, subject_id, 'rh', include_cingulate=include_cingulate, as_annot=as_annot);
    }
    return(ret_list);
}


#' @title Compute lobe labels for a single hemi from aparc atlas.
#'
#' @inheritParams subject.lobes
#'
#' @param hemi string, one of 'lh' or 'rh'. The hemisphere name.
#'
#' @note See \code{\link[fsbrain]{subject.lobes}} for details.
#'
#' @keywords internal
hemi.lobe.labels <- function(subjects_dir, subject_id, hemi, include_cingulate=TRUE, as_annot=FALSE) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    atlas = 'aparc';
    annot = subject.annot(subjects_dir, subject_id, hemi, atlas);

    frontal_lobe_regions = c('superiorfrontal', 'rostralmiddlefrontal', 'caudalmiddlefrontal' , 'parstriangularis', 'parsopercularis', 'parsorbitalis', 'lateralorbitofrontal', 'medialorbitofrontal', 'paracentral', 'precentral', 'frontalpole');
    parietal_lobe_regions = c('superiorparietal', 'inferiorparietal', 'supramarginal', 'postcentral', 'precuneus');
    temporal_lobe_regions = c('superiortemporal', 'inferiortemporal', 'middletemporal', 'bankssts', 'fusiform', 'transversetemporal', 'entorhinal', 'parahippocampal', 'temporalpole');
    occipital_lobe_regions = c('lateraloccipital', 'lingual', 'cuneus', 'pericalcarine');

    if(include_cingulate) {
        frontal_lobe_regions = c(frontal_lobe_regions, 'caudalanteriorcingulate', 'rostralanteriorcingulate');
        parietal_lobe_regions = c(parietal_lobe_regions, 'posteriorcingulate', 'isthmuscingulate');
    }

    lobe_indices = rep(0L, length(annot$vertices));
    lobe_indices[annot$label_names %in% frontal_lobe_regions] = 1L;
    lobe_indices[annot$label_names %in% parietal_lobe_regions] = 2L;
    lobe_indices[annot$label_names %in% temporal_lobe_regions] = 3L;
    lobe_indices[annot$label_names %in% occipital_lobe_regions] = 4L;

    if(as_annot) {
        num_vertices_in_surface = length(annot$vertices);
        label_vertices_by_region = list('unknown2'=which(lobe_indices==0L), 'frontal'=which(lobe_indices==1L), 'parietal'=which(lobe_indices==2L), 'temporal'=which(lobe_indices==3L), 'occipetal'=which(lobe_indices==4L));
        output_annot = label.to.annot(label_vertices_by_region, num_vertices_in_surface);
        return(output_annot);
    } else {
        return(lobe_indices);
    }
}


#' @title Get subjects vertex count.
#'
#' @description Determine vertex counts for the brain meshes of a subject.
#'
#' @inheritParams subject.surface
#'
#' @param do_sum logical, whether to return the sum of the vertex counts for lh and rh. Ignored unless 'hemi' is 'both'. If set, a single scalar will be returned.
#'
#' @return hemilist of integers, the vertex count. If hemi is 'both' and 'do_sum' is `FALSE`, a hemilist of integers is returned. Otherwise, a single integer.
#'
#' @export
subject.num.verts <- function(subjects_dir, subject_id, surface='white', hemi='both', do_sum=FALSE) {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }
    sf = subject.surface(subjects_dir, subject_id, surface=surface, hemi=hemi);

    if(hemi == 'both') {
        if(do_sum) {
            return(nrow(sf$lh$vertices) + nrow(sf$rh$vertices));
        }
        return(list('lh'=nrow(sf$lh$vertices), 'rh'=nrow(sf$rh$vertices)));
    }
    return(nrow(sf$vertices));
}



