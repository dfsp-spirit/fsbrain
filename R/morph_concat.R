#' @title Concatenate native space data for a group of subjects.
#'
#' @description A measure is something like 'thickness' or 'area'. This function concatenates the native space data for all subjects into a single long vector for each measure. A dataframe is then created, in which each column is one such vector. This can be used to compute the correlation between measures on vertex level, for example.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measures, vector of strings. Names of the vertex-wise morhometry measures. E.g., c("area", "thickness"). Used to construct the names of the morphometry file to be loaded. The data of each measure will be one column in the resulting dataframe.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param cortex_only logical, whether to set non-cortex data to NA
#'
#' @return dataframe with concatenated vertex values. Each column contains the values for one measure, concatenated for all subjects. WARNING: This dataframe can get large if you have many subjects.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c('subject1', 'subject2');
#'    cm = group.concat.measures.native(subjects_dir, subjects_list,
#'     c("thickness", "area"), "lh");
#' }
#'
#' @family concatination functions
#'
#' @export
group.concat.measures.native <- function(subjects_dir, subjects_list, measures, hemi, cortex_only = FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    all_measures_data = list();
    all_measures_data_length_per_subject = list();
    for (measure in measures) {
        measure_data_all_subjects = c();
        measure_data_length_per_subject = c();

        for (subject_id in subjects_list) {
            measure_data = subject.morph.native(subjects_dir, subject_id, measure, hemi, cortex_only = cortex_only);
            measure_data_all_subjects = append(measure_data_all_subjects, measure_data);
            measure_data_length_per_subject = append(measure_data_length_per_subject, length(measure_data));
        }

        all_measures_data[[measure]] = measure_data_all_subjects;
        all_measures_data_length_per_subject[[measure]] = measure_data_length_per_subject;
    }

    # Check whether data for all measures has same length
    for(subject_idx in 1:length(subjects_list)) {
        subject_id = subjects_list[subject_idx];
        subject_data_len = -1;
        for(measure_idx in 1:length(measures)) {
            measure = measures[measure_idx];
            measure_length_this_subject = all_measures_data_length_per_subject[[measure]][subject_idx];
            if(measure_idx == 1) {
                subject_data_len = measure_length_this_subject;    # The first measure is used as a baseline to compare against all others.
            } else {
                if(subject_data_len != measure_length_this_subject) {
                    cat(sprintf("ERROR: Measure '%s' has %d values for subject %s, but expected %d from measure '%s'.\n", measure, measure_length_this_subject, subject_id, subject_data_len, measures[1]));
                }
            }
        }
    }

    return(as.data.frame(all_measures_data));
}


#' @title Concatenate standard space data for a group of subjects.
#'
#' @description A measure is something like 'thickness' or 'area'. This function concatenates the standard space data for all subjects into a single long vector for each measure. A dataframe is then created, in which each column is one such vector. This can be used to compute the correlation between measures on vertex level, for example.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measures, vector of strings. Names of the vertex-wise morhometry measures. E.g., c("area", "thickness"). Used to construct the names of the morphometry file to be loaded. The data of each measure will be one column in the resulting dataframe.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param fwhm_per_measure, vector of strings. The fwhm settings to use, for each measure. If this is a string instead of a vector of strings, the same fwhm will be used for all measures.
#'
#' @param cortex_only logical, whether to set non-cortex data to NA
#'
#' @return dataframe with concatenated vertex values. Each column contains the values for one measure, concatenated for all subjects. The column names are a concatination of the measure, "_fwhm", and the fwhm for that measure. WARNING: This dataframe can get large if you have many subjects.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c('subject1', 'subject2');
#'    cm = group.concat.measures.standard(subjects_dir, subjects_list,
#'     c("thickness", "area"), "lh", "10");
#' }
#'
#' @family concatination functions
#'
#' @export
group.concat.measures.standard <- function(subjects_dir, subjects_list, measures, hemi, fwhm_per_measure, cortex_only = FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(length(fwhm_per_measure) == 1) {
        fwhm_per_measure = as.vector(rep(fwhm_per_measure, length(measures)));
    }

    all_measures_data = list();
    for (measure_idx in 1:length(measures)) {
        measure_data_all_subjects = c();
        fwhm = fwhm_per_measure[measure_idx];
        measure = measures[measure_idx];

        for (subject_id in subjects_list) {
            measure_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, cortex_only=cortex_only);
            measure_data_all_subjects = append(measure_data_all_subjects, measure_data);
        }

        all_measures_data[[measure]] = measure_data_all_subjects;
    }


    dfm = as.data.frame(all_measures_data);
    measure_fwhm_names = measures;  # copy for initialization only
    for (measure_idx in 1:length(measures)) {
        measure_fwhm_names[measure_idx] = paste(measures[measure_idx], '_fwhm', fwhm_per_measure[measure_idx], sep='');
    }
    colnames(dfm) <- measure_fwhm_names;
    return(dfm);
}
