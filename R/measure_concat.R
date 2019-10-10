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
#' @return dataframe with concatenated vertex values. Each column contains the values for one measure, concatenated for all subjects. WARNING: This dataframe can get large if you have many subjects.
#'
#'
#' @export
concat_measures_native <- function(subjects_dir, subjects_list, measures, hemi) {
    all_measures_data = list();
    for (measure in measures) {
        measure_data_all_subjects = c();

        for (subject_id in subjects_list) {
            measure_data = subject.morph.native(subjects_dir, subject_id, measure, hemi);
            measure_data_all_subjects = append(measure_data_all_subjects, measure_data);
        }

        all_measures_data[[measure]] = measure_data_all_subjects;
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
#' @return dataframe with concatenated vertex values. Each column contains the values for one measure, concatenated for all subjects. The column names are a concatination of the measure, "_fwhm", and the fwhm for that measure. WARNING: This dataframe can get large if you have many subjects.
#'
#'
#' @export
concat_measures_standard <- function(subjects_dir, subjects_list, measures, hemi, fwhm_per_measure) {
    if(length(fwhm_per_measure) == 1) {
        fwhm_per_measure = as.vector(rep(fwhm_per_measure, length(measures)));
    }

    all_measures_data = list();
    for (measure_idx in 1:length(measures)) {
        measure_data_all_subjects = c();
        fwhm = fwhm_per_measure[measure_idx];
        measure = measures[measure_idx];

        for (subject_id in subjects_list) {
            measure_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm);
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
