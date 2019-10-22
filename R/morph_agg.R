

#' @title Aggregate native space morphometry data over one hemisphere for a group of subjects.
#'
#' @description Compute the mean (or other aggregates) over all vertices of a subject from native space morphometry data (like 'surf/lh.area'). Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param agg_fun, function. An R function that aggregates data, typically max, mean, min or something similar. Note: this is NOT a string, put the function name without quotes. Defaults to mean.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.
#'
#' @param cast, logical. Whether the format of the returned data frame should be cast, i.e., whether a separate 'hemi' column should be introduced. If this is set to FALSE, the following will be returned: a dataframe with 2 columns and n rows, where n is the number of subjects. The 2 columns are 'subject_id' and '<hemi>.<measure>' (e.g., "lh.thickness"), the latter contains the aggregated data. See the description of the return value for the default case (cast=TRUE). Defaults to TRUE.
#'
#' @return dataframe with aggregated values for all subjects, with 3 columns and n rows, where n is the number of subjects. The 3 columns are 'subject_id', 'hemi', and '<measure>' (e.g., "thickness"), the latter contains the aggregated data.
#'
#' @family global aggregation functions
#'
#' @export
group.morph.agg.native <- function(subjects_dir, subjects_list, measure, hemi, agg_fun = mean, cast=TRUE, format='curv') {
  if(!(hemi %in% c("lh", "rh", "both"))) {
    stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both but is '%s'.\n", hemi));
  }
  agg_all_subjects = data.frame();
  for (subject_id in subjects_list) {
      morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi, format=format);
      if(nrow(agg_all_subjects) == 0) {
        if(cast) {
          agg_all_subjects = data.frame(c(as.character(subject_id)), hemi, measure, agg_fun(morph_data), stringsAsFactors = FALSE);
        } else {
          agg_all_subjects = data.frame(c(as.character(subject_id)), agg_fun(morph_data), stringsAsFactors = FALSE);
        }
      } else {
        if(cast) {
          agg_all_subjects = rbind(agg_all_subjects, c(as.character(subject_id), hemi, measure, agg_fun(morph_data)));
        } else {
          agg_all_subjects = rbind(agg_all_subjects, c(as.character(subject_id), agg_fun(morph_data)));
        }
      }
  }

  if(cast){
    value_column_name = measure;
    colnames(agg_all_subjects) = c("subject_id", "hemi", "measure_name", "measure_value");
    agg_all_subjects$measure_value = as.numeric(agg_all_subjects$measure_value);
  } else {
    value_column_name = sprintf("%s.%s", hemi, measure);
    colnames(agg_all_subjects) = c("subject_id", value_column_name);
    agg_all_subjects[[value_column_name]] = as.numeric(agg_all_subjects[[value_column_name]]);
  }

  return(agg_all_subjects);
}




#' @title Aggregate standard space (fsaverage) morphometry data over one hemisphere for a group of subjects.
#'
#' @description Compute the mean (or other aggregates) over all vertices of a subject from standard space morphometry data (like 'surf/lh.area.fwhm10.fsaverage.mgh'). Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param fwhm, string. Smoothing as string, e.g. '10' or '25'.
#'
#' @param agg_fun, function. An R function that aggregates data, typically max, mean, min or something similar. Note: this is NOT a string, put the function name without quotes. Defaults to mean.
#'
#' @param template_subject, string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @param cast, logical. Whether the columns should be database style, i.e., separate columns for everything.
#'
#' @return dataframe with aggregated values for all subjects, with 2 columns and n rows, where n is the number of subjects. The 2 columns are 'subject_id' and '<hemi>.<measure>' (e.g., "lh.thickness"), the latter contains the aggregated data.
#'
#' @family global aggregation functions
#'
#' @export
group.morph.agg.standard <- function(subjects_dir, subjects_list, measure, hemi, fwhm, agg_fun = mean, template_subject='fsaverage', format='mgh', cast=TRUE) {
  if(!(hemi %in% c("lh", "rh", "both"))) {
    stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both but is '%s'.\n", hemi));
  }
  agg_all_subjects = data.frame();
  for (subject_id in subjects_list) {
    morph_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, template_subject=template_subject, format=format);

    if(nrow(agg_all_subjects) == 0) {
      if(cast) {
        agg_all_subjects = data.frame(c(as.character(subject_id)), hemi, measure, as.numeric(agg_fun(morph_data)), stringsAsFactors = FALSE);
      } else {
        agg_all_subjects = data.frame(c(as.character(subject_id)), as.numeric(agg_fun(morph_data)), stringsAsFactors = FALSE);
      }
    } else {
      if(cast) {
        agg_all_subjects = rbind(agg_all_subjects, c(as.character(subject_id), hemi, measure, as.numeric(agg_fun(morph_data))));
      } else {
        agg_all_subjects = rbind(agg_all_subjects, c(as.character(subject_id), as.numeric(agg_fun(morph_data))));
      }
    }
  }

  if(cast){
    colnames(agg_all_subjects) = c("subject_id", "hemi", "measure_name", "measure_value");
    agg_all_subjects$measure_value = as.numeric(agg_all_subjects$measure_value);
  } else {
    value_column_name = sprintf("%s.%s", hemi, measure);
    colnames(agg_all_subjects) = c("subject_id", value_column_name);
    agg_all_subjects[[value_column_name]] = as.numeric(agg_all_subjects[[value_column_name]]);
  }

  return(agg_all_subjects);
}


#' @title Aggregate standard space (fsaverage) morphometry data for multiple measures over hemispheres for a group of subjects.
#'
#' @description Compute the mean (or other aggregates) over all vertices of a subject from standard space morphometry data (like 'surf/lh.area.fwhm10.fsaverage.mgh'). You can specify several measures and hemispheres. Uses knowledge about the FreeSurfer directory structure to load the correct files.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measures, vector of strings. Names of the vertex-wise morphometry measures. E.g., c("area", "thickness"). Used to construct the names of the morphometry file to be loaded.
#'
#' @param hemis, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param fwhm, string. Smoothing as string, e.g. '10' or '25'.
#'
#' @param agg_fun, function. An R function that aggregates data, typically max, mean, min or something similar. Note: this is NOT a string, put the function name without quotes. Defaults to mean.
#'
#' @param template_subject, string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @param cast, Whether a separate 'hemi' column should exist.
#'
#' @return dataframe with aggregated values over all measures and hemis for all subjects, with m columns and n rows, where n is the number of subjects. The m columns are 'subject_id' and '<hemi>.<measure>' (e.g., "lh.thickness") for all combinations of hemi and measure, the latter contains the aggregated data.
#'
#' @family global aggregation functions
#'
#' @export
group.multimorph.agg.standard <- function(subjects_dir, subjects_list, measures, hemis, fwhm, agg_fun = mean, template_subject='fsaverage', format='mgh', cast=TRUE) {
  agg_all_measures_and_hemis = data.frame();
  for (hemi in hemis) {
    if(!(hemi %in% c("lh", "rh", "both"))) {
      stop(sprintf("Each entry in the parameter 'hemis' must be one of 'lh', 'rh' or 'both, but the current one is '%s'.\n", hemi));
    }
    for (measure in measures) {
        measure_hemi_data = group.morph.agg.standard(subjects_dir, subjects_list, measure, hemi, fwhm, agg_fun = agg_fun, template_subject=template_subject, format=format, cast=cast);

        if(nrow(agg_all_measures_and_hemis) == 0) {
          agg_all_measures_and_hemis = measure_hemi_data;
        } else {
          if(cast) {
            agg_all_measures_and_hemis = rbind(agg_all_measures_and_hemis, measure_hemi_data);
          } else {
            agg_all_measures_and_hemis = merge(agg_all_measures_and_hemis, measure_hemi_data, by.x='subject_id', by.y='subject_id');
          }
        }
    }
  }

  return(agg_all_measures_and_hemis);
}




#' @title Aggregate native space morphometry data for multiple measures over hemispheres for a group of subjects.
#'
#' @description Compute the mean (or other aggregates) over all vertices of a subject from native space morphometry data (like 'surf/lh.area'). You can specify several measures and hemispheres. Uses knowledge about the FreeSurfer directory structure to load the correct files.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measures, vector of strings. Names of the vertex-wise morhometry measures. E.g., c("area", "thickness"). Used to construct the names of the morphometry file to be loaded.
#'
#' @param hemis, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param agg_fun, function. An R function that aggregates data, typically max, mean, min or something similar. Note: this is NOT a string, put the function name without quotes. Defaults to mean.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @param cast, logical. Whether a separate hemi column should exist.
#'
#' @return dataframe with aggregated values over all measures and hemis for all subjects, with m columns and n rows, where n is the number of subjects. The m columns are 'subject_id' and '<hemi>.<measure>' (e.g., "lh.thickness") for all combinations of hemi and measure, the latter contains the aggregated data.
#'
#' @family global aggregation functions
#'
#' @export
group.multimorph.agg.native <- function(subjects_dir, subjects_list, measures, hemis, agg_fun = mean, format='curv', cast=TRUE) {
  agg_all_measures_and_hemis = data.frame();
  for (hemi in hemis) {
    if(!(hemi %in% c("lh", "rh", "both"))) {
      stop(sprintf("Each entry in the parameter 'hemis' must be one of 'lh', 'rh' or 'both' but the current one is '%s'.\n", hemi));
    }
    for (measure in measures) {
      measure_hemi_data = group.morph.agg.native(subjects_dir, subjects_list, measure, hemi, agg_fun = agg_fun, format=format, cast=cast);

      if(nrow(agg_all_measures_and_hemis) == 0) {
        agg_all_measures_and_hemis = measure_hemi_data;
      } else {

        if(cast) {
          agg_all_measures_and_hemis = rbind(agg_all_measures_and_hemis, measure_hemi_data);
        } else {
          agg_all_measures_and_hemis = merge(agg_all_measures_and_hemis, measure_hemi_data, by.x='subject_id', by.y='subject_id');
        }
      }
    }
  }

  return(agg_all_measures_and_hemis);
}
