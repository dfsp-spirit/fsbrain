#' @title Aggregate native space morphometry data over one hemisphere for a group of subjects.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param agg_fun, function. An R function that aggregates data, typically max, mean, min or something similar. Note: this is NOT a string, put the function name without quotes. Defaults to mean.
#'
#' @return dataframe with aggregated values for all subjects, with 2 columns and n rows, where n is the number of subjects. The 2 columns are 'subject_id' and 'value', the latter contains the aggregated data.
#'
#'
#' @export
group.morph.agg.native <- function(subjects_dir, subjects_list, measure, hemi, agg_fun = mean, format='curv') {
  agg_all_subjects = data.frame();
  for (subject_id in subjects_list) {
      cat(sprintf("aggregated for subject '%s'\n",subject_id ))
      morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi, format=format);
      if(nrow(agg_all_subjects) == 0) {
        agg_all_subjects = data.frame(c(as.character(subject_id)), agg_fun(morph_data), stringsAsFactors = FALSE);
      } else {
        agg_all_subjects = rbind(agg_all_subjects, c(as.character(subject_id), agg_fun(morph_data)));
      }
  }
  colnames(agg_all_subjects) = c("subject_id", "value");
  return(agg_all_subjects);
}


#' @title Retrieve morphometry data for a single subject.
subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, format='curv') {
  curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s", hemi, measure, freesurferformats::fs.get.morph.file.ext.for.format(format)));
  if(!file.exists(curvfile)) {
    stop(sprintf("Curv file '%s' for subject '%s' measure '%s' cannot be accessed.\n", curvfile, subject_id, measure));
  }
  return(freesurferformats::read.fs.morph(curvfile));
}
