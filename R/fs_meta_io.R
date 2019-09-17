#' @title Read subjects file
#'
#' @description Load a list of subjects from a subjects file, i.e., a simple text file containing one subject name per line and no header.
#'
#' @param subjects_file, string. The path to the file.
#'
#' @return a vector of strings. The subject IDs.
#'
#' @export
nit.read.subjectsfile = function(subjects_file) {
    subjects_df = utils::read.table(subjects_file, header=FALSE, col.names = c("subject_id"));
    subjects_list = as.vector(subjects_df$subject_id);
    return(subjects_list);
}

#' @title Read demographics file
#'
#' @description Load a list of subjects and metadata from a demographics file, i.e., a tab-separated file containing an arbitrary number of columns, one of which must be the subject id.
#'
#' @param demographics_file, string. The path to the file.
#'
#' @return a data.frame. The data in the file.
#'
#' @export
nit.read.demographics = function(demographics_file) {
  demographics_df = utils::read.table(demographics_file, header=TRUE, sep='\t');
  demographics_df = demographics_df %>% dplyr::mutate_if(is.character, as.factor) %>% dplyr::mutate_if(is.numeric, scale, scale=F);
  return(demographics_df);
}
