#' @title Read subjects file
#'
#' @description Load a list of subjects from a subjects file, i.e., a simple text file containing one subject name per line.
#'
#' @param subjects_file, string. The path to the file.
#'
#' @param header, logical. Whether the file starts with a header line. Defaults to FALSE.
#'
#' @return a vector of strings. The subject IDs.
#'
#' @export
read.subjects = function(subjects_file, header=FALSE) {
    subjects_df = utils::read.table(subjects_file, header=header, col.names = c("subject_id"));
    subjects_list = as.vector(subjects_df$subject_id);
    return(subjects_list);
}



#' @title Read demographics file
#'
#' @description Load a list of subjects and metadata from a demographics file, i.e., a tab-separated file containing an arbitrary number of columns, one of which must be the subject id.
#'
#' @param demographics_file, string. The path to the file.
#'
#' @param header, logical. Whether the file starts with a header line.
#'
#' @param center, logical. Whether to center the data. See the docs for base::scale().
#'
#' @param scale, logical. Whether to scale the data. See the docs for base::scale().
#'
#' @param sep, string. Separator passed to utils::read.table(), defaults to '\t' for tabulator.
#'
#' @return a data.frame. The data in the file.
#'
#' @export
#' @importFrom dplyr "%>%"
read.demographics = function(demographics_file, header=TRUE, center=TRUE, scale=FALSE, sep='\t') {
  demographics_df = utils::read.table(demographics_file, header=header, sep=sep);
  demographics_df = demographics_df %>% dplyr::mutate_if(is.character, as.factor) %>% dplyr::mutate_if(is.numeric, scale, center=center, scale=scale);
  return(demographics_df);
}
