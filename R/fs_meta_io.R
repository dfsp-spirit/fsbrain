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
#' @param column_names, vector of strings. The column names to set in the returned dataframe. The length must match the number of columns in the file.
#'
#' @param header, logical. Whether the file starts with a header line.
#'
#' @param scale_and_center, logical. Whether to center and scale the data. Defaults to FALSE.
#'
#' @param sep, string. Separator passed to utils::read.table(), defaults to tabulator.
#'
#' @param report, logical. Whether to write an overview, i.e., some descriptive statistics for each column, to STDOUT. Defaults to TRUE.
#'
#' @return a dataframe. The data in the file. String columns will be returned as factors, which you may want to adapt afterwards for the subject identifier column.
#'
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom stats sd
read.demographics = function(demographics_file, column_names, header=TRUE, scale_and_center=FALSE, sep='\t', report=TRUE) {
    demographics_df = utils::read.table(demographics_file, header=header, sep=sep);

    if(ncol(demographics_df) != length(column_names)) {
      stop(sprintf("Column count mismatch in demographics file '%s': expected %d from 'column_names' parameter, but got %d.\n", demographics_file, length(column_names), ncol(demographics_df)));
    }

    demographics_df = demographics_df %>% dplyr::mutate_if(is.character, as.factor);
    colnames(demographics_df) = column_names;

    numeric_colname = c();
    numeric_colmin = c();
    numeric_colmean = c();
    numeric_colmax = c();
    factor_colname = c();
    factor_numlevels = c();
    if(report) {
        for (colname in names(demographics_df)) {
            if(is.numeric(demographics_df[[colname]])) {
              numeric_colname = c(numeric_colname, colname);
              numeric_colmin = c(numeric_colmin, min(demographics_df[[colname]]));
              numeric_colmean = c(numeric_colmean, mean(demographics_df[[colname]]));
              numeric_colmax = c(numeric_colmax, max(demographics_df[[colname]]));
            } else {
              factor_colname = c(factor_colname, colname);
              factor_numlevels = c(factor_numlevels, nlevels(demographics_df[[colname]]));
            }
        }
        cat(sprintf("===Demographics report follows===\n"));
        cat(sprintf(" *Demographics report notice: pass 'report=FALSE' to read.demographics() to silence this report.\n"));
        cat(sprintf(" *Demographics report for the %d numeric columns (min/mean/max from %d rows):\n", length(numeric_colname), nrow(demographics_df)));
        numeric_desc = data.frame("column"=numeric_colname, min=numeric_colmin, mean=numeric_colmean, max=numeric_colmax);
        print(numeric_desc, row.names = TRUE);
        cat(sprintf(" *Demographics report for the %d factor columns (levels from %d rows):\n", length(factor_colname), nrow(demographics_df)));
        factor_desc = data.frame("column"=factor_colname, "num_levels"=factor_numlevels);
        print(factor_desc, row.names = TRUE);
    }

    if(scale_and_center) {
        if(report) {
            cat(sprintf(" *Demographics report notice: the numeric data will be scaled and centered, and the report above represents the data BEFORE that operation.\n"));
        }
        scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm);
        demographics_df = demographics_df %>% dplyr::mutate_if(is.numeric, scale2);
    }

    if(report) {
      cat(sprintf("===End of Demographics Report===\n"));
    }

    return(demographics_df);
}
