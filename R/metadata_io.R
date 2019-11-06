# Functions to read study meta information, like the subjects list and demographics data, from text files.
# These files are typically CSV or TSV files (sometimes exported from spreadsheet applications).

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
#' @examples
#'    subjects_file = system.file("extdata", "subjects.txt", package = "fsbrain", mustWork = TRUE);
#'    subjects_list = read.md.subjects(subjects_file);
#'
#' @family metdata functions
#'
#' @export
read.md.subjects = function(subjects_file, header=FALSE) {
    if(! file.exists(subjects_file)) {
        stop(sprintf("Cannot access subjects file '%s'.\n", subjects_file));
    }
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
#' @param report, logical. Whether to write an overview, i.e., some descriptive statistics for each column, to STDOUT. Defaults to FALSE. See [fsbrain::report.on.demographics].
#'
#' @param stringsAsFactors, logical. Whether to convert strings in the input data to factors. Defaults to TRUE.
#'
#' @return a dataframe. The data in the file. String columns will be returned as factors, which you may want to adapt afterwards for the subject identifier column.
#'
#' @family metdata functions
#'
#' @examples
#'    demographics_file =
#'    system.file("extdata", "demographics.tsv", package = "fsbrain", mustWork = TRUE);
#'    column_names = c("subject_id", "group", "age");
#'    demographics = read.md.demographics(demographics_file,
#'    column_names = column_names, report = FALSE);
#'
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom stats sd
read.md.demographics = function(demographics_file, column_names, header=TRUE, scale_and_center=FALSE, sep='', report=FALSE, stringsAsFactors=TRUE, group_column_name=NULL) {
    if(! file.exists(demographics_file)) {
        stop(sprintf("Cannot access demographics file '%s'.\n", demographics_file));
    }
    demographics_df = utils::read.table(demographics_file, header=header, sep=sep, stringsAsFactors=stringsAsFactors);

    if(ncol(demographics_df) != length(column_names)) {
      stop(sprintf("Column count mismatch in demographics file '%s': expected %d from 'column_names' parameter, but got %d.\n", demographics_file, length(column_names), ncol(demographics_df)));
    }

    colnames(demographics_df) = column_names;

    if(report) {
      report_lines = report.on.demographics(demographics_df, group_column_name=group_column_name);
    }


    if(scale_and_center) {
        if(report) {
          report_lines = c(report_lines, sprintf(" *Demographics report notice: the numeric data will be scaled and centered, and the report above represents the data BEFORE that operation.\n"));
        }
        scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm);
        demographics_df = demographics_df %>% dplyr::mutate_if(is.numeric, scale2);
    }

    if(report) {
      report_lines = c(report_lines, sprintf("===End of Demographics Report==="));
    }

    return(demographics_df);
}


#' @title Print a demographics report
#'
#' @param demographics_df a demographics data.frame, as returned by [read.md.demographics]
#'
#' @param group_column_name, string or NULL. If given, the column name of the group column. It must be a factor column with 2 levels. Enables group-comparison tests. Defaults to NULL.
#'
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom stats sd var.test t.test shapiro.test wilcox.test
report.on.demographics = function(demographics_df, group_column_name=NULL) {

    report_lines = c();

    if(! is.null(group_column_name)) {
      if(! (group_column_name %in% colnames(demographics_df))) {
          stop(sprintf("Group column '%s' does not exist in demographics_df with %d columns.\n", group_column_name, ncol(demographics_df)));
      }
    }

    report_lines = c(report_lines, sprintf("===Demographics report follows==="));

    numeric_colname = c();
    numeric_colmin = c();
    numeric_colmean = c();
    numeric_colmax = c();
    numeric_nacount = c();

    factor_colname = c();
    factor_numlevels = c();
    factor_nacount = c();

    string_colname = c();
    string_numunique = c();
    string_nacount = c();

    run_group_tests = (!is.null(group_column_name));
    if(run_group_tests) {
      if(! is.factor(demographics_df[[group_column_name]])) {
        demographics_df[[group_column_name]] = as.factor(demographics_df[[group_column_name]]);
      }

      if(nlevels(demographics_df[[group_column_name]]) != 2) {
        run_group_tests = FALSE;
        warning(sprintf("Group column '%s' has %d levels, but 2 required. Not running group tests.\n", group_column_name, nlevels(demographics_df[[group_column_name]])));
      }
    }
    if(run_group_tests) {

      group_levels = levels(demographics_df[[group_column_name]]);
      group1_name = group_levels[1];
      group2_name = group_levels[2];
      group1_data = subset(demographics_df, demographics_df[[group_column_name]] == group1_name);
      group2_data = subset(demographics_df, demographics_df[[group_column_name]] == group2_name);

      report_lines = c(report_lines, sprintf(" *Found group column with 2 levels: %d subjects belong to group '%s', %d to '%s'.", nrow(group1_data), group1_name, nrow(group2_data), group2_name));
      if(!(nrow(group1_data) >= 3 && nrow(group2_data) >= 3)) {
        run_group_tests = FALSE;
        warning(sprintf("The group '%s' has %d members, and the group '%s' has %d. Both groups must have at least 3 members to run group tests, skipping tests.\n", group1_name, nrow(group1_data), group2_name, nrow(group2_data)));
      }
    }

    for (colname in colnames(demographics_df)) {
        coldata = demographics_df[[colname]];
        if(is.numeric(coldata)) {
            numeric_colname = c(numeric_colname, colname);
            numeric_colmin = c(numeric_colmin, min(coldata));
            numeric_colmean = c(numeric_colmean, mean(coldata));
            numeric_colmax = c(numeric_colmax, max(coldata));
            numeric_nacount = c(numeric_nacount, sum(is.na(coldata)));
            if(run_group_tests) {
              test_lines = test.numerical.meandiff(colname, group1_name, group2_name, group1_data[[colname]], group2_data[[colname]], isPaired = FALSE);
            }
        } else if (is.factor(demographics_df[[colname]])) {
            factor_colname = c(factor_colname, colname);
            factor_numlevels = c(factor_numlevels, nlevels(demographics_df[[colname]]));
            factor_nacount = c(factor_nacount, sum(is.na(coldata)));
        } else if (is.character(demographics_df[[colname]])) {
            string_colname = c(string_colname, colname);
            string_numunique = c(string_numunique, length(unique(demographics_df[[colname]])));
            string_nacount = c(string_nacount, sum(is.na(coldata)));
        }
    }



    report_lines = c(report_lines, sprintf(" *Demographics report for the %d numeric columns (min/mean/max from %d rows):", length(numeric_colname), nrow(demographics_df)));

    numeric_desc = data.frame("column"=numeric_colname, min=numeric_colmin, mean=numeric_colmean, max=numeric_colmax);
    for(numeric_col_idx in seq_len(length(numeric_colname))) {
      report_lines = c(report_lines, sprintf("%s\t%f\t%f\t%f\t%d", numeric_colname[numeric_col_idx], numeric_colmin[numeric_col_idx], numeric_colmean[numeric_col_idx], numeric_colmax[numeric_col_idx], numeric_nacount[numeric_col_idx]));
    }

    report_lines = c(report_lines, sprintf(" *Demographics report for the %d factor columns (levels from %d rows):", length(factor_colname), nrow(demographics_df)));
    for(factor_col_idx in seq_len(length(factor_colname))) {
      report_lines = c(report_lines, sprintf("%s\t%f\t%d", factor_colname[factor_col_idx], factor_numlevels[factor_col_idx], factor_nacount[factor_col_idx]));
    }

    report_lines = c(report_lines, sprintf(" *Demographics report for the %d character columns (from %d rows):", length(string_colname), nrow(demographics_df)));
    for(string_col_idx in seq_len(length(string_colname))) {
      report_lines = c(report_lines, sprintf("%s\t%d\t%d", string_colname[string_col_idx], string_numunique[string_col_idx], string_nacount[string_col_idx]));
    }
}


#' @keywords internal
test.numerical.meandiff <- function(colname, group1_name, group2_name, group1_data_column, group2_data_column, isPaired) {
  if(isPaired) {
    warning("Paired testing not implemented yet. Skipping group-level tests.\n");
    return(c());
  } else {
    return(test.numerical.meandiff.unpaired(colname, group1_name, group2_name, group1_data_column, group2_data_column));
  }
}


#' @keywords internal
#' @importFrom dplyr "%>%"
#' @importFrom stats sd var.test t.test shapiro.test wilcox.test
test.numerical.meandiff.unpaired <- function(colname, group1_name, group2_name, group1_data_column, group2_data_column) {
  test_lines = c();
  # Test data for equal variances of the two groups
  ftest_res = stats::var.test(group1_data_column, group2_data_column);
  var_equal = (ftest_res$p.value > 0.05);
  if(! var_equal) {
    test_lines = c(test_lines, sprintf(" Demographics column '%s': variance not equal between groups with p=%f", colname, ftest_res$p.value));
  }

  # Test whether data follows normal distribution
  shap_res_group1 = stats::shapiro.test(group1_data_column);
  both_columns_look_normal = TRUE;
  if(shap_res_group1$p.value < 0.05) {
    test_lines = c(test_lines, sprintf(" Demographics column '%s': Data for group '%s' does not seem to follow a normal distribution (shapiro.test p.value=%f).", colname, group1_name, shap_res_group1$p.value));
    both_columns_look_normal = FALSE;
  }
  shap_res_group2 = stats::shapiro.test(group2_data_column);
  if(shap_res_group2$p.value < 0.05) {
    test_lines = c(test_lines, sprintf(" Demographics column '%s': Data for group '%s' does not seem to follow a normal distribution (shapiro.test p.value=%f).", colname, group2_name, shap_res_group2$p.value));
    both_columns_look_normal = FALSE;
  }


  # Test for significant mean difference using parametric method, taking the variance result into account
  ttest_res = stats::t.test(group1_data_column, group2_data_column, var.equal=var_equal);
  test_lines = c(test_lines, sprintf(" Demographics column '%s': p=%f for t.test for different group means", colname, ttest_res$p.value));
  if(!both_columns_look_normal) {
    test_lines = c(test_lines, sprintf(" Demographics column '%s': WARNING: non-normal data detected, consider ignoring the ttest result and using the following wilcox test result instead.\n", colname));
  }
  wilcox_res = stats::wilcox.test(group1_data_column, group2_data_column);
  test_lines = c(test_lines, sprintf(" Demographics column '%s': p=%f for wilcox test for different group means.\n", colname, wilcox_res$p.value));
  return(test_lines);
}

