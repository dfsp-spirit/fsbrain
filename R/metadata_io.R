# Functions to read study meta information, like the subjects list and demographics data, from text files.
# These files are typically CSV or TSV files (sometimes exported from spreadsheet applications).

#' @title Read subjects file
#'
#' @description Load a list of subjects from a subjects file, i.e., a simple text file containing one subject name per line.
#'
#' @param subjects_file character string, the path to the subjects file.
#'
#' @param header logical, whether the file starts with a header line.
#'
#' @return vector of strings, the subject identifiers.
#'
#' @examples
#'    subjects_file = system.file("extdata", "subjects.txt", package = "fsbrain", mustWork = TRUE);
#'    subjects_list = read.md.subjects(subjects_file, header = FALSE);
#'
#' @family metadata functions
#'
#' @export
read.md.subjects = function(subjects_file, header) {
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
#' @param sep, string. Separator passed to \code{\link[utils]{read.table}}, defaults to tabulator.
#'
#' @param report, logical. Whether to write an overview, i.e., some descriptive statistics for each column, to STDOUT. Defaults to FALSE. See \code{\link[fsbrain]{report.on.demographics}}.
#'
#' @param stringsAsFactors, logical. Whether to convert strings in the input data to factors. Defaults to TRUE.
#'
#' @param group_column_name, string or NULL. If given, the column name of the group column. It must be a factor column with 2 levels. Enables group-comparison tests. Defaults to NULL.
#'
#' @return a dataframe. The data in the file. String columns will be returned as factors, which you may want to adapt afterwards for the subject identifier column.
#'
#' @family metadata functions
#'
#' @examples
#'    demographics_file =
#'    system.file("extdata", "demographics.tsv", package = "fsbrain", mustWork = TRUE);
#'    column_names = c("subject_id", "group", "age");
#'    demographics = read.md.demographics(demographics_file,
#'    header = TRUE, column_names = column_names, report = FALSE);
#'
#' @export
#' @importFrom stats sd
#' @importFrom utils read.table
read.md.demographics = function(demographics_file, column_names=NULL, header=FALSE, scale_and_center=FALSE, sep='', report=FALSE, stringsAsFactors=TRUE, group_column_name=NULL) {
    if(! file.exists(demographics_file)) {
        stop(sprintf("Cannot access demographics file '%s'.\n", demographics_file));
    }
    if(! is.logical(header)) {
      stop("Parameter 'header' must be logical");
    }
    demographics_df = utils::read.table(demographics_file, header=header, sep=sep, stringsAsFactors=stringsAsFactors);

    if(! header) {
        if(is.null(column_names)) {
            stop("Parameter 'column_names' is required if the file has no header (see parameter 'header' if it does.).");
        }
        if(ncol(demographics_df) != length(column_names)) {
          stop(sprintf("Column count mismatch in demographics file '%s': expected %d from 'column_names' parameter, but got %d.\n", demographics_file, length(column_names), ncol(demographics_df)));
        }
        colnames(demographics_df) = column_names;
    } else {
        if(! is.null(column_names)) {
            if(ncol(demographics_df) != length(column_names)) {
                stop(sprintf("Column count mismatch in demographics file '%s': expected %d from 'column_names' parameter, but got %d.\n", demographics_file, length(column_names), ncol(demographics_df)));
            }
            colnames(demographics_df) = column_names;
        }
    }

    if(report) {
      report_lines = report.on.demographics(demographics_df, group_column_name=group_column_name);
    }


    if(scale_and_center) {
        if(report) {
          report_lines = c(report_lines, sprintf(" *Demographics report notice: the numeric data will be scaled and centered, and the report above represents the data BEFORE that operation.\n"));
        }
        scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm);
        for (cn in colnames(demographics_df)) {
            if(is.numeric(demographics_df[[cn]])) {
                demographics_df[[cn]] = scale2(demographics_df[[cn]]);
            }
        }
    }

    if(report) {
      report_lines = c(report_lines, sprintf("===End of Demographics Report==="));
      cat(paste(report_lines, sep="\n"));
    }

    return(demographics_df);
}

# truncation:
# demographics_df = read.md.demographics('~/data/truncation/demographics.txt', column_names=c('id', 'group', 'site', 'sex', 'age', 'iq', 'ct', 'saw', 'sap', 'tbv'), header=F, stringsAsFactors = F)
# demographics.to.fsgd.file('~/truncation.fsgd', demographics_df, var_columns = c('site', 'sex', 'age', 'iq'))

# lGI:
# demographics_df = read.md.demographics('/Volumes/shared/projects/lgi_paper_AIMS/data/Demographics_639.txt', column_names=c('id', 'group', 'site', 'sex', 'age', 'iq', 'sa', 'ct', 'tbv'), header=F, stringsAsFactors = F)
# demographics.to.fsgd.file('~/lgi.fsgd', demographics_df, var_columns = c('site', 'sex', 'age', 'iq'))



#' @title Write FreeSurfer Group Descriptor (FSGD) file from demographics dataframe.
#'
#' @param filepath character string, the path to the output file in FSGD format
#'
#' @param demographics_df data.frame, as returned by \code{read.md.demographics} or created manually. Note that the data.frame must not contain any character columns, they should be converted to factors.
#'
#' @param group_column_name character string, the column name of the group column in the 'demographics_df'
#'
#' @param subject_id_column_name character string, the column name of the subject identifier column in the 'demographics_df'
#'
#' @param var_columns vector of character strings, the column names to include as variables in the FSGD file. If NULL (the default), all columns will be included (with the exception of the group column and the subject id column).
#'
#' @param ftitle character string, freeform title for the FSGD file
#'
#' @param fsgd_flag_lines vector of character strings, extra flag lines to write to the file. The default setting will activate de-meaning and rescaling.
#'
#' @return vector of character strings, the lines written to the 'filepath', invisible.
#'
#' @family metadata functions
#' @export
demographics.to.fsgd.file <- function(filepath, demographics_df, group_column_name='group', subject_id_column_name='id', var_columns=NULL, ftitle="OSGM", fsgd_flag_lines=c("DeMeanFlag 1", "ReScaleFlag 1")) {
  #GroupDescriptorFile 1
  #Title MyFSGD
  #Class Group1Male
  #Class Group1Female
  #Class Group2Male
  #Class Group2Female
  #Variables Age Weight
  #Input subject1 Group1Male 30 90
  #Input subject2 Group2Female 40 65
  if(! group_column_name %in% colnames(demographics_df)) {
    stop(sprintf("Dataframe does not contain group column: no column named '%s'.\n", group_column_name));
  }
  if(! subject_id_column_name %in% colnames(demographics_df)) {
    stop(sprintf("Dataframe does not contain subject identifier column: no column named '%s'.\n", subject_id_column_name));
  }

  if(is.null(var_columns)) {
    var_columns = colnames(demographics_df);
  }

  # remove group and subject columns from variable columns, if needed
  if(group_column_name %in% var_columns) {
    group_column_index = which(var_columns == group_column_name);
    retain_at_idx = rep(TRUE, length(var_columns));
    retain_at_idx[group_column_index] = F;
    var_columns = var_columns[retain_at_idx];
  }
  if(subject_id_column_name %in% var_columns) {
    subject_id_column_index = which(var_columns == subject_id_column_name);
    retain_at_idx = rep(TRUE, length(var_columns));
    retain_at_idx[subject_id_column_index] = F;
    var_columns = var_columns[retain_at_idx];
  }

  # Separate numerical columns from factor/character columns, the latter will be part of the Classes

  numeric_covariate_columns = c();
  class_part_columns = c();
  for(cname in var_columns) {
      if(is.numeric(demographics_df[[cname]])) {
          numeric_covariate_columns = c(numeric_covariate_columns, cname);
      } else {
          if(is.factor(demographics_df[[cname]])) {
            message(sprintf("Column '%s' is of type factor, which may lead to a numerical Class name that is hard to read.\n", cname));
          }

          class_part_columns = c(class_part_columns, cname);
      }
  }
  #cat(sprintf("Found %d numerical covariates, %d factors that will become part of the subject class (in addition to the group column '%s').\n", length(numeric_covariate_columns), length(class_part_columns), group_column_name));


  fsgd_lines = c("GroupDescriptorFile 1", sprintf("Title %s", ftitle));

  all_var_names = paste(numeric_covariate_columns, collapse=" ");
  fsgd_variable_line = sprintf("Variables %s", all_var_names);

  num_subjects = nrow(demographics_df);
  for(cname in var_columns) {
    if(! cname %in% colnames(demographics_df)) {
      stop(sprintf("Invalid entry in 'var_columns': dataframe 'demographics_df' does not contain column named '%s'.\n", cname));
    }
  }

  var_df = data.frame('idx_dummy' = rep(seq.int(num_subjects), num_subjects));
  for(cname in numeric_covariate_columns) {
    var_df[[cname]] = demographics_df[[cname]];
  }
  var_df$idx_dummy = NULL; # remove dummy column


  fsgd_subject_lines = c();
  fsgd_classes = c();
  for(subject_idx in seq.int(nrow(demographics_df))) {
    class_columns = c(group_column_name, class_part_columns);
    subject_class = get.subject.class(demographics_df, subject_idx, class_columns);
    if(! subject_class %in% fsgd_classes) {
        fsgd_classes = c(fsgd_classes, subject_class);
    }
    subject_id = as.character(demographics_df[[subject_id_column_name]][subject_idx]);
    subject_variables = paste(as.character(unname(var_df[subject_idx,])), collapse=" ");
    subject_line = sprintf("Input %s %s %s", subject_id, subject_class, subject_variables);
    fsgd_subject_lines = c(fsgd_subject_lines, subject_line);
  }
  fsgd_class_lines = paste("Class", fsgd_classes);
  fsgd_lines = c(fsgd_lines, fsgd_variable_line, fsgd_class_lines, fsgd_flag_lines, fsgd_subject_lines);

  fh = file(filepath);
  writeLines(fsgd_lines, fh);
  close(fh);
  return(invisible(fsgd_lines));
}


#' @title Construct FSGD Class name from group and non-continuous covariate columns.
#'
#' @inheritParams demographics.to.fsgd.file
#'
#' @param row_idx integer, the row in the df that belongs to this subject
#'
#' @param class_columns the column names to use
#'
#' @param collapse character string, the separator
#'
#' @return character string, the Class name for this subject, derived from the values in the 'class_columns'.
#'
#' @keywords internal
get.subject.class <- function(demographics_df, row_idx, class_columns, collapse="_") {
  df_values = unname(demographics_df[class_columns][row_idx,]);
  return(paste(as.character(df_values), collapse=collapse));
}


#' @title Print a demographics report
#'
#' @param demographics_df a demographics data.frame, as returned by  \code{\link[fsbrain]{read.md.demographics}}.
#'
#' @param group_column_name, string or NULL. If given, the column name of the group column. It must be a factor column with 2 levels. Enables group-comparison tests. Defaults to `NULL`.
#'
#' @param paired Whether the data of the two groups if paired (repeated measurements). Only relevant if group_column_name is given and tests for group differences are included in the report. Defaults to `FALSE`.
#'
#' @return vector of character strings, the lines of the demographics report.
#'
#' @family metadata functions
#'
#' @export
#' @importFrom stats sd var.test t.test shapiro.test wilcox.test
report.on.demographics = function(demographics_df, group_column_name=NULL, paired=FALSE) {

    report_lines = c();

    if(! is.null(group_column_name)) {
      if(! (group_column_name %in% colnames(demographics_df))) {
          stop(sprintf("Group column '%s' does not exist in demographics_df with %d columns.\n", group_column_name, ncol(demographics_df)));
      }
    }

    report_lines = c(report_lines, sprintf("===Demographics report for %d subjects follows===", nrow(demographics_df)));

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

      if(!(nrow(group1_data) >= 3 && nrow(group2_data) >= 3)) {
        run_group_tests = FALSE;
        warning(sprintf("The group '%s' has %d members, and the group '%s' has %d. Both groups must have at least 3 members to run group tests, skipping tests.\n", group1_name, nrow(group1_data), group2_name, nrow(group2_data)));
      }
    }

    if(run_group_tests) {
      if(paired) {
        paired_tag = "paired";
      } else {
        paired_tag = "unpaired";
      }
      report_lines = c(report_lines, sprintf("* Found group column with 2 levels: %d subjects belong to group '%s', %d to '%s'. Running %s tests for mean differences on numeric columns.", nrow(group1_data), group1_name, nrow(group2_data), group2_name, paired_tag));
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
              report_lines = c(report_lines, test.numerical.meandiff(colname, group1_name, group2_name, group1_data[[colname]], group2_data[[colname]], paired = paired));
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


    if(length(numeric_colname) > 0) {
      report_lines = c(report_lines, sprintf("* Demographics report for the %d numeric columns:", length(numeric_colname)));
      report_lines = c(report_lines, sprintf("    column min mean max num_NA"));
      for(numeric_col_idx in seq_len(length(numeric_colname))) {
        report_lines = c(report_lines, sprintf("    %s %f %f %f %d", numeric_colname[numeric_col_idx], numeric_colmin[numeric_col_idx], numeric_colmean[numeric_col_idx], numeric_colmax[numeric_col_idx], numeric_nacount[numeric_col_idx]));
      }
    }

    if(length(factor_colname) > 0) {
      report_lines = c(report_lines, sprintf("* Demographics report for the %d factor columns:", length(factor_colname)));
      report_lines = c(report_lines, sprintf("    column num_levels num_NA"));
      for(factor_col_idx in seq_len(length(factor_colname))) {
        report_lines = c(report_lines, sprintf("    %s %d %d", factor_colname[factor_col_idx], factor_numlevels[factor_col_idx], factor_nacount[factor_col_idx]));
      }
    }

    if(length(string_colname) > 0) {
      report_lines = c(report_lines, sprintf("* Demographics report for the %d character columns:", length(string_colname)));
      report_lines = c(report_lines, sprintf("    column num_unique num_NA"));
      for(string_col_idx in seq_len(length(string_colname))) {
        report_lines = c(report_lines, sprintf("    %s %d %d", string_colname[string_col_idx], string_numunique[string_col_idx], string_nacount[string_col_idx]));
      }
    }

    return(report_lines);
}

#' @title Perform tests for group differences on paired or unpaired data for two groups.
#'
#' @description This function is intended to give you a quick overview of your demographics data, it is in no way intended to replace a detailed analysis of your data. You should always visualize and analyze your data interactively instead of relying on automated methods like this. Outliers and are very common in real-world data while perfectly normal data is very rare, multiple testing may affect your results. Look at your data!
#'
#' @param colname string, the name of the data (used to label the data in the output)
#'
#' @param group1_name string, the name of the first group (used to label the data in the output)
#'
#' @param group2_name string, the name of the first group (used to label the data in the output)
#'
#' @param group1_data_column the data for group1 as a numerical vector. Typically a column from your demographics dataframe.
#'
#' @param group2_data_column the data for group2 as a numerical vector. Typically a column from your demographics dataframe.
#'
#' @param paired logical, whether the data is paired (repeated measures).
#'
#' @return vector of strings, the lines of the report. You can print to STDOUT or write it to a file.
#'
#' @keywords internal
test.numerical.meandiff <- function(colname, group1_name, group2_name, group1_data_column, group2_data_column, paired) {
  if(paired) {
    return(test.numerical.meandiff.paired(colname, group1_name, group2_name, group1_data_column, group2_data_column));
  } else {
    return(test.numerical.meandiff.unpaired(colname, group1_name, group2_name, group1_data_column, group2_data_column));
  }
}

#' @title Perform tests for group differences on unpaired data for two groups.
#'
#' @description This function is intended to give you a quick overview of your demographics data, it is in no way intended to replace a detailed analysis of your data. You should always visualize and analyze your data interactively instead of relying on automated methods like this. Outliers and are very common in real-world data while perfectly normal data is very rare, multiple testing may affect your results. Look at your data!
#'
#' @param colname string, the name of the data (used to label the data in the output)
#'
#' @param group1_name string, the name of the first group (used to label the data in the output)
#'
#' @param group2_name string, the name of the first group (used to label the data in the output)
#'
#' @param group1_data_column the data for group1 as a numerical vector. Typically a column from your demographics dataframe.
#'
#' @param group2_data_column the data for group2 as a numerical vector. Typically a column from your demographics dataframe.
#'
#' @return vector of strings, the lines of the report. You can print to STDOUT or write it to a file.
#'
#' @keywords internal
#' @importFrom stats sd var.test t.test shapiro.test wilcox.test
test.numerical.meandiff.unpaired <- function(colname, group1_name, group2_name, group1_data_column, group2_data_column) {
  test_lines = c();
  # Test data for equal variances of the two groups
  ftest_res = stats::var.test(group1_data_column, group2_data_column);
  var_equal = (ftest_res$p.value > 0.05);
  if(! var_equal) {
    test_lines = c(test_lines, sprintf("  - Demographics column '%s': variance not equal between groups with p=%f", colname, ftest_res$p.value));
  }

  # Test whether data follows normal distribution
  shap_res_group1 = stats::shapiro.test(group1_data_column);
  both_columns_look_normal = TRUE;
  if(shap_res_group1$p.value < 0.05) {
    test_lines = c(test_lines, sprintf(" - Demographics column '%s': Data for group '%s' does not seem to follow a normal distribution (shapiro.test p.value=%f).", colname, group1_name, shap_res_group1$p.value));
    both_columns_look_normal = FALSE;
  }
  shap_res_group2 = stats::shapiro.test(group2_data_column);
  if(shap_res_group2$p.value < 0.05) {
    test_lines = c(test_lines, sprintf(" - Demographics column '%s': Data for group '%s' does not seem to follow a normal distribution (shapiro.test p.value=%f).", colname, group2_name, shap_res_group2$p.value));
    both_columns_look_normal = FALSE;
  }


  # Test for significant mean difference using parametric method, taking the variance result into account
  ttest_res = stats::t.test(group1_data_column, group2_data_column, var.equal=var_equal);
  test_lines = c(test_lines, sprintf(" - Demographics column '%s': p=%f for t.test for different group means", colname, ttest_res$p.value));
  if(!both_columns_look_normal) {
    test_lines = c(test_lines, sprintf(" - Demographics column '%s': WARNING: non-normal data detected, consider ignoring the ttest result and using the following wilcox test result instead.\n", colname));
  }
  wilcox_res = stats::wilcox.test(group1_data_column, group2_data_column);
  test_lines = c(test_lines, sprintf(" - Demographics column '%s': p=%f for wilcox test for different group means.\n", colname, wilcox_res$p.value));
  return(test_lines);
}


#' @title Perform tests for group differences on paired data (repeated measurements) for two conditions or time points.
#'
#' @description This function is intended to give you a quick overview of your demographics data, it is in no way intended to replace a detailed analysis of your data. You should always visualize and analyze your data interactively instead of relying on automated methods like this. Outliers and are very common in real-world data while perfectly normal data is very rare, multiple testing may affect your results. Look at your data!
#'
#' @param colname string, the name of the data (used to label the data in the output)
#'
#' @param condition1_name string, the name of the first condition (used to label the data in the output)
#'
#' @param condition2_name string, the name of the first condition (used to label the data in the output)
#'
#' @param condition1_data_column the data for condition1 as a numerical vector. Typically a column from your demographics dataframe.
#'
#' @param condition2_data_column the data for condition2 as a numerical vector. Typically a column from your demographics dataframe.
#'
#' @return vector of strings, the lines of the report. You can print to STDOUT or write it to a file.
#'
#' @keywords internal
#' @importFrom stats sd var.test t.test shapiro.test wilcox.test
test.numerical.meandiff.paired <- function(colname, condition1_name, condition2_name, condition1_data_column, condition2_data_column) {
  test_lines = c();

  if(length(condition1_data_column) != length(condition2_data_column)) {
    warning(sprintf("Data for condition 1 has %d observations, but condition 2 has %d. Counts must match, skipping tests.\n", length(condition1_data_column), length(condition2_data_column)));
    return(test_lines);
  }

  # Compute differences between the two conditions
  differences = condition1_data_column - condition2_data_column;
  # Test whether the differences follow normal distribution
  shap_res_differences = stats::shapiro.test(differences);
  differences_are_not_normal = FALSE;
  if(shap_res_differences$p.value < 0.05) {
    test_lines = c(test_lines, sprintf(" - Demographics column '%s': Differences between the 2 groups do not seem to follow a normal distribution (shapiro.test p.value=%f).", colname, shap_res_differences$p.value));
    differences_are_not_normal = TRUE;
  }

  ttest_res = stats::t.test(condition1_data_column, condition2_data_column, paired=TRUE);
  test_lines = c(test_lines, sprintf(" - Demographics column '%s': p=%f for paired t.test for different group means", colname, ttest_res$p.value));
  if(differences_are_not_normal) {
    test_lines = c(test_lines, sprintf(" - Demographics column '%s': WARNING: non-normal data detected, consider ignoring the paired ttest result and using the following paired wilcox test result instead.\n", colname));
    wilcox_res = stats::wilcox.test(condition1_data_column, condition2_data_column, paired=TRUE);
    test_lines = c(test_lines, sprintf(" - Demographics column '%s': p=%f for wilcox test for different group means.\n", colname, wilcox_res$p.value));
  }
  return(test_lines);
}


#' @title Read subjects list from an FSGD file.
#'
#' @param filepath character string, path to a FreeSurfer Group Descriptor (FSGD) file.
#'
#' @return vector of character strings, the subject identifiers
#'
#' @note This is not a parser for all data in an FSGD file.
#'
#' @seealso \code{\link{demographics.to.fsgd.file}}
#'
#' @export
read.md.subjects.from.fsgd <- function(filepath) {
  fsgd_lines = readLines(filepath);
  subject_lines = fsgd_lines[startsWith(fsgd_lines, 'Input')];
  subject_lines_split = strsplit(subject_lines, " ");
  # A split line looks like: "Input"   "subj12345"   "control" "25"      "male"    "London"  "116"

  num_subjects = length(subject_lines_split);

  subjects = rep("", num_subjects);
  for(row_idx in seq.int(num_subjects)) {
    subjects[row_idx] = subject_lines_split[[row_idx]][2]; # The subject ID is at 2nd position.
  }
  return(subjects);
}


#' @title Convert a dataframe containing demographics data to a qdec.table.dat file and related files.
#'
#' @description This creates the `qdec.table.dat` and all required related files (the factor level files) in a directory.
#'
#' @param df a data.frame containing demographics information. Make sure to have factors encoded as factors (not strings), so that the QDEC level files get created for them. Must contain a column named 'fsid' with the subject IDs as first column. If you want a long table, make sure to use \code{\link{qdec.table.skeleton}} to generate the timepoint information instead of doing it manually.
#'
#' @param output_path character string, existing directory into which to write the QDEC files. If the last directory level does not exist, it will be created.
#'
#' @param long logical, whether this is for a longitudinal run. If so, the df must contain a column named 'fsid-base' as the second column. It must also contain some column that gives the inter-scan time (from this scan timepoint to the previous one). The time unit (years, days, ...) is up to you, but typically one is interested in yearly change, the unit should be years. The name of the column (e.g., 'years') must be given to 'mris_slopes' later on the command line with the \code{--time <column_name>} argument. The requires information can be generated conveniently with the \code{\link{qdec.table.skeleton}} function.
#'
#' @param long_timecolumn character string, the name of the column holding the inter-scan time. Ignored unless parameter \code{long} is \code{TRUE}. See the description for parameter \code{long} for details.
#'
#' @param add_fake_level2 logical, whether to add a 2nd fake level to the level files of factors with only a single level. Such factors make little sense, but QDEC refuses to open the resulting files at all in such a case, which seems a bit overkill. If TRUE, a 2nd level named 'level2' will be added so that one can open the output in QDEC.
#'
#' @param qdec_file_name character string, the filename of the QDEC file to write. Must be only the file name (with extension if you want). See \code{output_path} to set the ouput directory where this will be created.
#'
#' @note IMPORTANT: If you import the dataframe from a text file with functions like \code{read.table}, they will by default replace dashes in column names with dots. So if you have a column named \code{fsid-base} in there, after loading it will be named \code{fsid.base}. See the \code{check.names} parameter for \code{read.table} to prevent that.
#'
#' @seealso The function \code{\link{qdec.table.skeleton}} to generate the data.frame used as the 'df' argument for this function.
#'
#' @examples
#' \dontrun{
#'    dem = readxl::read_xls("~/data/study1/demographics.xsl");
#'    # or: dem = read.table("~/demographics.csv", check.names=FALSE);
#'    # You may want to rearrange/rename/delete some columns here.
#'    demographics.to.qdec.table.dat(dem, "~/data/study1/qdec/");
#'    #
#'    # a second one with real data:
#'    dem = data.frame("ID"=paste("subject", seq(5), sep=""),
#'       "age"=sample.int(20, 5)+10L, "isi"=rnorm(5, 2.0, 0.1)); #sample data.
#'    long_table = qdec.table.skeleton(dem$ID, dem$isi);
#'    demographics.to.qdec.table.dat(long_table, long=TRUE);
#' }
#' @importFrom utils write.table
#' @export
demographics.to.qdec.table.dat <- function(df, output_path=".", long=FALSE, add_fake_level2=FALSE, long_timecolumn="years", qdec_file_name="qdec.table.dat") {
  if(! dir.exists(output_path)) {
    dir.create(output_path); # create paths, but only non-recursively.
  }
  if(! dir.exists(output_path)) {
    stop(sprintf("Creating the output directory '%s' failed.\n", output_path));
  }

  # Check for required columns.
  required_columns = c("fsid");
  if(long) {
    required_columns = c(required_columns, "fsid-base", long_timecolumn);
  }
  for(reqcol in required_columns) {
    if(! (reqcol %in% colnames(df))) {
      if(reqcol == "fsid-base") {
        if("fsid.base" %in% colnames(df)) {
          warning("A column named 'fsid.base' was found in the dataframe but requird column 'fsid-base' is missing. Was it maybe accidentaly renamed on import by R functions to create a valid R variable name? See the note in the documentation for this function for more hints on how to avoid that.");
        }
      }
      if(reqcol == long_timecolumn) {
        warning(sprintf("A time column giving the inter-scan time is required for a longitudinal table but was not found. Expected column name '%s' for this column, use parameter 'long_timecolumn' to adapt the name.\n", long_timecolumn));
      }
      stop(sprintf("The data.frame in parameter 'df' must contain a column named '%s'.\n", reqcol));
    }
  }
  # The fsid and fsid-base columns MUST be the first 2 columns.
  if(colnames(df)[1] != "fsid") {
    stop("The first column must be the 'fsid' column.");
  }
  if(long) {
    if(colnames(df)[2] != "fsid-base") {
      stop("The second column must be the 'fsid-base' column for longitudinal files.");
    }
  }

  # Write te qdec.table.dat file.
  qdec_table_dat_file = file.path(output_path, qdec_file_name);
  write.table(df, file=qdec_table_dat_file, quote = FALSE, col.names = TRUE, row.names = FALSE);
  message(sprintf("Wrote qdec table file '%s' containing %d columns and %d rows.\n", qdec_table_dat_file, ncol(df), nrow(df)));

  # Write the factor level files.
  for(qcol in colnames(df)) {
    if(is.factor(df[[qcol]])) {
      if(! (qcol %in% c("fsid", "fsid-base"))) {
        factor_filename = file.path(output_path, sprintf("%s.levels", qcol));
        file_conn = file(factor_filename);
        file_lines = levels(df[[qcol]]);
        if(length(levels(df[[qcol]])) < 2L) {
          if(add_fake_level2) {
            file_lines = c(file_lines, "level2");
            message(sprintf("The factor '%s' has only %d level, adding 2nd fake level 'level2' to file '%s' as requested by parameter 'add_fake_level2'.\n", qcol, length(levels(df[[qcol]])), factor_filename));
          } else {
            warning(sprintf("The factor '%s' has only %d level, QDEC requires at least 2 levels per factor and will NOT open the resulting file. You can fix this manually by adding a second line with a fake level to the file '%s' or set the parameter 'add_fake_level2' to TRUE.\n", qcol, length(levels(df[[qcol]])), factor_filename));
          }
        }
        writeLines(file_lines, file_conn);
        close(file_conn);
        message(sprintf("Wrote levels file '%s' for factor %s with %d levels.\n", factor_filename, qcol, length(levels(df[[qcol]]))));
      }
    }
  }
}


#' @title Get subject names from sub directories of FreeSurfer long directory.
#'
#' @description Find all subject names for which the FreeSurfer longitudinal pipeline may have finished. These are the subjects that have the \code{_MR1} and \code{_MR2} directories.
#'
#' @param subjects_dir character string, path to a single recon-all longitudinal output dir from FreeSurfer.
#'
#' @keywords internal
fslong.subjects.detect <- function(subjects_dir, timepoint_names=c("_MR1", "_MR2")) {
  potential_subject_dirs_files = list.files(path=subjects_dir, pattern="_MR1$");
  is_existing_dir = dir.exists(file.path(subjects_dir, potential_subject_dirs_files));
  potential_subject_dirs = potential_subject_dirs_files[is_existing_dir];
  potential_subject_dirs = potential_subject_dirs[nchar(potential_subject_dirs) > 3L]; # After we strip the '_MR1', something has to be left.
  subjects = substring(potential_subject_dirs, 1L, nchar(potential_subject_dirs) - 4L);

  # Check whether all directories exist
  subjects_existing_dirs = c();
  for(subject in subjects) {
    sd_tp1 = paste(subject, timepoint_names[1], sep="");
    sd_tp2 = paste(subject, timepoint_names[2], sep="");
    if(dir.exists(file.path(subjects_dir, sd_tp1)) & dir.exists(file.path(subjects_dir, sd_tp2))) {
      subjects_existing_dirs = c(subjects_existing_dirs, subject);
    }
  }

  return(subjects_existing_dirs);
}

#' @title Find completely run FreeSurfer long subjects in a recon-all long output folder.
#'
#' @description This finds all subjects for which the FreeSurfer long pipeline finished. It can work without a subjects file, by scanning the directory names to find all potential subjects. It checks only whether the expected folder for each subject exists. For a subject named 'subject1' and 2 timepoints, these folders are checked for existence: subject1, subject1_MR1, subject1_MR2, subject1_MR1.long.subject1, subject1_MR2.long.subject1
#'
#' @param subjects_dir char, the recon-all long output directory
#'
#' @param subjects_to_check a vector of chars, the subject names (the cross-sectional names, without the '_MR1' or '_MR2' or 'long' suffixes). If NULL, the folder will be scanned for subjects, by looking for all '_MR1' folders and stripping the '_MR1' suffix.
#'
#' @param timepoints vector of integers, the timepoints to check. E.g., \code{c(1,2)} or \code{seq.int(2)} if you want to check scan timepoints '_MR1' and 'MR2'.
#'
#' @return a named list with entries 'subjects_okay' and 'subjects_missing_dirs'. Each of these two keys contains a vector of character strings, the respective subjects (a subset if 'subjects_to_check'). In 'subjects_okay' are all subjects for which the expected long directories were found, the rest is in 'subjects_missing_dirs'.
#'
#' @keywords internal
fslong.subjects.finished <- function(subjects_dir, subjects_to_check=NULL, timepoints=seq.int(2)) {
  if(! dir.exists(subjects_dir)) {
    stop("The subjects_dir does not exist or cannot be read.");
  }

  # Let's figure out the subjects ourselves. We scan all directories that end with '_MR1' and strip that suffix.
  if(is.null(subjects_to_check)) {
    subjects_to_check = fslong.subjects.detect(subjects_dir);
  }

  subject_okay = rep(TRUE, length(subjects_to_check));
  for(tp in timepoints) {
    tp_suffix = sprintf("_MR%d", tp);
    subject_okay[which(!dir.exists(file.path(subjects_dir, paste(subjects_to_check, tp_suffix, sep=""))))] = FALSE;

    tp_long_suffix = sprintf("_MR%d.long.%s", tp, subjects_to_check);
    subject_okay[which(!dir.exists(file.path(subjects_dir, paste(subjects_to_check, tp_long_suffix, sep=""))))] = FALSE;
  }
  return(list('subjects_okay'=subjects_to_check[subject_okay], 'subjects_missing_dirs'=subjects_to_check[!subject_okay]));
}


#' @title Check whether subjects for FS longitudinal pipeline contain data that is identical between time points.
#'
#' @inheritParams qdec.table.skeleton
#'
#' @inheritParams subject.morph.native
#'
#' @note Keep in mind that this checks on the level of the FreeSurfer reconstruction, which is not 100% deterministic. So identical raw MRI data may lead to different vertex counts in 2 runs. So this is not a final check to exclude copied raw MRI images, it only checks for copied FreeSurfer reconstructions.
#'
#' @keywords internal
qc.fslong.checkidenticaldata <- function(subjects_dir, subjects_to_check=NULL, timepoint_names=c("_MR1", "_MR2"), measure="thickness", surface="white") {
  # Let's figure out the subjects ourselves. We scan all directories that end with '_MR1' and strip that suffix.
  if(is.null(subjects_to_check)) {
    subjects_to_check = fslong.subjects.detect(subjects_dir);
  }

  suspects = c();
  maybe_okay = c()

  for(subject in subjects_to_check) {
    sd_tp1 = paste(subject, timepoint_names[1], sep="");
    sd_tp2 = paste(subject, timepoint_names[2], sep="");
    nv_tp1 = subject.num.verts(subjects_dir, sd_tp1, surface=surface);
    nv_tp2 = subject.num.verts(subjects_dir, sd_tp2, surface=surface);
    if((nv_tp1$lh == nv_tp2$lh) & (nv_tp1$rh == nv_tp2$rh)) {
      cat(sprintf("Subject '%s' has identical vertex counts for both %s native hemispheres between timepoints %s and %s.\n", subject, surface, timepoint_names[1], timepoint_names[2]));
      suspects = c(suspects, subject);
    } else {
      maybe_okay = c(maybe_okay, subject);
    }
  }
  return(list("suspects"=suspects, "maybe_okay"=maybe_okay));
}


#' @title Generate skeleton dataframe for FreeSurfer QDEC long file from subjects list.
#'
#' @param subjects_list vector of character strings, the Freesurfer subject IDs (cross-sectional names, without any suffixes like \code{_MR1, long,} etc.)
#'
#' @param isi numerical vector, the inter-scan interval for the subjects, in a unit of your choice. Typically in years.
#'
#' @param isi_name character string, the name for the isi columns. Defaults to "years".
#'
#' @param timepoint_names vector of character strings, the timepoint names. These are mandatory for QDEC, so there should be very little reason to change them. Leave along unless you know what you are doing.
#'
#' @return data.frame with 3 columns named fsid and fsid-base and 'isi_name', a data.frame to use with the \code{\link{demographics.to.qdec.table.dat}} function.
#'
#' @seealso The function \code{\link{demographics.to.qdec.table.dat}} to write the result to a QDEC file.
#'
#' @examples
#'     dem = data.frame("ID"=paste("subject", seq(5), sep=""),
#'       "age"=sample.int(20, 5)+10L, "isi"=rnorm(5, 2.0, 0.1)); #sample data.
#'     qdec.table.skeleton(dem$ID, dem$isi);
#'
#' @export
qdec.table.skeleton <- function(subjects_list, isi=rep(0.8, length(subjects_list)), isi_name="years", timepoint_names=c("_MR1", "_MR2")) {

  if(length(subjects_list) != length(isi)) {
    stop("Length of parameters 'subjects_list' and 'isi' must match.");
  }

  num_timepoints = 2L;
  num_columns = length(subjects_list) * num_timepoints;
  qdec = data.frame("fsid"=rep("?", num_columns), "fsid-base"=rep("?", num_columns), stringsAsFactors = F, check.names = F);
  qdec[[isi_name]] = rep(0.0, num_columns); # temporary, will be overwritten later.

  current_subject_index = 1L;
  for(subject in subjects_list) {
    fsid_base = subject;
    df_start_column = current_subject_index * 2L - 1L; # for this subject
    df_end_column = df_start_column + 1L;              # for this subject
    qdec$fsid[df_start_column] = paste(subject, timepoint_names[1], sep="");
    qdec$fsid[df_end_column] = paste(subject, timepoint_names[2], sep="");
    qdec$`fsid-base`[df_start_column:df_end_column] = fsid_base;
    qdec[[isi_name]][df_start_column] = 0.0; # the first scan timepoint is always at time 0.
    qdec[[isi_name]][df_end_column] = isi[current_subject_index]; # the first scan timepoint is always at time 0.
    current_subject_index = current_subject_index + 1L;
  }
  return(qdec);
}




#' @title Filter QDEC long table for subjects.
#'
#' @param qdec_file the source QDEC table
#'
#' @param subjects_list the subjects to extract from the QDEC file
#'
#' @param output_qdec_file optional character string, a file name to which to write the resulting, filtered table. If not given, no file is created.
#'
#' @return the data.frame containing the subset of subjects from the original QDEC file.
#'
#' @note This assumes that there are 2 time points per subject and warns if not all requested subjects were found.
#'
#' @keywords internal
#' @importFrom utils write.table
qdec.table.filter <- function(qdec_file, subjects_list, output_qdec_file=NULL) {
  qdd = read.table(qdec_file, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE);
  if(! "fsid-base" %in% colnames(qdd)) {
    stop("Invalid longitudinal 'qdec_file': it does not contain a column named 'fsid-base'.");
  }
  cat(sprintf("Input QDEC file '%s' contains %d rows.\n", qdec_file, nrow(qdd)));
  subset_qdd = qdd[qdd$`fsid-base` %in% subjects_list, ];
  if(! is.null(output_qdec_file)) {
    write.table(subset_qdd, file=output_qdec_file, quote = FALSE, col.names = TRUE, row.names = FALSE);
    cat(sprintf("Writing filtered table to file '%s'.\n", output_qdec_file));
  }
  num_subjects_extracted = as.integer(nrow(subset_qdd)/2L);
  if(num_subjects_extracted != length(subjects_list)) {
    warning(sprintf("Requested to extract %d subjects from QDEC table, but %d found.\n", length(subjects_list), num_subjects_extracted));
  }
  return(subset_qdd);
}



#' @title Write deepcopy list for longitudinal subjects.
#'
#' @return vector of character strings, the file entries. Set ouput_file to also write them to a file.
#'
#' @keywords internal
deepcopylist.long <- function(measures=c("thickness", "area",  "volume"), fwhms=c("5", "10", "15"), hemis=c("lh", "rh"), long_measures=c("avg", "rate", "spc", "pc1"), template="fsaverage", has_stacked_file=TRUE, output_file=NULL) {
  filelist = c();
  for(measure in measures) {
    for (hemi in hemis) {
      for(long_measure in long_measures) {
        for(fwhm in fwhms) {
          filename = sprintf("%s.long.%s-%s.fwhm%s.%s.mgh", hemi, measure, long_measure, fwhm, template);
          filelist = c(filelist, filename);
        }
      }
      if(has_stacked_file) {
        stack_filename=sprintf("%s.long.%s-stack.mgh", hemi, measure);
        filelist = c(filelist, stack_filename);
      }
    }
  }
  if(! is.null(output_file)) {
    write.table(data.frame(filelist), file=output_file, quote = FALSE, col.names = FALSE, row.names = FALSE);
    cat(sprintf("Deepcopy list with %d entries written to file '%s'.\n", length(filelist), output_file));
  }
  return(filelist);
}


