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



