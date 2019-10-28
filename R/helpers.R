#' @title Transform first character of a string to uppercase.
#'
#' @description Transform first character of a string to uppercase. This is useful when labeling plots. Important: this function does not know about different encodings, languages or anything, it just calls toupper() for the first character.
#'
#' @param word, string. Any string.
#'
#' @return string, the input string with the first character transformed to uppercase.
#'
#' @examples
#'    word_up = fup("word");
#'
#' @export
fup <- function(word) {
  substr(word, 1, 1) <- toupper(substr(word, 1, 1));
  return(word);
}


#' @title Clip data at quantiles.
#'
#' @description Set all data values outside the given quantile range to the border values. This is usefull to visualize morphometry data with outliers.
#'
#' @param data, numeric vector. The input data.
#'
#' @param lower, numeric. The probability for the lower quantile, defaults to 0.05.
#'
#' @param upper, numeric. The probability for the upper quantile, defaults to 0.95.
#'
#' @return numeric vector. The output data.
#'
#' @examples
#'    full_data = rnorm(50, 3, 1);
#'    clipped = clip.data.at.quantiles(full_data);
#'
#' @export
clip.data.at.quantiles <- function(data, lower=0.05, upper=0.95){
    quantiles = quantile(data, c(lower, upper), na.rm = TRUE, names = FALSE);
    data[ data < quantiles[1] ] = quantiles[1];
    data[ data > quantiles[2] ] = quantiles[2];
    return(data);
}
