# hemilist functions
# a hemilist is just a named list with entries 'lh' and/or 'rh', which may contain anything.

#' @title Create a hemilist from lh and rh data.
#'
#' @description Simply runs \code{list('lh' = lh_data, 'rh' = rh_data)}: A hemilist (short for hemisphere list) is just a named list with entries 'lh' and/or 'rh', which may contain anything. Hemilists are used as parameters and return values in many \code{fsbrain} functions. The 'lh' and 'rh' keys typically contain surfaces or vectors of morphometry data.
#'
#' @param lh_data something to wrap, typically some data for a hemisphere, e.g., a vector of morphometry data values.
#'
#' @param rh_data something to wrap, typically some data for a hemisphere, e.g., a vector of morphometry data values.
#'
#' @return named list, with the 'lh_data' in the 'lh' key and the 'rh_data' in the 'rh' key.
#'
#' @family hemilist functions
#'
#' @examples
#'   lh_data = rnorm(163842, 5.0, 1.0);
#'   rh_data = rnorm(163842, 5.0, 1.0);
#'   hl = hemilist(lh_data, rh_data);
#'
#' @export
hemilist <- function(lh_data = NULL, rh_data = NULL) {
    return(list('lh' = lh_data, 'rh' = rh_data));
}

#' @title Wrap data into a named hemi list.
#'
#' @param data something to wrap, typically some data for a hemisphere, e.g., a vector of morphometry data values. If NULL, the name will not be created.
#'
#' @param hemi character string, one of 'lh' or 'rh'. The name to use for the data in the returned list.
#'
#' @param hemilist optional hemilist, an existing hemilist to add the entry to. If left at the default value `NULL`, a new list will be created.
#'
#' @return named list, with the 'data' in the name given by parameter 'hemi'
#'
#' @family hemilist functions
#'
#' @export
hemilist.wrap <- function(data, hemi, hemilist=NULL) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }
    if(is.null(hemilist)) {
        ret_list = list();
    } else {
        ret_list = hemilist;
    }
    if(!is.null(data)) {
        ret_list[[hemi]] = data;
    }
    return(ret_list);
}


#' @title Derive 'hemi' string from the data in a hemilist
#'
#' @param hemilist hemilist, an existing hemilist
#'
#' @return character string, one of 'lh', 'rh' or 'both'
#'
#' @family hemilist functions
#'
#' @export
hemilist.derive.hemi <- function(hemilist) {
    if(!is.hemilist(hemilist)) {
        stop("Parameter 'hemilist' must be a hemilist.");
    }
    if(is.null(hemilist$lh) | is.null(hemilist$rh)) {
        if(is.null(hemilist$lh)) {
            return('rh');
        } else {
            return('lh');
        }
    } else {
        return('both');
    }
}



#' @title Unwrap hemi data from a named hemi list.
#'
#' @param hemi_list named list, can have entries 'lh' and/or 'rh'
#'
#' @param hemi character string, the hemi data name to retrieve from the list. Can be NULL if the list only has a single entry.
#'
#' @param allow_null_list logical, whether to silently return NULL instead of raising an error if 'hemi_list' is NULL
#'
#' @return the data
#'
#' @family hemilist functions
#'
#' @export
hemilist.unwrap <- function(hemi_list, hemi=NULL, allow_null_list=FALSE) {
    if(is.null(hemi_list)) {
        if(allow_null_list) {
            return(NULL);
        } else {
            stop("Parameter 'hemi_list' must not be NULL unless 'allow_null_list' is TRUE.");
        }
    }
    if(! is.list(hemi_list)) {
        stop("Parameter 'hemi_list' must be a named list.");
    }
    if(length(hemi_list) < 1L) {
        stop("Parameter 'hemi_list' must not be empty.");
    }
    if(is.null(hemi)) {
        if(length(hemi_list) != 1L) {
            stop("Parameter 'hemi' can only be NULL if 'hemi_list' has exactly length 1.");
        }
        if("lh" %in% names(hemi_list)) {
            return(hemi_list$lh);
        } else if("rh" %in% names(hemi_list)) {
            return(hemi_list$rh);
        } else {
            stop("The entry in the 'hemi_list' must be named 'lh' or 'rh'.");
        }
    } else {
        if(!(hemi %in% c("lh", "rh"))) {
            stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh', or NULL but is '%s'.\n", hemi));
        }
        return(hemi_list[[hemi]]);
    }
}


#' @title Get combined data of hemi list
#'
#' @param hemi_list named list, can have entries 'lh' and/or 'rh'
#'
#' @return the data combined with \code{\link{c}}, or NULL if both entries are NULL.
#'
#' @family hemilist functions
#'
#' @export
hemilist.get.combined.data <- function(hemi_list) {
    lh_data = hemilist.unwrap(hemi_list, 'lh');
    rh_data = hemilist.unwrap(hemi_list, 'rh');
    if(is.null(lh_data) | is.null(rh_data)) {
        if(is.null(lh_data) & is.null(rh_data)) {
            return(NULL);
        } else {
            return(hemilist.unwrap(hemi_list));
        }
    } else {
        return(c(lh_data, rh_data));
    }
}


#' @title Check whether x is a hemilist
#'
#' @description A hemilist is a named list with entries 'lh' and/or 'rh'.
#'
#' @param x any R object
#'
#' @return whether 'x' is a hemilist
#'
#' @family hemilist functions
#'
#' @export
is.hemilist <- function(x) {
    return(is.list(x) & ("lh" %in% names(x) | "rh" %in% names(x)));
}
