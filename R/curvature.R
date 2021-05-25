# Functions to compute various curvature-based mesh geometry measures or shape descriptors.
# These function assume you already have the 2 principal curvatures at each vertex. If not, run mris_curvature or use some mesh library to get them.
# See Shimony et al. (2016). Comparison of cortical folding measures for evaluation of developing human brain. Neuroimage, 125, 780-790.


# One could also compute the curvatures directly in R using Rvcg:
# sf = freesurferformats::read.fs.surface("~/data/tim_only/tim/surf/lh.white")
# m3 = fs.surface.to.tmesh3d(sf);
# cr = Rvcg::vcgCurve(m3);
# k1 = cr$K1;
# k2 = cr$K2;
# fsbrain::vis.data.on.subject("~/data/tim_only", "tim", morph_data_lh=(k1+k2), rglactions = list('trans_fun'=fsbrain::clip.data));


#' @title Compute the k1 and k2 principal curvatures of a mesh.
#'
#' @param surface an fs.surface instance, as returned by \code{subject.surface}.
#'
#' @return named list, the entries 'K1' and 'K2' contain the principal curvatures.
#'
#' @note Require the optional dependency package 'Rvcg'.
#'
#' @export
surface.curvatures <- function(surface) {
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        tmesh = fs.surface.to.tmesh3d(surface);
        return(Rvcg::vcgCurve(tmesh));
    } else {
        stop("The 'Rvcg' package must be installed for this feature.");
    }
}


#' @title Computes principal curvatures according to 2 definitions from raw k1 and k2 values.
#'
#' @param k1_raw numerical vector, one of the two principal curvatures, one value per vertex
#'
#' @param k2_raw numerical vector, the other one of the two principal curvatures, one value per vertex
#'
#' @note To obtain k1_raw and k2_raw, use \code{surface.curvatures} to compute it from a mesh, or load the FreeSurfer files \code{'surf/?h.white.max'} and \code{'surf/?h.white.min'}.
#'
#' @return a named 'principal_curvatures' list, with entries 'principal_curvature_k1': larger value of k1_raw, k2_raw. 'principal_curvature_k2': smaller value of k1_raw, k2_raw. 'principal_curvature_k_major': larger value of abs(k1_raw), abs(k2_raw). 'principal_curvature_k_minor': smaller value of abs(k1_raw), abs(k2_raw).
#' @export
principal.curvatures <- function(k1_raw, k2_raw) {
     nv = length(k1_raw);
     if(nv != length(k2_raw)) {
         stop(sprintf("Vectors k1_raw and k2_raw must have same lengths, but found %d and %d.\n", nv, length(k2_raw)));
     }
     res_list = list();
     res_list$principal_curvature_k1 = ifelse(k1_raw >= k2_raw, k1_raw, k2_raw);
     res_list$principal_curvature_k2 = ifelse(k1_raw < k2_raw, k1_raw, k2_raw);
     res_list$principal_curvature_k_major = ifelse(abs(k1_raw) >= abs(k2_raw), k1_raw, k2_raw);
     res_list$principal_curvature_k_minor = ifelse(abs(k1_raw) < abs(k2_raw), k1_raw, k2_raw);
     class(res_list) = c(class(res_list), 'principal_curvatures');
     return(res_list);
}


#' @title Get all shape descriptor names.
#'
#' @return vector of character strings, the names
#' @export
shape.descriptor.names <- function() {
    all_measures = c('principal_curvature_k1', 'principal_curvature_k2', 'principal_curvature_k_major', 'principal_curvature_k_minor', 'mean_curvature', 'gaussian_curvature', 'intrinsic_curvature_index', 'negative_intrinsic_curvature_index', 'gaussian_l2_norm', 'absolute_intrinsic_curvature_index', 'mean_curvature_index', 'negative_mean_curvature_index' ,'mean_l2_norm', 'absolute_mean_curvature_index', 'folding_index', 'curvedness_index', 'shape_index', 'shape_type', 'area_fraction_of_intrinsic_curvature_index', 'area_fraction_of_negative_intrinsic_curvature_index', 'area_fraction_of_mean_curvature_index', 'area_fraction_of_negative_mean_curvature_index', 'sh2sh', 'sk2sk');
    return(all_measures);
}





#' @title Computes geometric curvature-based descriptors.
#'
#' @param pc a 'principal_curvatures' data list, see \code{\link{principal.curvatures}} for details.
#'
#' @param descriptors vector of character strings, the descriptors you want. See \code{\link{shape.descriptor.names}} for all available names.
#'
#' @references Shimony et al. (2016). Comparison of cortical folding measures for evaluation of developing human brain. Neuroimage, 125, 780-790.
#'
#' @return dataframe of descriptor values, each columns contains one descriptor.
#' @export
shape.descriptors <- function(pc, descriptors=shape.descriptor.names()) {
    all_measures = shape.descriptor.names();
    df = data.frame("principal_curvature_k1"=pc$principal_curvature_k1, "principal_curvature_k2"=pc$principal_curvature_k2, "principal_curvature_k_major"=pc$principal_curvature_k_major, "principal_curvature_k_minor"=pc$principal_curvature_k_minor);
    for(measure in descriptors) {
        if(! measure %in% all_measures) {
            stop(sprintf("Invalid curvature measure: '%s'.\n", measure));
        }
        if(measure %in% c('principal_curvature_k1', 'principal_curvature_k2', 'principal_curvature_k_major', 'principal_curvature_k_minor')) {
            next;
        }
        if(measure == 'gaussian_curvature') {
            df[[measure]] = gm.gaussian_curvature(pc);
        } else if(measure == 'mean_curvature') {
            df[[measure]] = gm.mean_curvature(pc);
        } else if(measure == 'intrinsic_curvature_index') {
            df[[measure]] = gm.intrinsic_curvature_index(pc);
        } else if(measure == 'negative_intrinsic_curvature_index') {
            df[[measure]] = gm.negative_intrinsic_curvature_index(pc);
        } else if(measure == 'absolute_intrinsic_curvature_index') {
            df[[measure]] = gm.absolute_intrinsic_curvature_index(pc);
        } else if(measure == 'mean_curvature_index') {
            df[[measure]] = gm.mean_curvature_index(pc);
        } else if(measure == 'negative_mean_curvature_index') {
            df[[measure]] = gm.negative_mean_curvature_index(pc);
        } else if(measure == 'absolute_mean_curvature_index') {
            df[[measure]] = gm.absolute_mean_curvature_index(pc);
        } else if(measure == 'mean_l2_norm') {
            df[[measure]] = gm.mean_l2_norm(pc);
        } else if(measure == 'gaussian_l2_norm') {
            df[[measure]] = gm.gaussian_l2_norm(pc);
        } else if(measure == 'folding_index') {
            df[[measure]] = gm.folding_index(pc);
        } else if(measure == 'curvedness_index') {
            df[[measure]] = gm.curvedness_index(pc);
        } else if(measure == 'area_fraction_of_mean_curvature_index') {
            df[[measure]] = gm.area_fraction_of_mean_curvature_index(pc);
        } else if(measure == 'area_fraction_of_negative_mean_curvature_index') {
            df[[measure]] = gm.area_fraction_of_negative_mean_curvature_index(pc);
        } else if(measure == 'area_fraction_of_intrinsic_curvature_index') {
            df[[measure]] = gm.area_fraction_of_intrinsic_curvature_index(pc);
        } else if(measure == 'area_fraction_of_negative_intrinsic_curvature_index') {
            df[[measure]] = gm.area_fraction_of_negative_intrinsic_curvature_index(pc);
        } else if(measure == 'sh2sh') {
            df[[measure]] = gm.sh2sh(pc);
        } else if(measure == 'sk2sk') {
            df[[measure]] = gm.sk2sk(pc);
        } else if(measure == 'shape_index') {
            df[[measure]] = gm.shape_index(pc);
        } else if(measure == 'shape_type') {
            df[[measure]] = gm.shape_type(pc);
        } else {
            stop(sprintf("Unhandled descriptor: '%s'\n", measure));   # nocov
        }
    }
    return(df);
}



#' @keywords internal
gm.gaussian_curvature <- function(pc) { return(pc$principal_curvature_k_major * pc$principal_curvature_k_minor) };


#' @keywords internal
gm.mean_curvature <- function(pc) { return((pc$principal_curvature_k_major + pc$principal_curvature_k_minor) / 2.0) };


#' @keywords internal
gm.intrinsic_curvature_index <- function(pc) { return(pmax(gm.gaussian_curvature(pc), 0.0)) };


#' @keywords internal
gm.negative_intrinsic_curvature_index <- function(pc) { return(pmin(gm.gaussian_curvature(pc), 0.0)) };


#' @keywords internal
gm.gaussian_l2_norm <- function(pc) { k = gm.gaussian_curvature(pc); return(k * k); };


#' @keywords internal
gm.mean_l2_norm <- function(pc) { h = gm.mean_curvature(pc); return(h * h); };


#' @keywords internal
gm.absolute_intrinsic_curvature_index <- function(pc) { return(abs(gm.gaussian_curvature(pc))) };


#' @keywords internal
gm.mean_curvature_index <- function(pc) { return(pmax(gm.mean_curvature(pc), 0.0)) };


#' @keywords internal
gm.negative_mean_curvature_index <- function(pc) { return(pmin(gm.mean_curvature(pc), 0.0)) };


#' @keywords internal
gm.absolute_mean_curvature_index <- function(pc) { return(abs(gm.mean_curvature(pc))) };

#' @keywords internal
gm.folding_index <- function(pc) {
    abs_k_maj = abs(pc$principal_curvature_k_major);
    abs_k_min = abs(pc$principal_curvature_k_minor);
    return(abs_k_maj * (abs_k_maj - abs_k_min));
}


#' @keywords internal
gm.curvedness_index <- function(pc) {
    return(sqrt((pc$principal_curvature_k_major * pc$principal_curvature_k_major + pc$principal_curvature_k_minor * pc$principal_curvature_k_minor) / 2.0));
}


#' @keywords internal
gm.shape_index <- function(pc) {
    return((2.0 * pi) * atan((pc$principal_curvature_k1 + pc$principal_curvature_k2) / (pc$principal_curvature_k2 - pc$principal_curvature_k1)));
}


#' @keywords internal
gm.shape_type <- function(pc) {
    shape_type = rep(0.0, length(pc$principal_curvature_k1));
    border1 = -1.0;
    border2 = -0.5;
    border3 = 0.0;
    border4 = 0.5;
    border5 = 1.0;
    shape_index = gm.shape_index(pc);
    shape_type[shape_index >= border1 & shape_index < border2] = 1;
    shape_type[shape_index >= border2 & shape_index < border3] = 2;
    shape_type[shape_index >= border3 & shape_index < border4] = 3;
    shape_type[shape_index >= border4 & shape_index < border5] = 4;
    return(shape_type);
}


#' @keywords internal
gm.area_fraction_of_intrinsic_curvature_index <- function(pc) {
    k = gm.gaussian_curvature(pc);
    condition_one = k > 0;
    condition_zero = k <= 0;
    af = 1 * condition_one + 0 * condition_zero + k * (!(condition_one | condition_zero));
    return(af);
}


#' @keywords internal
gm.area_fraction_of_negative_intrinsic_curvature_index <- function(pc) {
    k = gm.gaussian_curvature(pc);
    condition_one = k < 0;
    condition_zero = k >= 0;
    af = 1 * condition_one + 0 * condition_zero + k * (!(condition_one | condition_zero));
    return(af);
}


#' @keywords internal
gm.area_fraction_of_mean_curvature_index <- function(pc) {
    h = gm.mean_curvature(pc);
    condition_one = h > 0;
    condition_zero = h <= 0;
    af = 1 * condition_one + 0 * condition_zero + h * (!(condition_one | condition_zero));
    return(af);
}


#' @keywords internal
gm.area_fraction_of_negative_mean_curvature_index <- function(pc) {
    h = gm.mean_curvature(pc);
    condition_one = h < 0;
    condition_zero = h >= 0;
    af = 1 * condition_one + 0 * condition_zero + h * (!(condition_one | condition_zero));
    return(af);
}


#' @keywords internal
gm.sh2sh <- function(pc) {
    mln = gm.mean_l2_norm(pc);
    amci = gm.absolute_mean_curvature_index(pc);
    return(mln / amci);
}


#' @keywords internal
gm.sk2sk <- function(pc) {
    gln = gm.gaussian_l2_norm(pc);
    aici = gm.absolute_intrinsic_curvature_index(pc);
    return(gln / aici);
}


