# Functions related to cortical parcellations.


#' @title Extract a region from an annotation as a label.
#'
#' @description The returned label can be used to mask morphometry data, e.g., to set the values of a certain region to `NaN` or to extract only values from a certain region.
#'
#' @param annotdata, annotation. An annotation for one hemisphere, as returned by \code{\link[fsbrain]{subject.annot}}. This must be the loaded data, not a path to a file.
#'
#' @param region, string. A valid region name for the annotation, i.e., one of the regions of the atlas used to create the annotation.
#'
#' @param return_one_based_indices, logical. Whether the indices should be 1-based. Indices are stored zero-based in label files, but R uses 1-based indices. Defaults to TRUE.
#'
#' @param invert, logical. If TRUE, return the indices of all vertices which are NOT part of the region. Defaults to FALSE.
#'
#' @param error_on_invalid_region, logical. Whether to throw an error if the given region does not appear in the region list of the annotation. If set to FALSE, this will be ignored and an empty vertex list will be returned. Defaults to TRUE.
#'
#' @return integer vector with label data: the list of vertex indices in the label. See 'return_one_based_indices' for important information.
#'
#' @family atlas functions
#'
#' @export
label.from.annotdata <- function(annotdata, region, return_one_based_indices=TRUE, invert=FALSE, error_on_invalid_region=TRUE) {

    if(error_on_invalid_region) {
        if(!(region %in% annotdata$label_names)) {
            stop(sprintf("Region '%s' is not a valid region for the given annotation (which has %d regions). Set 'error_on_invalid_region' to FALSE to avoid this error.\n", region, annotdata$colortable$num_entries));
        }
    }

    if(invert) {
        indices = which(annotdata$label_names != region);
    } else {
        indices = which(annotdata$label_names == region);
    }

    if(!return_one_based_indices) {
        indices = indices - 1;
    }

    return(indices);
}


#' @title Extract a region from an atlas annotation as a label for a subject.
#'
#' @description The returned label can be used to mask morphometry data, e.g., to set the values of a certain region to NaN or to extract only values from a certain region.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param region, string. A valid region name for the annotation, i.e., one of the regions of the atlas.
#'
#' @param return_one_based_indices, logical. Whether the indices should be 1-based. Indices are stored zero-based in label files, but R uses 1-based indices. Defaults to TRUE.
#'
#' @param invert, logical. If TRUE, return the indices of all vertices which are NOT part of the region. Defaults to FALSE.
#'
#' @param error_on_invalid_region, logical. Whether to throw an error if the given region does not appear in the region list of the annotation. If set to FALSE, this will be ignored and an empty vertex list will be returned. Defaults to TRUE.
#'
#' @return integer vector with label data: the list of vertex indices in the label.
#'
#' @family atlas functions
#'
#' @export
subject.label.from.annot <- function(subjects_dir, subject_id, hemi, atlas, region, return_one_based_indices=TRUE, invert=FALSE, error_on_invalid_region=TRUE) {
    annot = subject.annot(subjects_dir, subject_id, hemi, atlas);
    return(label.from.annotdata(annot, region, return_one_based_indices=return_one_based_indices, invert=invert, error_on_invalid_region=error_on_invalid_region));
}

#' @title Extract a region from an atlas annotation as a label for a group of subjects.
#'
#' @description The returned label can be used to mask morphometry data, e.g., to set the values of a certain region to NaN or to extract only values from a certain region.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, vector of string. The subject identifiers.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param region, string. A valid region name for the annotation, i.e., one of the regions of the atlas.
#'
#' @param return_one_based_indices, logical. Whether the indices should be 1-based. Indices are stored zero-based in label files, but R uses 1-based indices. Defaults to TRUE.
#'
#' @param invert, logical. If TRUE, return the indices of all vertices which are NOT part of the region. Defaults to FALSE.
#'
#' @param error_on_invalid_region, logical. Whether to throw an error if the given region does not appear in the region list of the annotation. If set to FALSE, this will be ignored and an empty vertex list will be returned. Defaults to TRUE.
#'
#' @return named list of integer vectors with label data: for each subject, the list of vertex indices in the label.
#'
#' @family atlas functions
#'
#' @export
group.label.from.annot <- function(subjects_dir, subjects_list, hemi, atlas, region, return_one_based_indices=TRUE, invert=FALSE, error_on_invalid_region=TRUE) {
    group_annots = group.annot(subjects_dir, subjects_list, hemi, atlas);
    group_labels = lapply(group_annots, label.from.annotdata, region, return_one_based_indices=return_one_based_indices, invert=invert, error_on_invalid_region=error_on_invalid_region);
    return(group_labels);
}



#' @title Merge several labels into an annotation
#'
#' @description Merge several labels and a colortable into an annotation.
#'
#' @param label_vertices_by_region named list of integer vectors, the keys are strings which define region names, and the values are integer vectors: the vertex indices of the region.
#'
#' @param colortable_df NULL or dataframe, a colortable. It must contain the columns 'struct_name', 'r', 'g', 'b', and 'a'. All other columns will be derived if missing. The entries in 'struct_name' must match keys from the 'label_vertices_by_region' parameter. There must be one more row in here than there are labels. This row identifies the 'unknown' region (see also parameter 'index_of_unknown_region'). If NULL, a colortable will be auto-generated.
#'
#' @param num_vertices_in_surface integer, total number of vertices in the surface mesh
#'
#' @param index_of_unknown_region positive integer, the index of the row in 'colortable_df' that defines the 'unknown' or 'background' region to which all vertices will be assigned which are *not* part of any of the given labels.
#'
#' @return an annotation, see \code{\link[freesurferformats]{read.fs.annot}} for details.
#'
#' @family atlas functions
#'
#' @examples
#'   # Create two labels. Real-word labels would have more vertices, of course.
#'   label1 = c(46666, 467777);
#'   label2 = c(99888, 99889);
#'   label_vertices = list("region1"=label1, "region2"=label2);
#'   colortable_df = data.frame("struct_index"=seq(0, 2),
#'    "struct_name"=c("unknown", "region1", "region2"),
#'    "r"=c(255L, 255L, 0L), "g"=c(255L, 0L, 255L), "b"=c(255L, 0L, 0L), "a"=c(0L, 0L, 0L));
#'   annot = label.to.annot(label_vertices, 100000, colortable_df);
#'
#' @importFrom grDevices rgb col2rgb
#' @importFrom squash rainbow2
#' @export
label.to.annot <- function(label_vertices_by_region, num_vertices_in_surface, colortable_df=NULL, index_of_unknown_region=1L) {

    if('unknown' %in% names(label_vertices_by_region)) {
        stop("The region name 'unknown' must not occur in names of parameter 'label_vertices_by_region'.");
    }

    if(is.null(colortable_df)) {
        # Automatically generate a colortable_df
        num_regions_including_unknown = length(label_vertices_by_region) + 1L;
        rgb255colors = grDevices::col2rgb(squash::rainbow2(num_regions_including_unknown), alpha = TRUE);
        r = rgb255colors[1,];
        g = rgb255colors[2,];
        b = rgb255colors[3,];
        a = rgb255colors[4,];
        colortable_df = data.frame("struct_index"=seq(0, num_regions_including_unknown-1L), "struct_name"=c("unknown", names(label_vertices_by_region)), "r"=r, "g"=g, "b"=b, "a"=a);

    } else {
        if(length(label_vertices_by_region) != nrow(colortable_df) - 1L) {
            stop(sprintf("Number of regions in 'label_vertices_by_region' (%d) must match number of rows in colortable minus one (is %d).\n", length(label_vertices_by_region), nrow(colortable_df)));
        }
    }

    num_vertices_in_surface = as.integer(num_vertices_in_surface);
    if(num_vertices_in_surface <= 0L) {
        stop(sprintf("Parameter 'num_vertices_in_surface' must be a positive integer but is '%d'.\n", num_vertices_in_surface));
    }

    # We are friendly and do not require all redundant information in the colortable to be filled in, it is computed from the existing information.
    if(is.null(colortable_df$code)) {
        colortable_df$code = colortable_df$r + colortable_df$g*2^8 + colortable_df$b*2^16 + colortable_df$a*2^24;
    }
    if(is.null(colortable_df$num_entries)) {
        colortable_df$num_entries = nrow(colortable_df);
    }
    if(is.null(colortable_df$struct_index)) {
        colortable_df$struct_index = seq(0L, colortable_df$num_entries - 1L);
    }
    if(is.null(colortable_df$hex_color_string_rgb)) {
        colortable_df$hex_color_string_rgb = grDevices::rgb(colortable_df$r/255., colortable_df$g/255., colortable_df$b/255.);
    }
    if(is.null(colortable_df$hex_color_string_rgba)) {
        colortable_df$hex_color_string_rgba = grDevices::rgb(colortable_df$r/255., colortable_df$g/255., colortable_df$b/255., colortable_df$a/255);
    }

    colortable = list("num_entries"=colortable_df$num_entries, "struct_names"=colortable_df$struct_name, "table"=matrix(c(colortable_df$r, colortable_df$g, colortable_df$b, colortable_df$a, colortable_df$code), ncol=5));

    # Compute the code for each vertex
    label_codes = rep(as.integer(index_of_unknown_region), num_vertices_in_surface);
    label_names = rep("", num_vertices_in_surface);
    hex_colors_rgb = rep("#333333", num_vertices_in_surface);
    for(label_region in names(label_vertices_by_region)) {
        if(label_region %in% colortable_df$struct_name) {
            region_code = subset(colortable_df, colortable_df$struct_name == label_region)$code;
            if(length(region_code) != 1L) {
                stop(sprintf("Could not determine unique code for region '%s', found %d codes.\n", label_region, length(region_code)));
            }
            label_vertices = label_vertices_by_region[[label_region]];
            if(length(label_vertices) >= 1L) {
                label_codes[label_vertices] = region_code;
                label_names[label_vertices] = subset(colortable_df, colortable_df$struct_name == label_region)$struct_name;
                hex_colors_rgb[label_vertices] = subset(colortable_df, colortable_df$struct_name == label_region)$hex_color_string_rgb;
            }
        } else {
            stop(sprintf("Vertices for region '%s' cannot be assigned a code: region does not occur in colortable_df column 'struct_name'.\n", label_region));
        }
    }

    # Derive the label names and colors from the label codes

    annot = list("colortable_df"=colortable_df, "colortable"=colortable, "vertices"=seq(0L, as.integer(num_vertices_in_surface) - 1L), "label_codes"=label_codes, "label_names"=label_names, "hex_colors_rgb"=hex_colors_rgb);

}

