# Functions for quality checking datasets.
# These functions work on group data pre-processed with recon-all. They allow one to assess the relative quality of the
# subjects/scans, and also give an overview on regions which often show problems. This is based on outliers, which are
# identified based on segmentation statistics (like surface area in a certain brain atlas region).

#' @title Perform data quality check based on a dataframe containing aggregated region-wise data.
#'
#' @description Determine subjects that potentially failed segmentation, based on region-wise data. The data can be anything, but there must be one numerical value per subject per region.
#'
#' @param rdf data.frame, the region data. The first column must contain the subject identifier, all other columns should contain numerical data for a single region. (Each row represents a subject.) This can be produced by calling \code{\link[fsbrain]{group.agg.atlas.native}}.
#'
#' @param z_threshold numerical, the cutoff value for considering a subject an outlier (in standard deviations).
#'
#' @param verbosity integer, controls the output to stdout. 0=off, 1=normal, 2=verbose.
#'
#' @param num_bad_regions_allowed integer, the number of regions in which subjects are allowed to be outliers without being reported as potentially failed segmentation
#'
#' @return named list with entries: 'failed_subjects': vector of character strings, the subject identifiers which potentially failed segmentation. 'mean_dists_z': distance to mean, in standard deviations, per subject per region. 'num_outlier_subjects_per_region': number of outlier subjects by region. 'metadata': named list of metadata, e.g., hemi, atlas and measure used to compute these QC results.
#'
#' @family quality check functions
#'
#' @export
qc.from.regionwise.df <- function(rdf, z_threshold=2.8, verbosity=0L, num_bad_regions_allowed=1L) {
    dt = rdf;

    # the column name of the subjects column contains a short table type
    # description, something like 'lh.aparc.area'.
    table_type = colnames(dt)[1];
    table_type_parts = strsplit(table_type, ".", fixed=T)[[1]];
    if(length(table_type_parts) == 3L) {
        hemi = table_type_parts[1];
        atlas = table_type_parts[2];
        measure = table_type_parts[3];
    } else {
        # no usable metadata in subjects column name.
        hemi = atlas = measure = NULL;
    }
    metadata = list('hemi'=hemi, 'atlas'=atlas, 'measure'=measure);

    # Separate subjects column so we have an all-numerical data.frame
    subjects = dt[[table_type]];
    dt[[table_type]] = NULL;

    region_means = as.list(mapply(mean, dt));
    region_sds = as.list(mapply(sd, dt));

    mean_dists_z = dt; # data gets replaced later

    region_names = colnames(dt);
    num_outlier_subjects_per_region = list();
    for(region_name in region_names) {
        mean_dist = region_means[[region_name]] - dt[[region_name]];
        mean_dist_z = mean_dist / region_sds[[region_name]];

        mean_dist_z[is.nan(mean_dist_z)] = 0;

        mean_dists_z[[region_name]] = mean_dist_z;

        outlier_subjects_pos = subjects[which(mean_dist_z > z_threshold)]
        outlier_subjects_neg = subjects[which(mean_dist_z < -z_threshold)]
        num_pos = length(outlier_subjects_pos);
        num_neg = length(outlier_subjects_neg);
        num_outlier_subjects_per_region[[region_name]] = (num_pos + num_neg);
        if((num_pos + num_neg) > 0) {
            if(verbosity >= 2L) {
                cat(sprintf("Region '%s': found %d outlier subjects (%d pos, %d neg).\n", region_name, num_pos + num_neg, num_pos, num_neg));
            }
        }
    }

    # a matrix, the rows are subjects, the columns represent the regions and contain a 1 if the subjects is an outlier for that region, a 0 otherwise.
    subject_is_outlier_in_region = matrix((as.integer(mean_dists_z > z_threshold | mean_dists_z < -z_threshold)), ncol=ncol(dt));

    # count for each subject in how many regions it is an outlier
    subject_num_outlier_regions = rowSums(subject_is_outlier_in_region, na.rm = TRUE);

    potentially_failed_subjects = c();

    for(subject_idx in seq(length(subjects))) {
        subject_id = subjects[subject_idx];
        num_outlier_regions = subject_num_outlier_regions[subject_idx];
        if(num_outlier_regions > 0L) {
            outlier_regions = region_names[subject_is_outlier_in_region[subject_idx,] == 1L];
            if(verbosity >= 2L) {
                cat(sprintf("Subject '%s' (# %d) is an outlier in %d regions: %s.\n", subject_id, subject_idx, num_outlier_regions, paste(outlier_regions, collapse=', ')));
            } else if(verbosity >= 1L) {
                cat(sprintf("Subject '%s' (# %d) is an outlier in %d regions.\n", subject_id, subject_idx, num_outlier_regions));
            }
            if(num_outlier_regions > num_bad_regions_allowed) {
                potentially_failed_subjects = c(potentially_failed_subjects, subject_id);
            }
        }
    }
    return(list("failed_subjects"=potentially_failed_subjects, "mean_dists_z"=mean_dists_z, "num_outlier_subjects_per_region"=num_outlier_subjects_per_region, "metadata"=metadata));
}


#' @title Perform data quality check based on a segstats table.
#'
#' @param filepath path to input file, a tab-separated file generated by running the FreeSurfer tools 'aparcstats2table' or 'asegstats2table'. The command line in the system shell would be something like 'aparcstats2table_bin --subjectsfile $subjects_file --meas $measure --hemi $hemi -t $aparc_output_table'.
#'
#' @param ... parameters passed to \code{\link[fsbrain]{qc.from.regionwise.df}}.
#'
#' @return see \code{\link[fsbrain]{qc.from.regionwise.df}}
#'
#' @family quality check functions
#'
#' @keywords internal
qc.from.segstats.table <- function(filepath, ...) {
    rdf = read.table(filepath, sep='\t', header=TRUE, stringsAsFactors=FALSE);
    return(qc.from.regionwise.df(rdf, ...));
}


#' @title Perform data quality check based on a segstats table.
#'
#' @description Determine subjects that potentially failed segmentation, based on segstats table data. The input table file must be a segmentation or parcellation table, generated by running the FreeSurfer tools 'aparcstats2table' or 'asegstats2table' for your subjects.
#'
#' @inheritParams qc.from.segstats.table
#'
#' @param filepath_lh path to left hemisphere input file, a tab-separated file generated by running the FreeSurfer tools 'aparcstats2table' or 'asegstats2table'. The command line in the system shell would be something like 'aparcstats2table_bin --subjectsfile $subjects_file --meas $measure --hemi $hemi -t $aparc_output_table'.
#'
#' @param filepath_rh path to equivalent right hemisphere input file.
#'
#' @return qc result as a hemilist, each entry contains a named list as returned by \code{\link[fsbrain]{qc.from.regionwise.df}}.
#'
#' @export
qc.from.segstats.tables <- function(filepath_lh, filepath_rh, ...) {
    if(is.null(filepath_lh) & is.null(filepath_lh)) {
        stop("No more than one of 'filepath_lh' and 'filepath_rh' can be NULL.");
    }
    res_hl = list();
    if(! is.null(filepath_lh)) {
        res_hl$lh = qc.from.segstats.table(filepath_lh, ...);
    }
    if(! is.null(filepath_rh)) {
        res_hl$rh = qc.from.segstats.table(filepath_rh, ...);
    }
    return(res_hl);
}


#' @title Perform data quality check based on computed region stats.
#'
#' @description Determine subjects that potentially failed segmentation, based on region-wise morphometry data. The stats can be computed from any kind of data, but something like area or volume most likely works best. The stats are based on the mean of the region values, so the measure should at least roughly follow a normal distribution.
#'
#' @inheritParams group.agg.atlas.native
#'
#' @param ... parameters passed to \code{\link[fsbrain]{qc.from.regionwise.df}}.
#'
#' @return qc result as a hemilist, each entry contains a named list as returned by \code{\link[fsbrain]{qc.from.regionwise.df}}.
#'
#' @family quality check functions
#'
#' @export
qc.for.group <- function(subjects_dir, subjects_list, measure, atlas, hemi='both', ...) {
    if( ! hemi %in% c('lh', 'rh', 'both')) {
        stop("Parameter 'hemi' must be one of 'lh', 'rh', or 'both'.");
    }
    res_hl = list();
    if(hemi %in% c('lh', 'both')) {
        rdf = group.agg.atlas.native(subjects_dir, subjects_list, measure, 'lh', atlas);
        table_type = sprintf("%s.%s.%s", 'lh', atlas, measure);
        colnames(rdf)[1] = table_type; # store table type in column name of subjects column (FreeSurfer style).
        res_hl$lh = qc.from.regionwise.df(rdf, ...);
    }
    if(hemi %in% c('lh', 'both')) {
        rdf = group.agg.atlas.native(subjects_dir, subjects_list, measure, 'rh', atlas);
        table_type = sprintf("%s.%s.%s", 'rh', atlas, measure);
        colnames(rdf)[1] = table_type; # store table type in column name of subjects column (FreeSurfer style).
        res_hl$rh = qc.from.regionwise.df(rdf, ...);
    }
    return(res_hl);
}


#' @title Visualize the number of outlier subjects per region in your dataset.
#'
#' @description The function helps you to see which regions are affected the most by QC issues: for each region, it plots the number of subjects which are outliers in the region.
#'
#' @param qc_res hemilist of QC results, as returned by functions like \code{\link[fsbrain]{qc.for.group}} or \code{\link[fsbrain]{qc.from.segstats.tables}}.
#'
#' @inheritParams vis.region.values.on.subject
#'
#' @param ... extra parameters passed to \code{\link[fsbrain]{vis.region.values.on.subject}}. E.g., to change to interactive view, get a colorbar and better resolution, try: \code{draw_colorbar=T, rgloptions = rglo(), views='si'}.
#'
#' @note You can visualize this on any subject you like, 'fsaverage' is a typical choice. The atlas must be the one used during the QC step.
#'
#' @export
qc.vis.failcount.by.region <- function(qc_res, atlas, subjects_dir=fsaverage.path(), subject_id='fsaverage', ...) {
    lh_data = getIn(qc_res, c('lh' , 'num_outlier_subjects_per_region'));
    rh_data = getIn(qc_res, c('rh' , 'num_outlier_subjects_per_region'));
    num_colors_needed = max(as.integer(lh_data), as.integer(rh_data));
    makecmap_options = mkco.heat();
    makecmap_options$n = num_colors_needed;
    return(invisible(vis.region.values.on.subject(subjects_dir, subject_id, atlas, lh_region_value_list=lh_data, rh_region_value_list=rh_data, makecmap_options=makecmap_options, ...)));
}

