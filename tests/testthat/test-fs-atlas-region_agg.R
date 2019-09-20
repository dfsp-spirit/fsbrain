test_that("Aggregation on subject level works", {
  curvfile = system.file("extdata", "lh.thickness", package = "freesurferformats", mustWork = TRUE)
  ct = freesurferformats::read.fs.curv(curvfile);

  annot_file = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = freesurferformats::read.fs.annot(annot_file);
  expect_equal(length(ct), length(annot$label_names));  # ensure the data fits together.

  # Test without explicitely requesting all possible atlas regions: regions which have no verts assigned in subject will not occur
  agg = fs.atlas.region.agg(ct, annot$label_names);
  expect_equal(nrow(agg), 35);
  expect_false("unknown" %in% agg$region);
  expect_false("corpuscallosum" %in% agg$region);
  expect_true("bankssts" %in% agg$region);

  # Test with the default names and aggregation function (mean), but this time with all possible atlas names
  agg = fs.atlas.region.agg(ct, annot$label_names, requested_label_names = annot$colortable$struct_names);
  expect_equal(class(agg), "data.frame");
  expect_equal(nrow(agg), 36);
  expect_equal(ncol(agg), 2);
  expect_equal(colnames(agg), c("region", "aggregated"));

  # first 6
  expect_true("unknown" %in% agg$region);
  expect_true("bankssts" %in% agg$region);
  expect_true("caudalanteriorcingulate" %in% agg$region);
  expect_true("caudalmiddlefrontal" %in% agg$region);
  expect_true("corpuscallosum" %in% agg$region);
  expect_true("cuneus" %in% agg$region);

  # last 4
  expect_true("frontalpole" %in% agg$region);
  expect_true("temporalpole" %in% agg$region);
  expect_true("transversetemporal" %in% agg$region);
  expect_true("insula" %in% agg$region);

  mean_bankssts = subset(agg, region=="bankssts", select=aggregated, drop=TRUE);
  expect_equal(mean_bankssts, 2.49, tolerance=1e-2);


  # Test with custom region names and aggregation function
  agg2 = fs.atlas.region.agg(ct, annot$label_names, agg_fun = max, requested_label_names=c("bankssts", "nosuchregion"));
  expect_equal(class(agg2), "data.frame");
  expect_equal(nrow(agg2), 2);   # Only the 2 explicitely requested regions should occur
  expect_equal(ncol(agg2), 2);
  expect_equal(colnames(agg2), c("region", "aggregated"));
  expect_true("nosuchregion" %in% agg2$region);
  expect_true("bankssts" %in% agg2$region);
  max_bankssts = subset(agg2, region=="bankssts", select=aggregated, drop=TRUE);
  expect_equal(max_bankssts, 3.9, tolerance=1e-2);
  max_nosuchregion = subset(agg2, region=="nosuchregion", select=aggregated, drop=TRUE);
  expect_true(is.nan(max_nosuchregion));

  # Test with incorrect input data: mismatch between vertex count and label count
  expect_error(fs.atlas.region.agg(ct, annot$label_names[1:10]), "Counts must match");
})


test_that("Aggregation on group level works", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis
    subjects_list = c("tim", "timcopy")
    measure = "thickness"
    hemi = "lh"
    atlas = "aparc"

    # Test for mean aggregation
    agg.res = fs.atlas.region.agg.group(subjects_dir, subjects_list, measure, hemi, atlas);

    expect_equal(nrow(agg.res), 2);   # 2 subjects
    expect_equal(rownames(agg.res), c("tim", "timcopy"));
    expect_equal(ncol(agg.res), 37);  # 36 atlas region columns + the 1 'subject' column
    expect_true("bankssts" %in% colnames(agg.res));
    expect_equal(class(agg.res), "data.frame");

    region_names_aparc = c('unknown', 'bankssts', 'caudalanteriorcingulate', 'caudalmiddlefrontal', 'corpuscallosum', 'cuneus', 'entorhinal', 'fusiform', 'inferiorparietal', 'inferiortemporal', 'isthmuscingulate', 'lateraloccipital', 'lateralorbitofrontal', 'lingual', 'medialorbitofrontal', 'middletemporal', 'parahippocampal', 'paracentral', 'parsopercularis', 'parsorbitalis', 'parstriangularis', 'pericalcarine', 'postcentral', 'posteriorcingulate', 'precentral', 'precuneus', 'rostralanteriorcingulate','rostralmiddlefrontal', 'superiorfrontal', 'superiorparietal', 'superiortemporal', 'supramarginal', 'frontalpole', 'temporalpole', 'transversetemporal', 'insula')
    expect_equal(length(region_names_aparc), 36);
    for (region in region_names_aparc) {
      #cat(sprintf("handling region %s\n", region))
      if(!region %in% colnames(agg.res)) {
        expect_equal(region, "missing this aparc region in agg.res result");
      }
    }

    mean_bankssts_tim = agg.res$bankssts[1]
    expect_equal(mean_bankssts_tim, 2.49, tolerance=1e-2);



    # Test for max aggregation
    agg.res = fs.atlas.region.agg.group(subjects_dir, subjects_list, measure, hemi, atlas, agg_fun = max);

    expect_equal(nrow(agg.res), 2);   # 2 subjects
    expect_equal(rownames(agg.res), c("tim", "timcopy"));

    expect_equal(ncol(agg.res), 37);  # 36 atlas region columns + the 1 'subject' column
    expect_true("unknown" %in% colnames(agg.res));
    expect_true("bankssts" %in% colnames(agg.res));
    expect_true("corpuscallosum" %in% colnames(agg.res));
    expect_true("subject" %in% colnames(agg.res));
    expect_true("caudalmiddlefrontal" %in% colnames(agg.res));
    expect_true("caudalanteriorcingulate" %in% colnames(agg.res));
    expect_true("cuneus" %in% colnames(agg.res));

    expect_equal(class(agg.res), "data.frame");

    max_bankssts_tim = agg.res$bankssts[1]
    expect_equal(max_bankssts_tim, 3.9, tolerance=1e-2);

    # Test that extracting a region_value_list from the agg.res result works
    region_value_list = fs.value.list.from.agg.res(agg.res, "tim");
    expect_equal(length(region_value_list), 36); # only the 36 regions: the 'subject' column has been removed.
    expect_equal(class(region_value_list), "list");
    expect_true("bankssts" %in% names(region_value_list));
    expect_equal(region_value_list$bankssts, 3.9, tolerance=1e-2);
})


test_that("Spreading a single value over an atlas region works from agg.res result", {
    annot_file = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
    annot = freesurferformats::read.fs.annot(annot_file);

    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip when test data missing, e.g., on travis
    subjects_list = c("tim", "timcopy")
    measure = "thickness"
    hemi = "lh"
    atlas = "aparc"

    # Test for mean aggregation
    agg.res = fs.atlas.region.agg.group(subjects_dir, subjects_list, measure, hemi, atlas);
    region_value_list = fs.value.list.from.agg.res(agg.res, "tim");

    spread = fs.spread.value.over.region(annot, region_value_list);
    new_data = spread$spread_data;
    expect_equal(class(new_data), "numeric");
    expect_equal(length(new_data), length(annot$vertices));
})

test_that("Spreading a single value over an atlas region works from manually created list", {
    # Test with handcrafted list
    annot_file = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
    annot = freesurferformats::read.fs.annot(annot_file);

    region_value_list = list("bankssts"= 0.1, "blah"= 0.3)
    spread = fs.spread.value.over.region(annot, region_value_list);
    new_data = spread$spread_data;
    expect_equal(class(new_data), "numeric");
    expect_equal(length(new_data), length(annot$vertices));
    num_verts_bankssts = 1722
    expect_equal(new_data[annot$label_names=="bankssts"], unlist(rep(0.1, num_verts_bankssts))); # all vertices in bankssts region must be 0.1
    expect_equal(new_data[annot$label_names=="superiorparietal"][1], NaN);  # value of first vertex in superiorparietal region must be NaN
    check_first_n=100
    expect_equal(new_data[annot$label_names=="paracentral"][1:check_first_n], rep(NaN, check_first_n)); # first 100 in region must be NaN
    expect_equal(new_data[annot$label_names=="middletemporal"][1:check_first_n], rep(NaN, check_first_n)); # first 100 in region must be NaN
    expect_equal(new_data[annot$label_names=="frontalpole"][1:check_first_n], rep(NaN, check_first_n)); # first 100 in region must be NaN
})

test_that("Writing MGH data spread over regions works", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip when test data missing, e.g., on travis
    subject_id = "tim"
    measure = "thickness"
    hemi = "lh"
    atlas = "aparc"
    region_value_list = list("bankssts"= 0.1, "blah"= 0.3)
    num_verts_bankssts = 1722 # valid for this subject only, ofc
    num_verts_subject1_lh = 149244

    ret = fs.write.region.values(subjects_dir, subject_id, hemi, atlas, region_value_list, "ignored", do_write_file = FALSE);
    data = ret$data;
    expect_equal(typeof(data), "double")
    expect_equal(class(data), "numeric")
    expect_equal(length(data), num_verts_subject1_lh)
    expect_equal(sum(na.omit(data)==0.1), num_verts_bankssts)
    expect_equal(sum(na.omit(data)==0.3), 0)    # blah is not a valid aparc atlas region
    expect_equal(sum(is.nan(data)), num_verts_subject1_lh - num_verts_bankssts)
})

test_that("Writing faverage region values works", {
    hemi = "lh"
    atlas = "aparc"

    region_names_aparc = c('unknown', 'bankssts', 'caudalanteriorcingulate', 'caudalmiddlefrontal', 'corpuscallosum', 'cuneus', 'entorhinal', 'fusiform', 'inferiorparietal', 'inferiortemporal', 'isthmuscingulate', 'lateraloccipital', 'lateralorbitofrontal', 'lingual', 'medialorbitofrontal', 'middletemporal', 'parahippocampal', 'paracentral', 'parsopercularis', 'parsorbitalis', 'parstriangularis', 'pericalcarine', 'postcentral', 'posteriorcingulate', 'precentral', 'precuneus', 'rostralanteriorcingulate','rostralmiddlefrontal', 'superiorfrontal', 'superiorparietal', 'superiortemporal', 'supramarginal', 'frontalpole', 'temporalpole', 'transversetemporal', 'insula')
    num_regions = length(region_names_aparc)
    region_value_list = as.list(rnorm(num_regions, mean=5, sd=1.5));
    names(region_value_list) = region_names_aparc;
    region_value_list$bankssts= 0.1;

    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip when test data missing, e.g., on travis
    skip_if_not(dir.exists(file.path(subjects_dir, 'fsaverage')), message="Test data for fsaverage missing.")

    ret = fs.write.region.values.fsaverage(hemi, atlas, region_value_list, output_file="/tmp/spread.mgz", template_subjects_dir=subjects_dir);
    data = ret$data;
    num_verts_fsaverage = 163842
    num_verts_fsaverage_bankssts = 2137
    expect_equal(typeof(data), "double")
    expect_equal(class(data), "numeric")
    expect_equal(length(data), num_verts_fsaverage)
    expect_equal(sum(na.omit(data)==0.1), num_verts_fsaverage_bankssts)
    expect_equal(sum(na.omit(data)==0.3), 0)    # blah is not a valid aparc atlas region
    expect_equal(sum(is.nan(data)), 13887)  # some vertices are outside of the regions we assigned value to
})
