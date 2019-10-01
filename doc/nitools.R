## ---- eval = FALSE-------------------------------------------------------
#      library("freesurferformats");
#      cortical_thickness = read.fs.morph("study_dir/subject1/surf/lh.thickness");

## ---- eval = FALSE-------------------------------------------------------
#      library("nitools");
#      subjects_dir = path.expand("~/data/study1")
#      subjects_list = c("subject1", "subject2", "subject3", "subject4", "subject5")
#      mean_thickness_lh = group.morph.agg.native(subjects_dir, subjects_list, "thickness", "lh", agg_fun=mean)

## ---- eval = FALSE-------------------------------------------------------
#      library("nitools");
#      subjects_dir = path.expand("~/data/study1")
#      subjects_list = c("subject1", "subject2", "subject3", "subject4", "subject5")
#      mean_thickness_lh = group.morph.agg.standard(subjects_dir, subjects_list, "thickness", "lh", fwhm="10", agg_fun=mean)

## ---- eval = FALSE-------------------------------------------------------
#    subjects_dir = file.path("study_dir")
#    subjects_list = c("subject1", "subject2", "subject3")  # You may want to read them from a subjects file instead if you have many
#    data = group.multimorph.agg.native(subjects_dir, subjects_list, c("thickness", "area", "volume"), c("lh", "rh"), agg_fun = mean);

## ---- eval = FALSE-------------------------------------------------------
#  library("nitools")
#  
#  hemi = "lh"               # 'lh' or 'rh'
#  atlas = "aparc"           # an atlas, e.g., 'aparc', 'aparc.a2009s', 'aparc.DKTatlas'
#  
#  template_subjects_dir = "/Applications/freesurfer/subjects";    # Some directory where we can find fsaverage. This can be omitted if FREESURFER_HOME is set, the function will find fsaverage in there by default.
#  
#  # One can also retrieve all region names of an atlas. This would get all 36 aparc regions:
#  region_names_aparc = nitools::get.atlas.region.names('aparc', template_subjects_dir=template_subjects_dir);
#  region_value_list = as.list(rnorm(length(region_names_aparc), mean=5, sd=1.5)); # assign some random normal values for this example. One would put effect size values or whatever here. The order of values has to match the order of the region names.
#  names(region_value_list) = region_names_aparc;    # Assign the names to the values.
#  
#  ret = nitools::fs.write.region.values.fsaverage(hemi, atlas, region_value_list, output_file="/tmp/spread.mgz", template_subjects_dir=template_subjects_dir, show_freeview_tip=TRUE);

## ---- eval = FALSE-------------------------------------------------------
#      library("nitools");
#      library("freesurferformats");   # a dependency of nitools, so you have it already
#      library("rgl");
#  
#      sample_subject_dir = file.path(Sys.getenv("FREESURFER_HOME"), "subjects", "bert");       # or whatever you prefer
#  
#      surf = fs.read.surface(file.path(sample_subject_dir, "surf", "lh.white"));
#      annot = read.fs.annot(file.path(sample_subject_dir, "label", "lh.aparc.annot"));

## ---- eval = FALSE-------------------------------------------------------
#  mesh = tmesh3d(unlist(surf$vertices), unlist(surf$faces), homogeneous=FALSE);
#  rgl.open();
#  col = annot$hex_colors_rgb;
#  wire3d(mesh, col=col, meshcolor="vertices");

