#!/usr/bin/env Rscript

library("fsbrain");
download_optional_data();
download_fsaverage(accept_freesurfer_license = TRUE);
subjects_dir = get_optional_data_filepath("subjects_dir");

fsbrain.set.default.figsize(3000, 3000);

#coloredmeshes = vis.subject.morph.standard(subjects_dir, "subject1", "sulc", cortex_only=TRUE, views=NULL);
#export(coloredmeshes, background_color = "black");

rglactions = list("snapshot_png"="fsbrain.png");
vis.subject.morph.standard(subjects_dir, "subject1", "sulc", cortex_only=TRUE, rglactions = rglactions);
