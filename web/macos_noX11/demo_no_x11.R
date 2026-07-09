#!/usr/bin/env Rscript
# Demo: fsbrain export on macOS without X11 (e.g., Tahoe 26.x, Sonoma 14.x)
#
# This script demonstrates that fsbrain can produce publication-ready plots
# with colorbars even when X11/XQuartz is broken, using the automatic PDF
# fallback introduced in fsbrain 0.6.1.
#
# Under the hood:
#   1. rgl::open3d(useNULL = TRUE) opens a headless device (no X11 needed)
#   2. rgl::rgl.postscript() exports the 3D scene as PDF
#   3. ImageMagick 'convert' turns the PDF into a PNG
#
# The colorbar is a separate 2D plot (fields::image.plot) and works without X11.

library(fsbrain);

# Download example data (only needed once)
fsbrain::download_optional_data();
subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

cat("=== fsbrain macOS no-X11 demo ===\n\n");

cat("1. Creating coloredmeshes (no window opened)...\n");
cm = vis.subject.morph.native(subjects_dir, "subject1", "thickness",
                               rglactions = list("no_vis" = TRUE));

cat("\n2. Exporting 4-view layout with colorbar...\n");
cat("   Using PDF fallback (no X11 required).\n\n");

# Force NULL device to simulate no X11 (on affected macOS, rgl falls back automatically)
rgl::close3d();
rgl::open3d(useNULL = TRUE);

export(cm,
       colorbar_legend = "Cortical Thickness [mm]",
       output_img = "demo_export_4view.png",
       draw_colorbar = "horizontal",
       view_angles = c("sd_lateral_lh", "sd_lateral_rh", "sd_medial_lh", "sd_medial_rh"));

rgl::close3d();

cat("\n3. Exporting single view with colorbar...\n\n");

rgl::open3d(useNULL = TRUE);

export(cm,
       colorbar_legend = "Cortical Thickness [mm]",
       output_img = "demo_export_single.png",
       draw_colorbar = "vertical",
       view_angles = c("sd_lateral_lh"));

rgl::close3d();

cat("\n4. Interactive browser-based visualization (rglwidget)...\n");
cat("   This works on all platforms, including macOS without X11.\n");
cat("   Note: rglwidget does not include a colorbar.\n\n");

widget = vis.rglwidget(cm);
cat("   Widget created successfully.\n");

cat("\n=== Demo complete ===\n\n");
cat("Output files:\n");
for(f in c("demo_export_4view.png", "demo_export_single.png")) {
    if(file.exists(f)) {
        info = file.info(f);
        cat(sprintf("  - %s (%.2f MB)\n", f, info$size / 1e6));
    }
}
cat("\nThese plots were created WITHOUT X11, using the automatic PDF fallback.\n");
cat("They are publication-ready and include colorbars.\n");
