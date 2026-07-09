#!/usr/bin/env Rscript
# Prototype: Test rgl.postscript fallback for screenshot export
# This script demonstrates the SVG fallback approach for systems without X11

library(fsbrain);

# Download example data if needed
fsbrain::download_optional_data();
subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

cat("Creating visualization...\n");

# Use subject1 with thickness data
cm = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', 
                               rglactions = list('no_vis' = TRUE));

cat("Testing SVG fallback approach...\n");

# Open NULL device (no X11 needed)
rgl::open3d(useNULL = TRUE);

# Render the coloredmeshes
fsbrain:::vis.coloredmeshes(cm);

# Export to SVG using rgl.postscript
svg_file = "test_fallback.svg";
rgl::rgl.postscript(svg_file, fmt = "svg");
cat(sprintf("SVG exported to: %s\n", svg_file));

rgl::close3d();

# Convert SVG to PNG using ImageMagick command line
cat("Converting SVG to PNG using ImageMagick...\n");
png_file = "test_fallback.png";
system(sprintf("convert -density 150 -background white -flatten %s %s", svg_file, png_file));

if(file.exists(png_file)) {
    info = file.info(png_file);
    cat(sprintf("PNG exported to: %s (%.1f MB)\n", png_file, info$size / 1e6));
    cat("Please check the PNG file to see if colors are rendered correctly.\n");
} else {
    cat("PNG conversion failed.\n");
}

cat("\nDone! Check the output files:\n");
cat(sprintf("  - SVG: %s (%.1f MB)\n", svg_file, file.info(svg_file)$size / 1e6));
cat(sprintf("  - PNG: %s\n", png_file));
