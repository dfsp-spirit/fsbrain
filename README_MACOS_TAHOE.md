# Visualization problems on recent macOS versions

**If you are using macOS Tahoe (26.x) or macOS Sonoma (14.x) and fsbrain does not open any visualization windows or produces blank plots**, this is a known issue with the rgl package and XQuartz on these macOS versions. Recent macOS releases have changed how they handle OpenGL and X11, which breaks the graphics window that fsbrain needs to create plots.

This is not a problem with fsbrain itself, but with the underlying graphics system (rgl/XQuartz) that fsbrain relies on. The issue is being tracked in the rgl package:
- [rgl issue #488: rgl not displaying plots in XQuartz on Mac OS Tahoe](https://github.com/dmurdoch/rgl/issues/488)
- [rgl issue #423: Issue displaying 3D plots in rgl on MacOS](https://github.com/dmurdoch/rgl/issues/423)

## Workaround 1: Use browser-based visualization

You can visualize your data using `vis.rglwidget()`, which creates an interactive 3D view in your web browser instead of an X11 window. Here's a complete example:

```r
library(fsbrain);

# Download example data (only needed once)
fsbrain::download_optional_data();
fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

# Create visualization data without opening a window
cm = vis.subject.morph.standard(subjects_dir, 'fsaverage', 'sulc', 
                                 surface = 'inflated',
                                 rglactions = list('no_vis' = TRUE));

# Create interactive browser-based visualization
widget = vis.rglwidget(cm);

# Display the widget (in RStudio viewer or web browser)
widget;
```

**Note**: The `vis.rglwidget()` function provides interactive 3D viewing in the browser, but does not include colorbars.

## Workaround 2: Export plots with colorbars (automatic fallback)

Starting with fsbrain 0.6.1, the package automatically detects when X11 is not available and uses a fallback method to create plots. This means you can use the standard `export()` function and it will work even without X11:

```r
library(fsbrain);

# Download example data (only needed once)
fsbrain::download_optional_data();
fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

# Create visualization and export with colorbar
cm = vis.subject.morph.standard(subjects_dir, 'fsaverage', 'sulc', 
                                 surface = 'inflated',
                                 rglactions = list('no_vis' = TRUE));

# Export with colorbar - this now works even without X11!
export(cm, colorbar_legend = 'Sulcal depth', output_img = 'brain_plot.png');
```

Under the hood, fsbrain exports the 3D scene as a vector graphic (SVG) and converts it to PNG, bypassing the need for X11 entirely. This produces publication-ready plots with colorbars, just like on other platforms.
