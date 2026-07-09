# Visualization problems on recent macOS versions

**If you are using macOS Tahoe (26.x) or macOS Sonoma (14.x) and fsbrain does not open any visualization windows or produces blank plots**, this is a known issue with the rgl package and XQuartz on these macOS versions. Recent macOS releases have changed how they handle OpenGL and X11, which breaks the graphics window that fsbrain needs to create plots.

This is not a problem with fsbrain itself, but with the underlying graphics system (rgl/XQuartz) that fsbrain relies on. The issue is being tracked in the rgl package:
- [rgl issue #488: rgl not displaying plots in XQuartz on Mac OS Tahoe](https://github.com/dmurdoch/rgl/issues/488)
- [rgl issue #423: Issue displaying 3D plots in rgl on MacOS](https://github.com/dmurdoch/rgl/issues/423)

## Workaround: Use browser-based visualization instead

You can still visualize your data using `vis.rglwidget()`, which creates an interactive 3D view in your web browser instead of an X11 window. Here's a complete example:

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

## Important limitation

The `vis.rglwidget()` function provides interactive 3D viewing in the browser, but does not include colorbars. If you need publication-ready plots with colorbars, you will need to wait for the rgl/XQuartz issue to be fixed, or use a Linux system or older macOS version where X11 still works properly.
