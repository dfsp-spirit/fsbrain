## camera comparison script rgl vs scimesh

To run this:

```bash
R CMD build . && R CMD INSTALL ./fsbrain_0.6.1.tar.gz && Rscript examples/rgl_vs_scimesh/camera_verification.R
```

Then compare output files, especially dimensions and perspective.

For dimension, you could also use `identify` on linux command line (from image magick).