# The fsbrain FAQ


### **Q**: Can I use fsbrain to visualize data from SPM12 / CAT12?

Yes, the computational anatomy toolbox (CAT12) writes surfaces in GIFTI format and the morphometry data in curv format, both formats are compatible with fsbrain. After running CAT12 surface measure computation on your subject `subject1`, you should have the following files in the *surf/* subdir:

* lh.central.subject1.gii
* lh.gyrification.subject1

Try the following to visualize the gyrifiaction data for the left hemisphere in *fsbrain*:

```
lh_surf = freesurferformats::read_nisurface('~/data/subject1_spm12/surf/lh.central.subject1.gii');
lh_gyrification = freesurferformats::read.fs.curv('~/data/subject1_spm12/surf/lh.gyrification.subject1');
vis.data.on.subject('~/data/', 'subject1_spm12', lh_gyrification, NULL, surface=lh_surf);
```
