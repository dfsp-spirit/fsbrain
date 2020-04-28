# The fsbrain FAQ

### **Q**: What kind of input data do I need for fsbrain?

The *fsbrain* software is designed to be used with the output of the [FreeSurfer](http://freesurfer.net/) *recon-all* command and compatible software. Running recon-all on your T1w MRI scan results in a directory structure full of different files and file types for each subject. The *fsbrain* library uses knowledge on the directory layout to load the proper data.

### **Q**: Which file formats are supported? 

The *fsbrain* library uses [freesurferformats](https://github.com/dfsp-spirit/freesurferformats) to load a variety of neuroimaging file formats, including data exchange formats used by other brain imaging software. See the freesurferformats website for the full list.

### **Q**: I want to load a single file that is on my harddisk, not with the standard recon-all output directory structure. How can I load it?

You can use [freesurferformats](https://github.com/dfsp-spirit/freesurferformats) directly to load the data, then pass it to fsbrain. See the next question for an example.

### **Q**: Can I use fsbrain to visualize data from SPM12 / CAT12?

Yes, the [computational anatomy toolbox (CAT12)](http://www.neuro.uni-jena.de/cat/) writes surfaces in GIFTI format and the morphometry data in curv format, both formats are supported by *fsbrain*. After running CAT12 surface measure computation on your subject `subject1`, you should have the following files in the *surf/* subdir:

* lh.central.subject1.gii
* lh.gyrification.subject1

Try the following to visualize the gyrification data for the left hemisphere in *fsbrain*:

```
lh_surf = freesurferformats::read_nisurface('~/data/subject1_spm12/surf/lh.central.subject1.gii');
lh_gyrification = freesurferformats::read.fs.curv('~/data/subject1_spm12/surf/lh.gyrification.subject1');
vis.data.on.subject('~/data/', 'subject1_spm12', lh_gyrification, NULL, surface=lh_surf);
```
