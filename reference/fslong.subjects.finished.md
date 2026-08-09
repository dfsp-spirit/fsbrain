# Find completely run FreeSurfer long subjects in a recon-all long output folder.

This finds all subjects for which the FreeSurfer long pipeline finished.
It can work without a subjects file, by scanning the directory names to
find all potential subjects. It checks only whether the expected folder
for each subject exists. For a subject named 'subject1' and 2
timepoints, these folders are checked for existence: subject1,
subject1_MR1, subject1_MR2, subject1_MR1.long.subject1,
subject1_MR2.long.subject1

## Usage

``` r
fslong.subjects.finished(
  subjects_dir,
  subjects_to_check = NULL,
  timepoints = seq.int(2)
)
```

## Arguments

- subjects_dir:

  char, the recon-all long output directory

- subjects_to_check:

  a vector of chars, the subject names (the cross-sectional names,
  without the '\_MR1' or '\_MR2' or 'long' suffixes). If NULL, the
  folder will be scanned for subjects, by looking for all '\_MR1'
  folders and stripping the '\_MR1' suffix.

- timepoints:

  vector of integers, the timepoints to check. E.g., `c(1,2)` or
  `seq.int(2)` if you want to check scan timepoints '\_MR1' and 'MR2'.

## Value

a named list with entries 'subjects_okay' and 'subjects_missing_dirs'.
Each of these two keys contains a vector of character strings, the
respective subjects (a subset if 'subjects_to_check'). In
'subjects_okay' are all subjects for which the expected long directories
were found, the rest is in 'subjects_missing_dirs'.
