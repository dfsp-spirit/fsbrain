#' @title Download optional data for this package if required.
#'
#' @description Ensure that the optioanl data is available locally in the package cache. Will try to download the data only if it is not available. This data is not required for the package to work, but it is used in the examples, in the unit tests and also in the example code from the vignette. Downloading it is highly recommended.
#'
#' @return Named list. The list has entries: "available": vector of strings. The names of the files that are available in the local file cache. You can access them using get_optional_data_file(). "missing": vector of strings. The names of the files that this function was unable to retrieve.
#'
#' @export
download_optional_data <- function() {
    pkg_info = pkgfilecache::get_pkg_info("fsbrain");

    # Replace these with your optional data files.
    base_path_subject1 = c('subjects_dir', 'subject1');
    local_filenames_subject1 = list(c(base_path_subject1, 'label', 'lh.aparc.a2009s.annot'),
                           c(base_path_subject1, 'label', 'lh.aparc.annot'),
                           c(base_path_subject1, 'label', 'lh.cortex.label'),
                           c(base_path_subject1, 'label', 'rh.aparc.a2009s.annot'),
                           c(base_path_subject1, 'label', 'rh.aparc.annot'),
                           c(base_path_subject1, 'label', 'rh.cortex.label'),
                           c(base_path_subject1, 'mri', 'brain.mgz'),
                           c(base_path_subject1, 'surf', 'lh.thickness'),
                           c(base_path_subject1, 'surf', 'lh.white'),
                           c(base_path_subject1, 'surf', 'rh.thickness'),
                           c(base_path_subject1, 'surf', 'rh.white'),
                           c(base_path_subject1, 'surf', 'lh.thickness.fwhm10.fsaverage.mgh'),
                           c(base_path_subject1, 'surf', 'rh.thickness.fwhm10.fsaverage.mgh'),
                           c(base_path_subject1, 'surf', 'lh.area'),
                           c(base_path_subject1, 'surf', 'rh.area'),
                           c(base_path_subject1, 'surf', 'lh.area.fwhm10.fsaverage.mgh'),
                           c(base_path_subject1, 'surf', 'rh.area.fwhm10.fsaverage.mgh'),
                           c(base_path_subject1, 'mri', 'aseg.mgz'),
                           c(base_path_subject1, 'mri', 'aparc+aseg.mgz'),
                           c(base_path_subject1, 'label', 'vol_midbrain.label'),
                           c(base_path_subject1, 'surf', 'lh.curv'),
                           c(base_path_subject1, 'surf', 'rh.curv'),
                           c(base_path_subject1, 'surf', 'lh.sulc'),
                           c(base_path_subject1, 'surf', 'rh.sulc'),
                           c(base_path_subject1, 'surf', 'lh.pial'),
                           c(base_path_subject1, 'surf', 'rh.pial'),
                           c(base_path_subject1, 'surf', 'lh.inflated'),
                           c(base_path_subject1, 'surf', 'rh.inflated'),
                           c(base_path_subject1, 'surf', 'lh.white.min'),
                           c(base_path_subject1, 'surf', 'rh.white.min'),
                           c(base_path_subject1, 'surf', 'lh.white.max'),
                           c(base_path_subject1, 'surf', 'rh.white.max'),
                           c(base_path_subject1, 'surf', 'lh.pial.min'),
                           c(base_path_subject1, 'surf', 'rh.pial.min'),
                           c(base_path_subject1, 'surf', 'lh.pial.max'),
                           c(base_path_subject1, 'surf', 'rh.pial.max'),
                           c(base_path_subject1, 'surf', 'lh.volume'),
                           c(base_path_subject1, 'surf', 'rh.volume'),
                           c(base_path_subject1, 'surf', 'lh.jacobian_white'),
                           c(base_path_subject1, 'surf', 'rh.jacobian_white'),
                           c(base_path_subject1, 'surf', 'lh.sulc.fwhm10.fsaverage.mgh'),
                           c(base_path_subject1, 'surf', 'rh.sulc.fwhm10.fsaverage.mgh'),
                           c(base_path_subject1, 'mri', 'T1.mgz')
                           );
    base_path_subject2 = c('subjects_dir', 'subject2');
    local_filenames_subject2 = list(c(base_path_subject2, 'label', 'lh.aparc.a2009s.annot'),
                                    c(base_path_subject2, 'label', 'lh.aparc.annot'),
                                    c(base_path_subject2, 'label', 'lh.cortex.label'),
                                    c(base_path_subject2, 'label', 'rh.aparc.a2009s.annot'),
                                    c(base_path_subject2, 'label', 'rh.aparc.annot'),
                                    c(base_path_subject2, 'label', 'rh.cortex.label'),
                                    c(base_path_subject2, 'mri', 'brain.mgz'),
                                    c(base_path_subject2, 'surf', 'lh.thickness'),
                                    c(base_path_subject2, 'surf', 'lh.white'),
                                    c(base_path_subject2, 'surf', 'rh.thickness'),
                                    c(base_path_subject2, 'surf', 'rh.white'),
                                    c(base_path_subject2, 'surf', 'lh.thickness.fwhm10.fsaverage.mgh'),
                                    c(base_path_subject2, 'surf', 'rh.thickness.fwhm10.fsaverage.mgh'),
                                    c(base_path_subject2, 'surf', 'lh.area'),
                                    c(base_path_subject2, 'surf', 'rh.area'),
                                    c(base_path_subject2, 'surf', 'lh.area.fwhm10.fsaverage.mgh'),
                                    c(base_path_subject2, 'surf', 'rh.area.fwhm10.fsaverage.mgh'),
                                    c(base_path_subject2, 'mri', 'aseg.mgz'),
                                    c(base_path_subject2, 'mri', 'aparc+aseg.mgz'),
                                    c(base_path_subject2, 'label', 'vol_midbrain.label'),
                                    c(base_path_subject2, 'surf', 'lh.curv'),
                                    c(base_path_subject2, 'surf', 'rh.curv'),
                                    c(base_path_subject2, 'surf', 'lh.sulc'),
                                    c(base_path_subject2, 'surf', 'rh.sulc'),
                                    c(base_path_subject2, 'surf', 'lh.pial'),
                                    c(base_path_subject2, 'surf', 'rh.pial'),
                                    c(base_path_subject2, 'surf', 'lh.inflated'),
                                    c(base_path_subject2, 'surf', 'rh.inflated'),
                                    c(base_path_subject2, 'surf', 'lh.white.min'),
                                    c(base_path_subject2, 'surf', 'rh.white.min'),
                                    c(base_path_subject2, 'surf', 'lh.white.max'),
                                    c(base_path_subject2, 'surf', 'rh.white.max'),
                                    c(base_path_subject2, 'surf', 'lh.pial.min'),
                                    c(base_path_subject2, 'surf', 'rh.pial.min'),
                                    c(base_path_subject2, 'surf', 'lh.pial.max'),
                                    c(base_path_subject2, 'surf', 'rh.pial.max'),
                                    c(base_path_subject1, 'surf', 'lh.volume'),
                                    c(base_path_subject2, 'surf', 'rh.volume'),
                                    c(base_path_subject2, 'surf', 'lh.jacobian_white'),
                                    c(base_path_subject2, 'surf', 'rh.jacobian_white'),
                                    c(base_path_subject2, 'surf', 'lh.sulc.fwhm10.fsaverage.mgh'),
                                    c(base_path_subject2, 'surf', 'rh.sulc.fwhm10.fsaverage.mgh'),
                                    c(base_path_subject2, 'mri', 'T1.mgz')
                                    );
    local_filenames = c(local_filenames_subject1, local_filenames_subject2);


    md5sums_subject1 = c('099e738654aedc71cd580256f4f3914b',
                'c0ac6e39e3536ef968f1051f908a80c4',
                '929873a4ae3542331d84e5a97c852824',
                '31dd188591d11784f9efed66601b9267',
                'd18d466b35f64ec5ff1c97d885095863',
                '6b650fa9076561d96cd4ac1bbb6dd55d',
                '2c4874576eb935bf9445dda0529774e0',
                '96d6350a6b158453a0231a1f01cfbd58',
                'b6d2cdb9793aae3b76c2dcbf03491988',
                '4ec315e8daa6c3bbda46c36b9188b60f',
                '8034395bc9fcfa02c05c6cf6559ab97e',
                'c58c2dcfc093f409cc1d6a431ac676de',
                '2ef44d7a40590242c2516930443712e4',
                '33ac2ccf1cd388e458e2d03fcc5cc3e6',
                '1e55e15bb468652b5b024daf37c4ec12',
                '492516bb4e1e31224dba96b2da0d07c4',
                '4b7d88484182326576218abdce8ac378',
                '148a5b715d5f0686fe8ed2143e7e1987',
                '600c8969bd6eaba8ef9cfe181d6ce17a',
                '016316901d8148a0971dbb39c7395970', # midbrain label
                'dae80996f45e1639ea81e17f4a08a879', # lh.curv
                '8e8d24c2434cbcc72729fa06cbc13b47',
                'bc268800c1cb102a43e99a3ab061ca94', # lh.sulc
                '13aec75d8712f14345688ce5ad53f648',
                'c433b8925de4f74845e57d6172d6a3c2', # lh.pial
                '78c05bd3aeacb7cdc9b1b075095209d4',
                '7e7128b115ac48fee45816567988f7ff', # lh.inflated
                '7f5aadf8f11065f379e123b723b0ef14',
                'd69337d93d377684c5a2db9fd12379d4', # lh white k2 min
                'bfd2fa8c2aee83f8f449aef9694ffb94',
                '16024b469736f1ddf7fda81596c698db', # lh white k1 max
                'b5aba6cb21c014feb546570c20ad574a',
                'c5745dfe82180a198c9968467990b6ec', # lh pial k2 min
                '0ebe2283126f09918289749745705326',
                'db277584962e30b586d0768c46adb696', # lh pial k1 max
                'a8456e694d8003a4e56efde824f6dc7a',
                'ba84cc43ab41cfd4a694656cd5b6bb20', # volume
                '94f9ea482af6f1b18038c5ebaddad2a8',
                '85ad831974e9f5d86d8b000df98a277b', # jacobian
                '6258b08da232576dd25a05d8359364c2',
                '42ab1b2e251a3ed84bb586079146e037', # lh.sulc.fwhm10.fsaverage.mgh
                '089dab3337de5cd6f9fe345908af9707',
                '24bb590cad3e091c13741b5edce2ea7d'
                );

    md5sums_subject2 = md5sums_subject1; # In our example data, subject2 is actually just a copy of subject1, so the md5 hashes are identical.
    md5sums = c(md5sums_subject1, md5sums_subject2);

    ext_url_subject_part_subject1 = 'subjects_dir/subject1/';
    ext_url_subject_part_subject2 = 'subjects_dir/subject2/';
    ext_url_parts_each_subject = c('label/lh.aparc.a2009s.annot',
                 'label/lh.aparc.annot',
                 'label/lh.cortex.label',
                 'label/rh.aparc.a2009s.annot',
                 'label/rh.aparc.annot',
                 'label/rh.cortex.label',
                 'mri/brain.mgz',
                 'surf/lh.thickness',
                 'surf/lh.white',
                 'surf/rh.thickness',
                 'surf/rh.white',
                 'surf/lh.thickness.fwhm10.fsaverage.mgh',
                 'surf/rh.thickness.fwhm10.fsaverage.mgh',
                 'surf/lh.area',
                 'surf/rh.area',
                 'surf/lh.area.fwhm10.fsaverage.mgh',
                 'surf/rh.area.fwhm10.fsaverage.mgh',
                 'mri/aseg.mgz',
                 'mri/aparc+aseg.mgz',
                 'label/vol_midbrain.label',
                 'surf/lh.curv',
                 'surf/rh.curv',
                 'surf/lh.sulc',
                 'surf/rh.sulc',
                 'surf/lh.pial',
                 'surf/rh.pial',
                 'surf/lh.inflated',
                 'surf/rh.inflated',
                 'surf/lh.white.min',
                 'surf/rh.white.min',
                 'surf/lh.white.max',
                 'surf/rh.white.max',
                 'surf/lh.pial.min',
                 'surf/rh.pial.min',
                 'surf/lh.pial.max',
                 'surf/rh.pial.max',
                 'surf/lh.volume',
                 'surf/rh.volume',
                 'surf/lh.jacobian_white',
                 'surf/rh.jacobian_white',
                 'surf/lh.sulc.fwhm10.fsaverage.mgh',
                 'surf/rh.sulc.fwhm10.fsaverage.mgh',
                 'mri/T1.mgz'
                 );
    ext_urls_subject1 = paste(ext_url_subject_part_subject1, ext_url_parts_each_subject, sep='');
    ext_urls_subject2 = paste(ext_url_subject_part_subject2, ext_url_parts_each_subject, sep='');
    ext_urls = c(ext_urls_subject1, ext_urls_subject2);

    base_url = 'http://rcmd.org/projects/nitestdata/';
    urls = paste(base_url, ext_urls, sep='');

    cfiles = pkgfilecache::ensure_files_available(pkg_info, local_filenames, urls, md5sums=md5sums);
    cfiles$file_status = NULL; # not exposed to end user
    return(invisible(cfiles));
}


#' @title Download the FreeSurfer v6 fsaverage subject.
#'
#' @description Download some relevant files from the FreeSurfer v6 fsaverage subject. The files are subject to the FreeSurfer software license, see parameter 'accept_freesurfer_license' for details. This data is not required for the package to work. If you are working on a machine that has FreeSurfer installed, you already have this data anyways and do not need to download it. If not, it is very convenient to have it if you are using the fsaverage template subject to analyze your standard space data, as it is required for visualization of such data.
#'
#' @param accept_freesurfer_license logical, whether you accept the FreeSurfer license for fsaverage, available at https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense. Defaults to FALSE.
#'
#' @return Named list. The list has entries: "available": vector of strings. The names of the files that are available in the local file cache. You can access them using get_optional_data_file(). "missing": vector of strings. The names of the files that this function was unable to retrieve.
#'
#' @export
download_fsaverage <- function(accept_freesurfer_license=FALSE) {

    if(! accept_freesurfer_license) {
        cat(sprintf("Nothing downloaded. You have to accept the FreeSurfer license to download and use fsaverage.\n"));
        cat(sprintf("Read the license at https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense and set parameter 'accept_freesurfer_license' to TRUE if you accept it.\n"));
        return(invisible(NULL));
    }

    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    base_path_fsaverage = c('subjects_dir', 'fsaverage');
    local_filenames = list(c(base_path_fsaverage, 'label', 'lh.aparc.a2009s.annot'),
                                    c(base_path_fsaverage, 'label', 'rh.aparc.a2009s.annot'),
                                    c(base_path_fsaverage, 'label', 'lh.aparc.annot'),
                                    c(base_path_fsaverage, 'label', 'rh.aparc.annot'),
                                    c(base_path_fsaverage, 'label', 'lh.cortex.label'),
                                    c(base_path_fsaverage, 'label', 'rh.cortex.label'),
                                    c(base_path_fsaverage, 'mri', 'brain.mgz'),
                                    c(base_path_fsaverage, 'surf', 'lh.white'),
                                    c(base_path_fsaverage, 'surf', 'rh.white'),
                                    c(base_path_fsaverage, 'surf', 'lh.pial'),
                                    c(base_path_fsaverage, 'surf', 'rh.pial'),
                                    c(base_path_fsaverage, 'surf', 'lh.inflated'),
                                    c(base_path_fsaverage, 'surf', 'rh.inflated'),
                                    c(base_path_fsaverage, 'surf', 'lh.curv'),
                                    c(base_path_fsaverage, 'surf', 'rh.curv'),
                                    c(base_path_fsaverage, 'ext', 'FreeSurferColorLUT.txt'),
                                    c(base_path_fsaverage, 'LICENSE'),
                                    c(base_path_fsaverage, 'surf', 'lh.sphere'),
                                    c(base_path_fsaverage, 'surf', 'rh.sphere')
    );



    md5sums = c('b4310b1e4435defaf27fc7ee98199e6a',
                '6077dc6cb42dd8c48bb382672d65743c',
                'bf0b488994657435cdddac5f107d21e8',
                '8f504caddedfde367a40501da6222809',
                '578f81e9946a76eb1c42d897d07da4a7',
                'c8f59de23e9f90f18e96e9d037e42799',
                'b8bc4b5854f2d5e66d5c4f95d4f9cf63',
                'cbffce8198e0e10c17f79f6ae0454af5', # lh.white
                '1159a9ee160b1b0c76e0bb9ae789b9be',
                'c53c1f70ae8971e1c04bd19e3277fa14',
                '71f11c33db672360d7589c7dbd0e4a3f',
                '95df985980d7eefa009ac104589ee3c5',
                'bb4d58289aefcdf8d017e45e531c4807',
                '3e81598a5ac0546443ec37d0ac477c80',
                '76ad91d2488de081392313ad5a87fafb',
                'a3735566ef949bd4d7ed303837cc5e77',  # color LUT
                'b39610adfe02fdce2ad9d30797c567b3',  # LICENSE
                '9bcc318b66c4da5e479b33e9451ec5e1',
                '9bcc318b66c4da5e479b33e9451ec5e1' # rh.sphere
    );



    ext_url_subject_part_fsaverage = 'subjects_dir/fsaverage/';
    ext_url_parts_each_subject = c('label/lh.aparc.a2009s.annot',
                                   'label/rh.aparc.a2009s.annot',
                                   'label/lh.aparc.annot',
                                   'label/rh.aparc.annot',
                                   'label/lh.cortex.label',
                                   'label/rh.cortex.label',
                                   'mri/brain.mgz',
                                   'surf/lh.white',
                                   'surf/rh.white',
                                   'surf/lh.pial',
                                   'surf/rh.pial',
                                   'surf/lh.inflated',
                                   'surf/rh.inflated',
                                   'surf/lh.curv',
                                   'surf/rh.curv',
                                   'ext/FreeSurferColorLUT.txt',
                                   'LICENSE',
                                   'surf/lh.sphere',
                                   'surf/rh.sphere'
    );
    ext_urls = paste(ext_url_subject_part_fsaverage, ext_url_parts_each_subject, sep='');
    base_url = 'http://rcmd.org/projects/nitestdata/';
    urls = paste(base_url, ext_urls, sep='');

    cfiles = pkgfilecache::ensure_files_available(pkg_info, local_filenames, urls, md5sums=md5sums);
    cfiles$file_status = NULL; # not exposed to end user
    return(invisible(cfiles));
}


#' @title Download the FreeSurfer v6 low-resolution fsaverage3 subject.
#'
#' @description Download some relevant files from the FreeSurfer v6 fsaverage3 subject. The files are subject to the FreeSurfer software license, see parameter 'accept_freesurfer_license' for details. This data is not required for the package to work. If you are working on a machine that has FreeSurfer installed, you already have this data anyways and do not need to download it. Also downloads data for subject1 that has been mapped to fsaverage.
#'
#' @inheritParams download_fsaverage
#'
#' @return Named list. The list has entries: "available": vector of strings. The names of the files that are available in the local file cache. You can access them using get_optional_data_file(). "missing": vector of strings. The names of the files that this function was unable to retrieve.
#'
#' @note The subject fsaverage3 is a downsampled (low mesh resolution) version of the standard fsaverage. If you never heard about fsaverage3, you do not need it. You will have to manually re-sample your data in FreeSurfer if you want to use it with fsaverage3.
#'
#' @export
download_fsaverage3 <- function(accept_freesurfer_license=FALSE) {

    if(! accept_freesurfer_license) {
        cat(sprintf("Nothing downloaded. You have to accept the FreeSurfer license to download and use fsaverage.\n"));
        cat(sprintf("Read the license at https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense and set parameter 'accept_freesurfer_license' to TRUE if you accept it.\n"));
        return(invisible(NULL));
    }

    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    base_path_fsaverage3 = c('subjects_dir', 'fsaverage3');
    base_path_subject1 = c('subjects_dir', 'subject1');
    local_filenames = list(c(base_path_fsaverage3, 'label', 'lh.cortex.label'),
                           c(base_path_fsaverage3, 'label', 'rh.cortex.label'),
                           c(base_path_fsaverage3, 'surf', 'lh.white'),
                           c(base_path_fsaverage3, 'surf', 'rh.white'),
                           c(base_path_fsaverage3, 'LICENSE'),
                           c(base_path_subject1, 'surf', 'lh.thickness.fwhm0.fsaverage3.mgz'),
                           c(base_path_subject1, 'surf', 'rh.thickness.fwhm0.fsaverage3.mgz')
    );



    md5sums = c('49a367e65ec7ecffbb721404b274fb3f', # fsaverage3 lh.cortex
                '76e2d42894351427405cc01ab351719b',
                'b014033974bc5b4deb8b54dc140abda8',
                '09a133fd8499f3192e051bdbd8bec6e8',
                'b39610adfe02fdce2ad9d30797c567b3',    # LICENSE fsaverage3
                'd191f6833d1d36016b30504fed1ce138',  # subject1 lh.thickness.fwhm0.fsaverage3.mgz
                'e874f8dc149fd11842f117c300d1a964'   # subject1 rh.thickness.fwhm0.fsaverage3.mgz
    );



    ext_url_subject_part_fsaverage3 = 'subjects_dir/fsaverage3/';
    ext_url_parts_fsaverage3 = c('label/lh.cortex.label',
                                   'label/rh.cortex.label',
                                   'surf/lh.white',
                                   'surf/rh.white',
                                   'LICENSE'
    );
    ext_urls_fsaverage3 = paste(ext_url_subject_part_fsaverage3, ext_url_parts_fsaverage3, sep='');

    ext_url_subject_part_subject1 = 'subjects_dir/subject1/';
    ext_url_parts_subject1 = c('surf/lh.thickness.fwhm0.fsaverage3.mgz',
                               'surf/rh.thickness.fwhm0.fsaverage3.mgz'
    );
    ext_urls_subject1 = paste(ext_url_subject_part_subject1, ext_url_parts_subject1, sep='');
    ext_urls = c(ext_urls_fsaverage3, ext_urls_subject1);
    base_url = 'http://rcmd.org/projects/nitestdata/';
    urls = paste(base_url, ext_urls, sep='');

    cfiles = pkgfilecache::ensure_files_available(pkg_info, local_filenames, urls, md5sums=md5sums);
    cfiles$file_status = NULL; # not exposed to end user
    return(invisible(cfiles));
}


#' @title Download extra data to reproduce the figures from the fsbrain paper.
#'
#' @return Named list. The list has entries: "available": vector of strings. The names of the files that are available in the local file cache. You can access them using get_optional_data_file(). "missing": vector of strings. The names of the files that this function was unable to retrieve.
#'
#' @note Called for side effect of data download.
#'
#' @export
download_optional_paper_data <- function() {

    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    base_path_subject1 = c('subjects_dir', 'subject1');
    local_filenames = list(c(base_path_subject1, 'surf', 'lh.tmap.fsaverage'),
                           c(base_path_subject1, 'surf', 'rh.tmap.fsaverage')
    );



    md5sums = c('896a2f60ee654952d6358bb89eb0c686', # lh.tmap.fsaverage
                'fc251982f366aef0971d48d6edd4dc49'  # rh.tmap.fsaverage
    );



    ext_url_subject_part_subject1 = 'subjects_dir/subject1/';
    ext_url_parts_subject1 = c('surf/lh.tmap.fsaverage',
                               'surf/rh.tmap.fsaverage'
    );
    ext_urls_subject1 = paste(ext_url_subject_part_subject1, ext_url_parts_subject1, sep='');
    ext_urls = ext_urls_subject1;
    base_url = 'http://rcmd.org/projects/nitestdata/';
    urls = paste(base_url, ext_urls, sep='');

    cfiles = pkgfilecache::ensure_files_available(pkg_info, local_filenames, urls, md5sums=md5sums);
    cfiles$file_status = NULL; # not exposed to end user
    return(invisible(cfiles));
}



#' @title Get file names available in package cache.
#'
#' @description Get file names of optional data files which are available in the local package cache. You can access these files with get_optional_data_file().
#'
#' @return vector of strings. The file names available, relative to the package cache.
#'
#' @export
list_optional_data <- function() {
    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    return(pkgfilecache::list_available(pkg_info));
}


#' @title Access a single file from the package cache by its file name.
#'
#' @param filename, string. The filename of the file in the package cache.
#'
#' @param mustWork, logical. Whether an error should be created if the file does not exist. If mustWork=FALSE and the file does not exist, the empty string is returned.
#'
#' @return string. The full path to the file in the package cache or the empty string if there is no such file available. Use this in your application code to open the file.
#'
#' @export
get_optional_data_filepath <- function(filename, mustWork=TRUE) {
    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    return(pkgfilecache::get_filepath(pkg_info, filename, mustWork=mustWork));
}


#' @title Delete all data in the package cache.
#'
#' @return integer. The return value of the unlink() call: 0 for success, 1 for failure. See the unlink() documentation for details.
#'
#' @export
delete_all_optional_data <- function() {
    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    return(pkgfilecache::erase_file_cache(pkg_info));
}


#' @title Download optional demo data if needed and return its path.
#'
#' @description This is a wrapper around \code{download_optional_data()} and \code{get_optional_data_filepath("subjects_dir")}. It will download the optional fsbrain demo data unless it already exists locally.
#'
#' @param accept_freesurfer_license logical, whether you want to also download fsaverage and fsaverage3, and accept the FreeSurfer license for fsaverage and fsaverage3, available at https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense. Defaults to FALSE. If FALSE, only the demo data from fsbrain itself ('subject1') will be downloaded.
#'
#' @return character string, the path to the 'subjects_dir' directory within the downloaded optional data directory.
#'
#' @note This function will stop if the data cannot be accessed, i.e., the 'subjects_dir' does not exist after trying to download the data.
#'
#' @export
sjd.demo <- function(accept_freesurfer_license=FALSE) {
    download_optional_data();
    download_optional_paper_data();
    if(accept_freesurfer_license) {
        download_fsaverage(accept_freesurfer_license);
        download_fsaverage3(accept_freesurfer_license);
    }
    return(get_optional_data_filepath("subjects_dir"));
}




