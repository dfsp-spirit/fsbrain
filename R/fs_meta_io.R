
nit.read.subjectsfile = function(subjects_file) {
    subjects_df = utils::read.table(subjects_file, header=FALSE, col.names = c("subject_id"));
    subjects_list = as.vector(subjects_df$subject_id);
    return(subjects_list);
}
