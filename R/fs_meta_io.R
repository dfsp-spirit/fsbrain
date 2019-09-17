
nit.read.subjectsfile = function(filepath) {
    subjects_df = read.table(subjects_file, header=FALSE, col.names = c("subject_id"));
    subjects_list = as.vector(subjects_df$subject_id)
    return(subjects_list);
}
