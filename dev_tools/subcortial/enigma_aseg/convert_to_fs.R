#!/usr/bin/env Rscript
#
# Convert the ENIGMA subcortical aparc meshes to FreeSurfer format files.
#
# Input files:
# sctx_lh.vtk: Meshes of left hemisphere subcortical aparc structures from ENIGMA, in ASCII VTK format. All substructures in one mesh.
# sctx_rh.vtk: Meshes of right hemisphere subcortical aparc structures from ENIGMA, in ASCII VTK format. All substructures in one mesh.
# sctx_lh.csv: CSV file with mapping of vertices to substructures for left hemisphere
# sctx_rh.csv: CSV file with mapping of vertices to substructures for right hemisphere
# The colors of the substructures for the annotation files are read from the file
# `FreeSurferColorLUT.txt` in your FreeSurfer installation (see the environment variable FREESURFER_HOME).
#
# Output files:
# lh.subcortical: FreeSurfer surface file (binary mesh format) for left hemisphere subcortical aparc structures
# rh.subcortical: FreeSurfer surface file (binary mesh format) for right hemisphere subcortical aparc structures
# lh.subcortical.annot: FreeSurfer annotation file for left hemisphere subcortical aparc structures
# rh.subcortical.annot: FreeSurfer annotation file for right hemisphere subcortical aparc structures
#
#
#

library(freesurferformats)

in_dir <- "."
out_dir <- "."

# The colors to use for the substructures in the annotation files are the standard FreeSurfer 'aseg'
# colors. We read them from the file 'FreeSurferColorLUT.txt' of the FreeSurfer installation instead
# of hardcoding them, because the FreeSurfer developers occasionally change or extend them.
freesurfer_home <- Sys.getenv("FREESURFER_HOME")
if (nchar(freesurfer_home) == 0L) {
  stop("The environment variable 'FREESURFER_HOME' is not set, so the FreeSurfer color LUT file cannot be located. Please install FreeSurfer and make sure that FREESURFER_HOME is set to your FreeSurfer installation directory (see the 'SetUpFreeSurfer.sh' script that ships with FreeSurfer).\n")
}
fs_lut_file <- file.path(freesurfer_home, "FreeSurferColorLUT.txt")
if (!file.exists(fs_lut_file)) {
  stop(sprintf("The FreeSurfer color LUT file '%s' does not exist, please check your FreeSurfer installation.\n", fs_lut_file))
}
fs_colortable <- freesurferformats::read.fs.colortable(fs_lut_file)

# Look up the color of a single brain structure by its name in a colortable as read from a FreeSurfer
# color LUT file. Returns a data.frame with a single row and the columns 'name', 'r', 'g' and 'b'.
#
# Note: the structure names in the CSV files do not always match the names used in the color LUT file
# exactly. For example, the CSVs use 'Left-Thalamus-Proper', while recent versions of
# 'FreeSurferColorLUT.txt' call this structure 'Left-Thalamus'. We therefore also check common name
# variants of the requested region.
lookup.fs.color <- function(region_name, colortable, lut_file = fs_lut_file) {
  candidate_names <- unique(c(region_name, sub("-Proper$", "", region_name), paste0(region_name, "-Proper")))
  for (candidate_name in candidate_names) {
    region_idx <- match(candidate_name, colortable$struct_name)
    if (!is.na(region_idx)) {
      return(data.frame("name" = region_name, "r" = colortable$r[region_idx], "g" = colortable$g[region_idx], "b" = colortable$b[region_idx], stringsAsFactors = FALSE))
    }
  }
  stop(sprintf("No color is defined for the region '%s' in the FreeSurfer color LUT file '%s', please add the region to that file or pass a custom colortable to 'create_annot_file'.\n", region_name, lut_file))
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

source_mesh_lh <- freesurferformats::read.fs.surface.vtk(file.path(in_dir, "sctx_lh.vtk")) # 1-based indices
source_mesh_rh <- freesurferformats::read.fs.surface.vtk(file.path(in_dir, "sctx_rh.vtk"))

# read the CSV files with the mapping of vertices to substructures
vertex_mapping_lh <- read.csv(file.path(in_dir, "sctx_lh.csv"), stringsAsFactors = FALSE)
vertex_mapping_rh <- read.csv(file.path(in_dir, "sctx_rh.csv"), stringsAsFactors = FALSE)

# some sanity checks: ensure that the highest vertex index in the mapping matches the number of vertices of the mesh
if (nrow(source_mesh_lh$vertices) != max(vertex_mapping_lh$end_vertex)) {
  stop("Vertex count mismatch for left hemisphere: mesh has ", nrow(source_mesh_lh$vertices), " vertices, but the mapping assigns vertices up to index ", max(vertex_mapping_lh$end_vertex), ".")
}
if (nrow(source_mesh_rh$vertices) != max(vertex_mapping_rh$end_vertex)) {
  stop("Vertex count mismatch for right hemisphere: mesh has ", nrow(source_mesh_rh$vertices), " vertices, but the mapping assigns vertices up to index ", max(vertex_mapping_rh$end_vertex), ".")
}

# create the FreeSurfer surface files
freesurferformats::write.fs.surface(file.path(out_dir, "lh.subcortical"), source_mesh_lh$vertices, source_mesh_lh$faces)
freesurferformats::write.fs.surface(file.path(out_dir, "rh.subcortical"), source_mesh_rh$vertices, source_mesh_rh$faces)

# Look up the standard FreeSurfer color for each substructure that occurs in the mapping files. Note
# that some structures have identical colors in the FreeSurfer LUT (e.g., the left and right thalamus
# are both green). This is not a problem here, because we write one annotation file per hemisphere and
# no color is used twice within a single hemisphere.
aseg_colortable <- do.call(rbind, lapply(unique(c(vertex_mapping_lh$name, vertex_mapping_rh$name)), lookup.fs.color, colortable = fs_colortable))

# Create a FreeSurfer annotation file from a mapping of vertex ranges to substructures.
#
# The 'vertex_mapping' is a data.frame with the columns 'name' (character, the substructure),
# 'start_vertex' and 'end_vertex' (integer, 1-based and inclusive). The color to use for each
# substructure is looked up by name in the 'colortable' data.frame, which must have the columns
# 'name', 'r', 'g' and 'b'. The order of the rows in the mapping defines the order of the regions
# in the colortable of the generated annotation file.
#
# Vertices which are not covered by any of the ranges of the mapping are labeled 'Unknown'.
create_annot_file <- function(vertex_mapping, annot_file_path, colortable = aseg_colortable) {
  # check that the vertex mapping has all required columns
  required_columns <- c("name", "start_vertex", "end_vertex")
  missing_columns <- setdiff(required_columns, colnames(vertex_mapping))
  if (length(missing_columns) > 0L) {
    stop(sprintf("Vertex mapping must have the columns '%s', but the column(s) '%s' are missing.\n", paste(required_columns, collapse = "', '"), paste(missing_columns, collapse = "', '")))
  }
  if (nrow(vertex_mapping) == 0L) {
    stop("Vertex mapping must not be empty.\n")
  }
  if (!(is.numeric(vertex_mapping$start_vertex) && is.numeric(vertex_mapping$end_vertex))) {
    stop("The columns 'start_vertex' and 'end_vertex' of the vertex mapping must be vertex indices (numeric).\n")
  }
  if (anyNA(vertex_mapping$start_vertex) || anyNA(vertex_mapping$end_vertex) || anyNA(vertex_mapping$name)) {
    stop("The vertex mapping must not contain missing values.\n")
  }
  vertex_mapping$name <- as.character(vertex_mapping$name)

  # make sure the regions are sorted by vertex index
  vertex_mapping <- vertex_mapping[order(vertex_mapping$start_vertex), , drop = FALSE]

  # sanity checks for the vertex ranges
  if (min(vertex_mapping$start_vertex) < 1L) {
    stop("Vertex indices in the mapping are 1-based, but the mapping contains a 'start_vertex' smaller than 1.\n")
  }
  if (any(vertex_mapping$end_vertex < vertex_mapping$start_vertex)) {
    stop("The mapping contains a region with 'end_vertex' smaller than 'start_vertex'.\n")
  }
  if (nrow(vertex_mapping) > 1L) {
    # the ranges must be disjoint, otherwise the assignment of vertices to substructures would be ambiguous
    overlap_indices <- which(vertex_mapping$start_vertex[-1L] <= vertex_mapping$end_vertex[-nrow(vertex_mapping)])
    if (length(overlap_indices) > 0L) {
      stop(sprintf("The vertex ranges of the regions '%s' and '%s' overlap, each vertex must be assigned to at most one region.\n", vertex_mapping$name[overlap_indices], vertex_mapping$name[overlap_indices + 1L]))
    }
  }

  # ensure that we know the color of all substructures that occur in the mapping
  regions_without_color <- setdiff(vertex_mapping$name, colortable$name)
  if (length(regions_without_color) > 0L) {
    stop(sprintf("No color is defined for the region(s) '%s', please add them to the colortable.\n", paste(regions_without_color, collapse = "', '")))
  }

  # Assign the vertices to the regions. The value 0 means 'Unknown', values 1 to n refer to the
  # n regions of the mapping (in the order given by the mapping).
  num_vertices <- max(vertex_mapping$end_vertex)
  vertex_region_indices <- rep(0L, num_vertices)
  for (region_idx in seq_len(nrow(vertex_mapping))) {
    vertex_region_indices[seq.int(vertex_mapping$start_vertex[region_idx], vertex_mapping$end_vertex[region_idx])] <- region_idx
  }
  num_unknown_vertices <- sum(vertex_region_indices == 0L)
  if (num_unknown_vertices > 0L) {
    warning(sprintf("The vertex ranges of the mapping do not cover all %d vertices of the mesh, %d vertices are not assigned to any region and will be labeled 'Unknown'.\n", num_vertices, num_unknown_vertices))
  }

  # Build the colortable for the annotation file. The first entry is 'Unknown' (which is unused if
  # the mapping covers all vertices), followed by one entry per region. The columns 'struct_name',
  # 'r', 'g', 'b' and 'a' are required by 'write.fs.annot'.
  region_colors <- colortable[match(vertex_mapping$name, colortable$name), , drop = FALSE]
  annot_colortable <- data.frame(
    struct_name = c("Unknown", region_colors$name),
    r = c(0L, region_colors$r),
    g = c(0L, region_colors$g),
    b = c(0L, region_colors$b),
    a = 0L,
    struct_index = seq.int(0L, nrow(vertex_mapping)),
    stringsAsFactors = FALSE
  )

  # 'write.fs.annot' expects the labels of the vertices as 1-based indices into the colortable, so
  # the 0-based region indices used above have to be shifted by one (the first colortable entry is 'Unknown').
  freesurferformats::write.fs.annot(annot_file_path, num_vertices = as.integer(num_vertices), colortable = annot_colortable, labels_as_indices_into_colortable = vertex_region_indices + 1L)

  # verify the file we just wrote by reading it back in
  annot <- freesurferformats::read.fs.annot(annot_file_path)
  if (length(annot$label_codes) != num_vertices) {
    stop(sprintf("Annotation file '%s' is invalid: it contains %d vertices, but %d were expected.\n", annot_file_path, length(annot$label_codes), num_vertices))
  }
  expected_region_names <- c("Unknown", vertex_mapping$name)
  mismatching_vertices <- which(annot$label_names != expected_region_names[vertex_region_indices + 1L])
  if (length(mismatching_vertices) > 0L) {
    v <- mismatching_vertices[1L]
    stop(sprintf("Annotation file '%s' is invalid: the label of vertex %d should be '%s' but is '%s'.\n", annot_file_path, v - 1L, expected_region_names[vertex_region_indices[v] + 1L], annot$label_names[v]))
  }

  message(sprintf("Wrote annotation file '%s' with %d vertices in %d regions.\n", annot_file_path, num_vertices, nrow(vertex_mapping)))
  return(invisible(annot))
}


# create the annotation files for left and right hemispheres
create_annot_file(vertex_mapping_lh, file.path(out_dir, "lh.subcortical.annot"))
create_annot_file(vertex_mapping_rh, file.path(out_dir, "rh.subcortical.annot"))

# done