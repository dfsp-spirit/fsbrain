# Diagnostic tests for GitHub issue #28: annot.outline as background appears monochrome black.
#
# See: https://github.com/dfsp-spirit/fsbrain/issues/28
# See also: https://github.com/dfsp-spirit/fsbrain/issues/14 (related: multi-layer transparency)
#
# These tests trace through the full pipeline that turns annotation outline colors
# into a background layer and merges them with cluster data colors.

context("Issue #28: annot.outline as background — diagnostic tests")


# ── Helpers ───────────────────────────────────────────────────────────────────

#' Check whether a color string contains an alpha channel (8-digit hex or named)
has_alpha <- function(color_str) {
    rgba <- grDevices::col2rgb(color_str, alpha = TRUE)
    return(rgba[4, ])
}

#' Get unique non-background colors from an annot.outline result
non_bg_colors <- function(outline_vec, bg = "white") {
    unique(outline_vec[outline_vec != bg])
}


# ── Step 1: Inspect raw annotation colortable colors ──────────────────────────

test_that("STEP 1: Annotation colortable produces valid RGBA hex strings", {
    skip_if(tests_running_on_cran_under_macos(),
            message = "Skipping on CRAN under MacOS.")
    fsbrain::download_optional_data()
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.")

    lh_annot <- subject.annot(subjects_dir, "fsaverage", "lh", "aparc")

    # The colortable_df must exist and contain hex_color_string_rgba
    expect_true("colortable_df" %in% names(lh_annot))
    ct_df <- lh_annot$colortable_df

    # Diagnostic: print colortable_df columns
    message(sprintf("colortable_df has %d rows, columns: %s",
                    nrow(ct_df), paste(colnames(ct_df), collapse = ", ")))

    expect_true("hex_color_string_rgba" %in% colnames(ct_df),
                label = "colortable_df MUST contain hex_color_string_rgba column")

    # Every region (except 'unknown', index 1) should have a non-transparent color
    for (idx in seq_len(nrow(ct_df))) {
        rgba_hex <- as.character(ct_df$hex_color_string_rgba[[idx]])
        region_name <- ct_df$struct_name[[idx]]

        expect_true(is.character(rgba_hex) && nchar(rgba_hex) > 0,
                    label = sprintf("Region '%s' has invalid hex_color_string_rgba", region_name))

        expect_match(rgba_hex, "^#[0-9A-Fa-f]{8}$",
                     label = sprintf("Region '%s' hex_color_string_rgba '%s' not 8-digit hex",
                                     region_name, rgba_hex))

        alpha_val <- has_alpha(rgba_hex)
        message(sprintf("  Region %2d '%s': %s  (alpha=%d)",
                        idx, region_name, rgba_hex, alpha_val))

        if (idx == 1L) {
            # The 'unknown' region typically has alpha=0
            expect_equal(alpha_val, 0L,
                         label = sprintf("Region 'unknown' should have alpha=0, got %d", alpha_val))
        } else {
            # All other regions should be fully opaque
            expect_equal(alpha_val, 255L,
                         label = sprintf("Region '%s' should be opaque (alpha=255), got %d",
                                         region_name, alpha_val))
        }
    }
})


# ── Step 2: Inspect annot.outline output ──────────────────────────────────────

test_that("STEP 2: annot.outline produces colored outlines, not all-black", {
    skip_if(tests_running_on_cran_under_macos(),
            message = "Skipping on CRAN under MacOS.")
    fsbrain::download_optional_data()
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.")

    lh_annot <- subject.annot(subjects_dir, "fsaverage", "lh", "aparc")
    lh_inf   <- subject.surface(subjects_dir, "fsaverage", "inflated", "lh")

    outline <- annot.outline(annotdata = lh_annot,
                             surface_mesh = lh_inf,
                             outline_color = NULL)

    expect_equal(length(outline), length(lh_annot$vertices))

    # Get non-white (non-background) colors to inspect
    bg_mask  <- (outline == "white")
    outline_colors <- outline[!bg_mask]

    expect_true(length(outline_colors) > 0,
                label = "annot.outline should produce some non-background outline vertices")

    message(sprintf("Number of outline (non-background) vertices: %d of %d (%.1f%%)",
                    length(outline_colors), length(outline),
                    100 * length(outline_colors) / length(outline)))

    unique_cols <- unique(outline_colors)
    message(sprintf("Number of unique outline colors: %d", length(unique_cols)))

    # Show first few unique colors
    for (i in seq_len(min(10, length(unique_cols)))) {
        alpha_val <- has_alpha(unique_cols[i])
        message(sprintf("  Color %d: %s  (alpha=%d)", i, unique_cols[i], alpha_val))
    }

    # CRITICAL CHECK: Outline colors should NOT all be black
    black_count <- sum(grepl("^#000000", outline_colors))
    message(sprintf("Black outline vertices: %d of %d (%.1f%%)",
                    black_count, length(outline_colors),
                    100 * black_count / max(1, length(outline_colors))))

    # Some outlines should be non-black colored
    non_black <- outline_colors[!grepl("^#000000", outline_colors)]
    expect_true(length(non_black) > 0,
                label = "annot.outline should produce some NON-BLACK colored outlines")
})


# ── Step 3: Check col2rgb parsing of RGBA hex strings ─────────────────────────

test_that("STEP 3: col2rgb correctly parses 8-digit RGBA hex strings", {
    # Test the exact format produced by grDevices::rgb(r, g, b, a)
    test_colors <- c(
        "#FF0000FF",  # red, opaque
        "#00FF00FF",  # green, opaque
        "#0000FFFF",  # blue, opaque
        "#FF000000",  # red, fully transparent
        "#00000000",  # black, fully transparent
        "#FFFFFFFF",  # white, opaque
        "#FFFFFF00"   # white, fully transparent
    )

    for (col in test_colors) {
        rgba <- grDevices::col2rgb(col, alpha = TRUE)
        expect_equal(nrow(rgba), 4L,
                     label = sprintf("col2rgb('%s') should return 4 rows", col))

        # Parse expected from hex
        r_expected <- strtoi(substr(col, 2, 3), base = 16L)
        g_expected <- strtoi(substr(col, 4, 5), base = 16L)
        b_expected <- strtoi(substr(col, 6, 7), base = 16L)
        a_expected <- strtoi(substr(col, 8, 9), base = 16L)

        expect_equal(rgba[1, 1], r_expected,
                     label = sprintf("Red channel mismatch for '%s'", col))
        expect_equal(rgba[2, 1], g_expected,
                     label = sprintf("Green channel mismatch for '%s'", col))
        expect_equal(rgba[3, 1], b_expected,
                     label = sprintf("Blue channel mismatch for '%s'", col))
        expect_equal(rgba[4, 1], a_expected,
                     label = sprintf("Alpha channel mismatch for '%s'", col))
    }
})


# ── Step 4: alphablend with annotation-like colors ────────────────────────────

test_that("STEP 4: alphablend preserves background color when foreground is transparent", {
    # Simulate: annotation outline color as background, cluster-data transparent as foreground

    # Typical annotation RGBA colors
    annot_colors <- c("#FF0000FF", "#00FF00FF", "#0000FFFF", "#FFFF00FF")
    # White background (non-outline vertices in annot.outline)
    bg_white <- "#FFFFFFFF"

    # Cluster data: opaque where cluster != 0, transparent where cluster == 0
    fg_opaque       <- "#884488FF"  # typical cluster color
    fg_transparent1  <- "#FFFFFF00"  # from squash::cmap with col.na='#FFFFFF00'
    fg_transparent2  <- NA_character_ # R NA (also treated as transparent in alphablend)

    # --- Case A: Transparent foreground (via '#FFFFFF00') over colored outline ---
    blended_A <- alphablend(fg_transparent1, annot_colors[1])
    message(sprintf("Transparent white over red outline: %s", blended_A))
    # Expected: the background red shows through fully
    expect_equal(blended_A, "#FF0000FF",
                 label = "Transparent '#FFFFFF00' over opaque red should give opaque red")

    # --- Case B: Transparent foreground (NA) over colored outline ---
    blended_B <- alphablend(fg_transparent2, annot_colors[1])
    message(sprintf("NA over red outline: %s", blended_B))
    expect_equal(blended_B, "#FF0000FF",
                 label = "NA over opaque red should give opaque red")

    # --- Case C: Opaque foreground over colored outline ---
    blended_C <- alphablend(fg_opaque, annot_colors[1])
    message(sprintf("Opaque cluster over red outline: %s", blended_C))
    # Expected: the opaque foreground completely obscures background
    expect_equal(blended_C, "#884488FF",
                 label = "Opaque fg over red outline should give opaque fg color")

    # --- Case D: Transparent white over white background ---
    blended_D <- alphablend(fg_transparent1, bg_white)
    message(sprintf("Transparent white over white bg: %s", blended_D))
    expect_equal(blended_D, "#FFFFFFFF",
                 label = "Transparent white over white bg should give white")
})


# ── Step 5: collayers.merge with annotation outlines ──────────────────────────

test_that("STEP 5: collayers.merge preserves annotation outline colors", {
    # Create synthetic layers mimicking the real scenario
    n_verts <- 100L

    # "Annotation outline" background: some vertices colored, rest white
    bg_layer <- rep("#FFFFFFFF", n_verts)
    bg_layer[20:30] <- "#FF0000FF"  # red outline region
    bg_layer[50:60] <- "#0000FFFF"  # blue outline region
    bg_layer[80:85] <- "#00FF00FF"  # green outline region

    # "Cluster data" foreground: opaque clusters with transparent gaps
    fg_layer <- rep("#FFFFFF00", n_verts)  # transparent by default (like col.na)
    fg_layer[10:40] <- "#FF4444FF"  # opaque cluster 1
    fg_layer[70:90] <- "#4444FFFF"  # opaque cluster 2

    merged <- collayers.merge(list(fg = fg_layer, bg = bg_layer),
                               opaque_background = FALSE)

    # Where fg is transparent and bg is an outline vertex, bg color should show
    # Vertices 20-25: bg=red, fg=transparent → should be red
    expect_equal(merged[22], "#FF0000FF",
                 label = "Transparent fg over red bg should give red")
    # Vertices 52-55: bg=blue, fg=transparent → should be blue
    expect_equal(merged[53], "#0000FFFF",
                 label = "Transparent fg over blue bg should give blue")
    # Vertices 81-83: bg=green, fg=transparent → should be green
    expect_equal(merged[82], "#00FF00FF",
                 label = "Transparent fg over green bg should give green")

    # Where fg is opaque, fg color should show
    # Vertices 15-18: bg=white, fg=opaque cluster → cluster color
    expect_equal(merged[15], "#FF4444FF",
                 label = "Opaque cluster fg should show over bg")
    # Vertices 75-78: bg=white, fg=opaque cluster 2
    expect_equal(merged[75], "#4444FFFF",
                 label = "Opaque cluster 2 fg should show over bg")

    message("All blended outline colors are correct in synthetic test.")
})


# ── Step 6: End-to-end pipeline with real data (no rendering) ─────────────────

test_that("STEP 6: Full pipeline — cluster data over annot.outline background", {
    skip_if(tests_running_on_cran_under_macos(),
            message = "Skipping on CRAN under MacOS.")
    fsbrain::download_optional_data()
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.")

    avpath <- fsbrain::fsaverage.path()

    # --- 6a: Create annotation outlines (like issue #28) ---
    lh_annot <- subject.annot(avpath, "fsaverage", "lh", "aparc")
    rh_annot <- subject.annot(avpath, "fsaverage", "rh", "aparc")
    lh_inf   <- subject.surface(avpath, "fsaverage", "inflated", "lh")
    rh_inf   <- subject.surface(avpath, "fsaverage", "inflated", "rh")

    aparc_outline <- list(
        lh = annot.outline(annotdata = lh_annot, surface_mesh = lh_inf,
                           outline_color = NULL),
        rh = annot.outline(annotdata = rh_annot, surface_mesh = rh_inf,
                           outline_color = NULL)
    )

    # Verify outlines are colored, not all-black or all-white
    lh_outline_colors <- aparc_outline$lh[aparc_outline$lh != "white"]
    rh_outline_colors <- aparc_outline$rh[aparc_outline$rh != "white"]
    message(sprintf("LH outline vertices: %d, RH outline vertices: %d",
                    length(lh_outline_colors), length(rh_outline_colors)))

    lh_black_outlines <- sum(grepl("^#000000", lh_outline_colors))
    rh_black_outlines <- sum(grepl("^#000000", rh_outline_colors))
    message(sprintf("LH black outlines: %d, RH black outlines: %d",
                    lh_black_outlines, rh_black_outlines))

    expect_true(length(lh_outline_colors) > 0,
                label = "LH annotation outline should have some colored vertices")
    expect_true(lh_black_outlines < length(lh_outline_colors),
                label = "LH outline should NOT be all black")

    # --- 6b: Load cluster data (like issue #28) ---
    lh_demo_cluster_file <- system.file("extdata", "lh.clusters_fsaverage.mgz",
                                        package = "fsbrain", mustWork = TRUE)
    rh_demo_cluster_file <- system.file("extdata", "rh.clusters_fsaverage.mgz",
                                        package = "fsbrain", mustWork = TRUE)
    lh_clust <- freesurferformats::read.fs.morph(lh_demo_cluster_file)
    rh_clust <- freesurferformats::read.fs.morph(rh_demo_cluster_file)

    # Map 0 to NA (matching issue #28 default: map_to_NA=c(0))
    lh_clust_mapped <- perform.na.mapping(lh_clust, map_to_NA = c(0))
    rh_clust_mapped <- perform.na.mapping(rh_clust, map_to_NA = c(0))

    # --- 6c: Create foreground color layer (same as vis.symmetric.data.on.subject) ---
    makecmap_options <- list(colFn = cm.cbry(), symm = TRUE,
                             col.na = '#FFFFFF00', n = 200)
    fg_colors <- collayer.from.morphlike.data(lh_clust_mapped, rh_clust_mapped,
                                               makecmap_options = makecmap_options,
                                               return_metadata = TRUE)
    metadata <- fg_colors$metadata
    fg_colors$metadata <- NULL

    # Verify foreground has transparent colors for NA values
    lh_na_mask <- is.na(lh_clust_mapped)
    if (any(lh_na_mask)) {
        na_colors_lh <- unique(fg_colors$lh[lh_na_mask])
        message(sprintf("LH: %d NA vertices → colors: %s",
                        sum(lh_na_mask),
                        paste(na_colors_lh, collapse = ", ")))
        # squash::cmap with col.na should return '#FFFFFF00' for NAs, not R NA
        expect_false(any(is.na(fg_colors$lh[lh_na_mask])),
                     label = "NA data values should map to '#FFFFFF00', not R NA in fg layer")
    }

    # --- 6d: Merge layers (same as vis.subject.morph.native does) ---
    # Important: opaque_background=FALSE is what vis.subject.morph.native uses
    merged_colors <- collayers.merge(
        collayers = list(fg = fg_colors, bg = aparc_outline),
        opaque_background = FALSE
    )

    # --- 6e: Verify merged result ---
    # Where cluster value is NA (was 0), annotation outline should show through
    # Pick a specific LH vertex that:
    #   - has cluster value NA (mapped from 0)
    #   - is an outline vertex in the annotation

    # Find LH vertices where cluster is NA AND annotation outline is non-white
    lh_clust_na  <- which(is.na(lh_clust_mapped))
    lh_is_outline <- which(aparc_outline$lh != "white")

    lh_na_outline_verts <- intersect(lh_clust_na, lh_is_outline)

    if (length(lh_na_outline_verts) > 0) {
        message(sprintf("Found %d LH vertices where cluster=NA AND outline is non-white",
                        length(lh_na_outline_verts)))

        # Sample a few and check
        for (vi in head(lh_na_outline_verts, 5)) {
            merged_color <- merged_colors$lh[vi]
            annot_color  <- aparc_outline$lh[vi]

            rgba_merged <- grDevices::col2rgb(merged_color, alpha = TRUE)
            rgba_annot  <- grDevices::col2rgb(annot_color, alpha = TRUE)

            message(sprintf("  Vertex %d: annot=%s merged=%s (rgba: %d,%d,%d,%d vs %d,%d,%d,%d)",
                            vi, annot_color, merged_color,
                            rgba_annot[1], rgba_annot[2], rgba_annot[3], rgba_annot[4],
                            rgba_merged[1], rgba_merged[2], rgba_merged[3], rgba_merged[4]))

            # The merged color should match the annotation outline color
            # for vertices where foreground is transparent
            expect_equal(merged_color, annot_color,
                         label = sprintf("Vertex %d: transparent fg over colored outline should preserve outline color", vi))
        }
    } else {
        message("WARNING: No LH vertices found where cluster=NA AND annot outline is non-white. This may mean the cluster covers all outline vertices.")
    }

    # Also check: where cluster is non-NA, the merge should show the cluster color
    lh_clust_non_na <- which(!is.na(lh_clust_mapped))
    if (length(lh_clust_non_na) > 0) {
        test_vi <- lh_clust_non_na[1]
        merged_color <- merged_colors$lh[test_vi]
        fg_color     <- fg_colors$lh[test_vi]
        annot_color  <- aparc_outline$lh[test_vi]

        message(sprintf("  Non-NA vertex %d: fg=%s annot=%s merged=%s",
                        test_vi, fg_color, annot_color, merged_color))
        # For opaque cluster colors, merged should equal fg
        if (has_alpha(fg_color) == 255L) {
            expect_equal(merged_color, fg_color,
                         label = "Opaque cluster fg should appear in merged result")
        }
    }
})


# ── Step 7: Check what rgl does with merged colors ────────────────────────────

test_that("STEP 7: Merged colors are fully opaque (check alpha values)", {
    skip_if(tests_running_on_cran_under_macos(),
            message = "Skipping on CRAN under MacOS.")
    fsbrain::download_optional_data()
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.")

    avpath <- fsbrain::fsaverage.path()

    lh_annot <- subject.annot(avpath, "fsaverage", "lh", "aparc")
    rh_annot <- subject.annot(avpath, "fsaverage", "rh", "aparc")
    lh_inf   <- subject.surface(avpath, "fsaverage", "inflated", "lh")
    rh_inf   <- subject.surface(avpath, "fsaverage", "inflated", "rh")

    aparc_outline <- list(
        lh = annot.outline(annotdata = lh_annot, surface_mesh = lh_inf,
                           outline_color = NULL),
        rh = annot.outline(annotdata = rh_annot, surface_mesh = rh_inf,
                           outline_color = NULL)
    )

    lh_demo_cluster_file <- system.file("extdata", "lh.clusters_fsaverage.mgz",
                                        package = "fsbrain", mustWork = TRUE)
    rh_demo_cluster_file <- system.file("extdata", "rh.clusters_fsaverage.mgz",
                                        package = "fsbrain", mustWork = TRUE)
    lh_clust <- freesurferformats::read.fs.morph(lh_demo_cluster_file)
    rh_clust <- freesurferformats::read.fs.morph(rh_demo_cluster_file)
    lh_clust_mapped <- perform.na.mapping(lh_clust, map_to_NA = c(0))
    rh_clust_mapped <- perform.na.mapping(rh_clust, map_to_NA = c(0))

    makecmap_options <- list(colFn = cm.cbry(), symm = TRUE,
                             col.na = '#FFFFFF00', n = 200)
    fg_colors <- collayer.from.morphlike.data(lh_clust_mapped, rh_clust_mapped,
                                               makecmap_options = makecmap_options,
                                               return_metadata = TRUE)
    fg_colors$metadata <- NULL

    # Merge WITHOUT opaque_background (as vis.subject.morph.native does)
    merged_no_bg <- collayers.merge(
        collayers = list(fg = fg_colors, bg = aparc_outline),
        opaque_background = FALSE
    )

    # Merge WITH opaque_background (as group morph functions do)
    merged_with_bg <- collayers.merge(
        collayers = list(fg = fg_colors, bg = aparc_outline),
        opaque_background = "#FFFFFF"
    )

    # Check alpha values in merged results
    for (hemi in c("lh", "rh")) {
        no_bg_alphas <- has_alpha(merged_no_bg[[hemi]])
        with_bg_alphas <- has_alpha(merged_with_bg[[hemi]])

        no_bg_non_opaque <- sum(no_bg_alphas < 255L)
        with_bg_non_opaque <- sum(with_bg_alphas < 255L)

        message(sprintf("%s without opaque_bg: %d / %d vertices have alpha < 255",
                        hemi, no_bg_non_opaque, length(no_bg_alphas)))
        message(sprintf("%s with opaque_bg:    %d / %d vertices have alpha < 255",
                        hemi, with_bg_non_opaque, length(with_bg_alphas)))

        # Without opaque_background, some colors may be non-opaque
        # With opaque_background, ALL colors should be fully opaque
        expect_equal(with_bg_non_opaque, 0L,
                     label = sprintf("%s: all colors should be opaque when opaque_background used", hemi))
    }
})


# ── Step 8: Compare the actual coloredmeshes from the two code paths ───────────

test_that("STEP 8: Compare coloredmeshes — direct annot.outline vs as background", {
    skip_if(tests_running_on_cran_under_macos(),
            message = "Skipping on CRAN under MacOS.")
    fsbrain::download_optional_data()
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.")

    avpath <- fsbrain::fsaverage.path()

    # Create annotation outlines
    lh_annot <- subject.annot(avpath, "fsaverage", "lh", "aparc")
    rh_annot <- subject.annot(avpath, "fsaverage", "rh", "aparc")
    lh_inf   <- subject.surface(avpath, "fsaverage", "inflated", "lh")
    rh_inf   <- subject.surface(avpath, "fsaverage", "inflated", "rh")

    aparc_outline <- list(
        lh = annot.outline(annotdata = lh_annot, surface_mesh = lh_inf,
                           outline_color = NULL),
        rh = annot.outline(annotdata = rh_annot, surface_mesh = rh_inf,
                           outline_color = NULL)
    )

    lh_demo_cluster_file <- system.file("extdata", "lh.clusters_fsaverage.mgz",
                                        package = "fsbrain", mustWork = TRUE)
    rh_demo_cluster_file <- system.file("extdata", "rh.clusters_fsaverage.mgz",
                                        package = "fsbrain", mustWork = TRUE)

    # --- Path 1: Direct visualization of annot.outline (cm1 in issue #28) ---
    cm1 <- vis.color.on.subject(avpath, "fsaverage",
                                color_lh = aparc_outline$lh,
                                color_rh = aparc_outline$rh,
                                surface = "inflated",
                                views = "t4",
                                rglactions = list(no_vis = TRUE))

    # --- Path 2: As background in vis.symmetric.data.on.subject (cm2 in issue #28) ---
    cm2 <- vis.symmetric.data.on.subject(avpath, "fsaverage",
                                         lh_demo_cluster_file,
                                         rh_demo_cluster_file,
                                         surface = "inflated",
                                         views = "t4",
                                         bg = aparc_outline,
                                         rglactions = list(no_vis = TRUE))

    # Compare the vertex colors between the two paths
    # In cm1 (direct), all vertices have the annot.outline colors
    # In cm2 (merged), vertices where cluster=0 should show annot.outline colors

    for (hemi in c("lh", "rh")) {
        cm1_cols <- cm1[[hemi]]$col
        cm2_cols <- cm2[[hemi]]$col

        message(sprintf("\n--- %s hemisphere ---", hemi))

        # Find non-white vertices in cm1 (these are the annotation outlines)
        outline_mask_cm1 <- (cm1_cols != "#FFFFFFFF" & cm1_cols != "white" &
                             cm1_cols != "#FFFFFF")

        # Find where cm2 differs from cm1
        diff_mask <- (cm1_cols != cm2_cols)

        n_diff <- sum(diff_mask)
        message(sprintf("Vertices where cm1 != cm2: %d of %d (%.1f%%)",
                        n_diff, length(cm1_cols),
                        100 * n_diff / length(cm1_cols)))

        # Where cm1 has outlines, check what cm2 has
        outline_and_diff <- which(outline_mask_cm1 & diff_mask)

        if (length(outline_and_diff) > 0) {
            message(sprintf("Outline vertices that differ: %d", length(outline_and_diff)))
            # Sample a few
            for (vi in head(outline_and_diff, 10)) {
                message(sprintf("  Vertex %d: cm1=%s  cm2=%s",
                                vi, cm1_cols[vi], cm2_cols[vi]))
                # Check if cm2 has black where cm1 has colored outline
                cm2_alpha <- has_alpha(cm2_cols[vi])
                message(sprintf("    cm2 alpha=%d", cm2_alpha))
            }

            # Count how many outline vertices in cm2 are black or near-black
            cm2_rgba <- grDevices::col2rgb(cm2_cols[outline_and_diff], alpha = TRUE)
            cm2_is_dark <- (cm2_rgba[1, ] < 30 & cm2_rgba[2, ] < 30 & cm2_rgba[3, ] < 30)
            message(sprintf("Dark/black outline vertices in cm2: %d of %d (%.1f%%)",
                            sum(cm2_is_dark), length(outline_and_diff),
                            100 * sum(cm2_is_dark) / length(outline_and_diff)))
        } else {
            message("No outline vertices differ between cm1 and cm2.")
        }

        # Where outlines are in cm1, what are the unique colors in cm2?
        outline_verts <- which(outline_mask_cm1)
        unique_cm2_outline <- unique(cm2_cols[outline_verts])
        message(sprintf("Unique cm2 colors at cm1-outline positions: %d",
                        length(unique_cm2_outline)))
        if (length(unique_cm2_outline) <= 20) {
            for (col in unique_cm2_outline) {
                message(sprintf("  %s", col))
            }
        }
    }
})


# ── Step 9: Check if freesurferformats hex_color_string_rgba is the culprit ────

test_that("STEP 9: Verify freesurferformats colortable_df hex_color_string_rgba format", {
    skip_if(tests_running_on_cran_under_macos(),
            message = "Skipping on CRAN under MacOS.")
    fsbrain::download_optional_data()
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.")

    avpath <- fsbrain::fsaverage.path()
    lh_annot <- subject.annot(avpath, "fsaverage", "lh", "aparc")
    ct_df <- lh_annot$colortable_df

    # Check if hex_color_string_rgba comes from freesurferformats or fsbrain
    # The freesurferformats package may or may not provide this column.
    # If it's missing, annot.outline would fail silently.

    if (!"hex_color_string_rgba" %in% colnames(ct_df)) {
        message("WARNING: hex_color_string_rgba NOT in colortable_df from read.fs.annot!")
        message("This means annot.outline with outline_color=NULL will fail.")
        message("Columns present: ", paste(colnames(ct_df), collapse = ", "))
    } else {
        message("hex_color_string_rgba IS present in colortable_df (good).")

        # Check if all entries are valid 8-digit hex
        for (idx in seq_len(nrow(ct_df))) {
            val <- as.character(ct_df$hex_color_string_rgba[[idx]])
            is_valid <- grepl("^#[0-9A-Fa-f]{8}$", val)
            if (!is_valid) {
                message(sprintf("INVALID hex_color_string_rgba at row %d: '%s'",
                                idx, val))
            }
            expect_true(is_valid,
                        label = sprintf("Row %d '%s' hex_color_string_rgba should be #RRGGBBAA", idx, ct_df$struct_name[[idx]]))
        }
    }

    # Also check the raw r,g,b,a values
    message("\nRaw colortable values:")
    for (idx in seq_len(min(6, nrow(ct_df)))) {
        message(sprintf("  %s: R=%d G=%d B=%d A=%d",
                        ct_df$struct_name[[idx]],
                        ct_df$r[[idx]], ct_df$g[[idx]],
                        ct_df$b[[idx]], ct_df$a[[idx]]))
    }
})
