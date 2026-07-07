# Unit tests for the alphablend function.
#
# alphablend implements the "over" alpha compositing operation:
#   out_alpha = α_src + α_dst × (1 − α_src)
#   out_rgb   = (rgb_src × α_src + rgb_dst × α_dst × (1 − α_src)) / out_alpha
#
# Ref: https://en.wikipedia.org/wiki/Alpha_compositing#Alpha_blending
#
# Historical note: alphablend previously used rgb(cbind(..., alpha), alpha=TRUE)
# which silently discarded per-color alpha (R's rgb() ignores the 4th matrix
# column when alpha=TRUE). Fixed 2026-07-07: now uses per-row apply().
# ═══════════════════════════════════════════════════════════════════════════════

context("alphablend — unit tests")


# ── Helper ────────────────────────────────────────────────────────────────────

parse_rgba <- function(hex) {
    as.integer(grDevices::col2rgb(hex, alpha = TRUE)[, 1])
}

has_alpha <- function(hex) {
    parse_rgba(hex)[4]
}


# ═══════════════════════════════════════════════════════════════════════════════
# 1. Opaque foreground (α = 1) — background hidden
# ═══════════════════════════════════════════════════════════════════════════════

test_that("opaque foreground completely hides background", {
    expect_equal(alphablend("#FF0000FF", "#0000FFFF"), "#FF0000FF")
    expect_equal(alphablend("#00FF00FF", "#000000FF"), "#00FF00FF")
    expect_equal(alphablend("#FFFFFFFF", "#000000FF"), "#FFFFFFFF")
    expect_equal(alphablend("#FFFFFFFF", "#FF0000FF"), "#FFFFFFFF")
})


# ═══════════════════════════════════════════════════════════════════════════════
# 2. Fully transparent foreground (α = 0) — background fully visible
# ═══════════════════════════════════════════════════════════════════════════════

test_that("fully transparent foreground shows background unchanged", {
    expect_equal(alphablend("#00000000", "#FF0000FF"), "#FF0000FF")
    expect_equal(alphablend("#FFFFFF00", "#0000FFFF"), "#0000FFFF")
    expect_equal(alphablend("#12345600", "#00FF00FF"), "#00FF00FF")
})


# ═══════════════════════════════════════════════════════════════════════════════
# 3. Semi-transparent over OPAQUE background — out_alpha=1, unaffected by bug
# ═══════════════════════════════════════════════════════════════════════════════

test_that("semi-transparent red (alpha ~0.5) over opaque white gives pink", {
    # out_alpha = 128/255 + 1*(127/255) = 1.0, out_r=1, out_g=127/255, out_b=127/255
    expect_equal(alphablend("#FF000080", "#FFFFFFFF"), "#FF7F7FFF")
})

test_that("semi-transparent green (alpha ~0.5) over opaque black gives dark green", {
    # out_alpha = 1.0, out_r=0, out_g=128/255, out_b=0
    expect_equal(alphablend("#00FF0080", "#000000FF"), "#008000FF")
})

test_that("semi-transparent blue (alpha ~0.25) over opaque white gives light blue", {
    # out_alpha = 1.0, out_r=out_g=191/255, out_b=1
    expect_equal(alphablend("#0000FF40", "#FFFFFFFF"), "#BFBFFFFF")
})


# ═══════════════════════════════════════════════════════════════════════════════
# 4. Both inputs semi-transparent — alpha correctly preserved
# ═══════════════════════════════════════════════════════════════════════════════

test_that("both semi-transparent: RGB and alpha both correct", {
    # #FF000080 (red, α≈0.5) over #00FF0040 (green, α≈0.25)
    # out_a≈0.627→~160, out_r≈0.801→~204, out_g≈0.199→~51, out_b=0
    result <- alphablend("#FF000080", "#00FF0040")
    rgba <- parse_rgba(result)

    expect_equal(rgba[1], 204L, tolerance = 1)   # R ≈ 204
    expect_equal(rgba[2], 51L, tolerance = 1)    # G ≈ 51
    expect_equal(rgba[3], 0L)                    # B = 0
    expect_equal(rgba[4], 160L, tolerance = 1)   # A ≈ 160 (was: 255 before fix)
})


# ═══════════════════════════════════════════════════════════════════════════════
# 5. NA handling — NA treated as fully transparent black (#00000000)
# ═══════════════════════════════════════════════════════════════════════════════

test_that("NA foreground over opaque background -> background color", {
    expect_equal(alphablend(NA_character_, "#FF0000FF"), "#FF0000FF")
    expect_equal(alphablend(NA_character_, "#0000FFFF"), "#0000FFFF")
})

test_that("opaque foreground over NA background -> foreground", {
    expect_equal(alphablend("#FF0000FF", NA_character_), "#FF0000FF")
})

test_that("both NA -> fully transparent black", {
    result <- alphablend(NA_character_, NA_character_)
    rgba <- parse_rgba(result)
    expect_equal(rgba[1:3], c(0L, 0L, 0L))
    expect_equal(rgba[4], 0L)  # both transparent → transparent
})


# ═══════════════════════════════════════════════════════════════════════════════
# 6. Vectorized blending
# ═══════════════════════════════════════════════════════════════════════════════

test_that("vectorized blending works for multiple color pairs", {
    front <- c("#FF0000FF", "#FFFFFF00", NA_character_)
    back  <- c("#0000FFFF", "#000000FF", "#00FF00FF")

    result <- alphablend(front, back)

    expect_equal(result[1], "#FF0000FF")  # opaque red over blue
    expect_equal(result[2], "#000000FF")  # transparent white over black
    expect_equal(result[3], "#00FF00FF")  # NA over green
})

test_that("all-opaque foreground warns in non-silent mode", {
    front <- c("#FF0000FF", "#00FF00FF", "#0000FFFF")
    back  <- rep("#FFFFFFFF", 3L)

    expect_message(
        alphablend(front, back, silent = FALSE),
        "Background will not be visible"
    )
    result <- alphablend(front, back)
    expect_equal(result, front)
})


# ═══════════════════════════════════════════════════════════════════════════════
# 7. Edge cases
# ═══════════════════════════════════════════════════════════════════════════════

test_that("both fully transparent -> fully transparent black", {
    # Division by zero edge case: out_alpha=0, NaN→0 for rgb
    r1 <- alphablend("#00000000", "#00000000")
    r2 <- alphablend("#FF000000", "#00FF0000")
    expect_equal(has_alpha(r1), 0L)
    expect_equal(has_alpha(r2), 0L)
})

test_that("opaque foreground over transparent background -> foreground", {
    expect_equal(alphablend("#FF0000FF", "#00000000"), "#FF0000FF")
    expect_equal(alphablend("#123456FF", "#00000000"), "#123456FF")
})

test_that("length mismatch throws error", {
    expect_error(
        alphablend(c("#FF0000", "#00FF00"), "#000000"),
        "must be vectors with identical length"
    )
})

test_that("empty input works", {
    result <- alphablend(character(0), character(0))
    expect_equal(length(result), 0L)
})


# ═══════════════════════════════════════════════════════════════════════════════
# 8. Hemilist handling
# ═══════════════════════════════════════════════════════════════════════════════

test_that("hemilist front and back are blended per hemisphere", {
    fg <- list(lh = c("#FF000080", "#FFFFFF00"), rh = c("#0000FFFF"))
    bg <- list(lh = c("#FFFFFFFF", "#000000FF"), rh = c("#00FF00FF"))

    result <- alphablend(fg, bg)

    expect_true(is.list(result))
    expect_equal(names(result), c("lh", "rh"))
    expect_equal(result$lh[1], "#FF7F7FFF")
    expect_equal(result$lh[2], "#000000FF")
    expect_equal(result$rh[1], "#0000FFFF")
})

test_that("hemilist with NULL hemisphere works", {
    fg <- list(lh = c("#FF0000FF"))
    bg <- list(lh = c("#0000FFFF"))

    result <- alphablend(fg, bg)
    expect_equal(result$lh, "#FF0000FF")
    expect_null(result$rh)
})

test_that("mixed hemilist/non-hemilist throws error", {
    expect_error(
        alphablend(list(lh = "#FF0000"), "#000000"),
        "must have the same type"
    )
})


# ═══════════════════════════════════════════════════════════════════════════════
# 9. Associativity — "over" compositing is associative
# ═══════════════════════════════════════════════════════════════════════════════

test_that("blending is fully associative: (A over B) over C == A over (B over C)", {
    A <- "#FF000080"; B <- "#00FF0040"; C <- "#0000FFFF"

    AB_C <- alphablend(alphablend(A, B), C)
    A_BC <- alphablend(A, alphablend(B, C))

    rgba_ab_c <- parse_rgba(AB_C)
    rgba_a_bc <- parse_rgba(A_BC)

    # Both RGB and alpha must match
    expect_equal(rgba_ab_c, rgba_a_bc, tolerance = 1)
})


# ═══════════════════════════════════════════════════════════════════════════════
# 10. Invariant: output alpha should be in valid range [0, 255]
# ═══════════════════════════════════════════════════════════════════════════════

test_that("output alpha is always in valid range [0, 255]", {
    set.seed(42)
    for (i in 1:100) {
        col1 <- grDevices::rgb(runif(1), runif(1), runif(1), runif(1))
        col2 <- grDevices::rgb(runif(1), runif(1), runif(1), runif(1))
        result <- alphablend(col1, col2)
        alpha <- has_alpha(result)
        expect_true(alpha >= 0L && alpha <= 255L,
                    label = sprintf("alpha=%d for blend of %s over %s", alpha, col1, col2))
    }
})


# ═══════════════════════════════════════════════════════════════════════════════
# 11. Issue #28 scenario: transparent fg over opaque annotation bg
#     (Unaffected by the bug: out_alpha=1 with one opaque input)
# ═══════════════════════════════════════════════════════════════════════════════

test_that("'#FFFFFF00' (squash::cmap col.na) over opaque color -> bg preserved", {
    annot_colors <- c("#FF0000FF", "#00FF00FF", "#0000FFFF",
                      "#FFFF00FF", "#FF00FFFF", "#00FFFFFF")

    for (annot_col in annot_colors) {
        result <- alphablend("#FFFFFF00", annot_col)
        expect_equal(result, annot_col)
        expect_equal(has_alpha(result), 255L)
    }
})

test_that("R NA over opaque annotation color -> annotation preserved", {
    annot_colors <- c("#FF0000FF", "#00FF00FF", "#0000FFFF")

    for (annot_col in annot_colors) {
        result <- alphablend(NA_character_, annot_col)
        expect_equal(result, annot_col)
        expect_equal(has_alpha(result), 255L)
    }
})
