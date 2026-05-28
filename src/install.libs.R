progs <- if (WINDOWS) "callrem.exe" else "callrem"

dest <- file.path(R_PACKAGE_DIR, paste0("bin", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
# If `configure` decided not to build the launcher (no libR), the
# binary is absent and this is a silent no-op; the R-side glue then
# falls back to the system R executable.
suppressWarnings(file.copy(progs, dest, overwrite = TRUE))
