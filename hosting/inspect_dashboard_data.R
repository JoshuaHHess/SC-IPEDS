script_file <- if (!is.null(sys.frame(1)$ofile)) {
  normalizePath(sys.frame(1)$ofile)
} else {
  normalizePath("hosting/inspect_dashboard_data.R")
}

project_root <- normalizePath(file.path(dirname(script_file), ".."))

e <- new.env()
load(file.path(project_root, "data", "processed", "dashboard.RData"), envir = e)
cat('SC institutions:', sum(e$df.control$STABBR == 'SC', na.rm = TRUE), '\n')
control_years <- if ('year' %in% names(e$df.control)) e$df.control$year else e$df.control$ipedsyear - 1L
cat('df.control years:', paste(range(control_years, na.rm = TRUE), collapse = '-'), '\n')
cat('df.stem years:', paste(range(e$df.stem$year, na.rm = TRUE), collapse = '-'), '\n')
cat('efc years:', paste(range(e$efc$ipedsyear, na.rm = TRUE), collapse = '-'), '\n')
cat('efc states sample:', paste(head(sort(unique(e$efc$EFCSTATE)), 20), collapse = ', '), '\n')
cat('efc lines sample:', paste(head(sort(unique(e$efc$LINE)), 20), collapse = ', '), '\n')
cat('stemdummy sample:', paste(sort(unique(e$df.stem$stemdummy)), collapse = ', '), '\n')
cat('sector sample:', paste(sort(unique(e$df.control$SECTOR)), collapse = ', '), '\n')
sc <- e$df.control[e$df.control$STABBR == 'SC', c(
  'UNITID',
  'ipedsyear',
  'INSTNM',
  'STABBR',
  'SECTOR',
  'under.in.charge',
  'under.out.charge'
)]
print(utils::head(sc, 10))
cat('Unique SC UNITIDs:', length(unique(sc$UNITID)), '\n')
