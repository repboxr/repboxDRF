# Data loading and cache for R metareg analysis

# Main aspect is caching

# We can have two types of caches:
# cache in memory mcache
# cache files fcache


# For debugging purposes helpful but may destroy some
# Stata pipeline so don't add by default
drf_set_add_org_row = function(add_org_row=TRUE) {
  options(repbox_data_add_org_row=add_org_row)
  return(add_org_row)
}

drf_load_data = function(project_dir, rel_path, ..., add_org_row=isTRUE(getOption("repbox_data_add_org_row"))) {
  restore.point("repbox_load_data")
  data = drf_cached_data(file=rel_path, project_dir=project_dir)
  if (!is.null(data)) return(data)
  file = file.path(project_dir, "drf/org_data", rel_path)
  if (!file.exists(file)) {
    stop(paste0("The original data set ", rel_path, " is not yet stored in the DRF folder ", file.path(project_dir, "drf/org_data")))
  }

  ext = tolower(tools::file_ext(file))
  if (ext=="dta") {
    data = haven::read_dta(file)
  } else if (ext=="rds") {
    data = readRDS(file)
  } else {
    data = rio::import(file)
  }

  if (isTRUE(add_org_row)) {
    data = bind_cols(tibble(.ORG.ROW = seq_len(NROW(data))), data)
  }

  drf_store_if_mcache_cand(data, file=rel_path, project_dir=project_dir)

  data
}
