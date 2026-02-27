drf_run_df_add_data_paths = function(run_df = drf$run_df, drf_dir = drf$drf_dir, drf = NULL) {
  run_df$org_data_path = ifelse(!nzchar(run_df$found_path), NA_character_,file.path(drf_dir, "org_data", run_df$found_path))
  run_df$has_org_data = file.exists(run_df$org_data_path)

  cache_data_path = file.path(drf_dir, "cache_dta", paste0(run_df$runid,".dta"))
  has_cache_data = file.exists(cache_data_path)
  run_df$cache_data_path = ifelse(has_cache_data, cache_data_path, NA_character_)
  run_df$has_cache_data = has_cache_data
  run_df

}


drf_data_load_needs_clear = function(run_df=NULL, cmd=run_cmd$cmd, cmd_type=run_df$cmd_type) {
  cmd_type %in% c("load")
}

# This function just replaces the data path
drf_replace_run_df_code_data_path = function(run_df =drf$run_df, drf_dir = drf$drf_dir, drf = NULL, cmd_types = c("load","merge"), rows=NULL, prefer_cache = FALSE) {
  restore.point("drf_replace_run_df_org_data_path")

  if (!has_col(run_df, "org_data_path")) {
    run_df = drf_run_df_add_data_paths(run_df, drf_dir=drf_dir)
  }

  if (!has_col(run_df,"code")) {
    stop("You need the column run_df$code.")
  }

  if (is.null(rows) & !is.null(cmd_types)) {
    rows = which(run_df$cmd_type %in% cmd_types)
  } else if (is.null(rows)) {
    rows = seq_len(NROW(run_df))
  }

  if (length(rows)==0) return(run_df)


  # Replace original code that load original data set
  org_rows = rows[run_df$has_org_data[rows]]
  add_clear = drf_data_load_needs_clear(run_df[org_rows,])
  run_df$code[org_rows] = replace_stata_cmdline_path(run_df$code[org_rows], paste0('"',run_df$org_data_path[rows],'"'),add_clear=add_clear)

  # TO DO: cache replace:
  # If we load a cache, we load the cache file with a complete new load command
  # e.g. use "cachefile.dta", clear

  # ...
  run_df
}



# TO DO: Need better time estimates and also size estimates
drf_suggest_caches = function(ap_df=drf$ap_df, drf=NULL) {
  if (is.null(ap_df)) cat("\nYou need an ap_df")

  ap_df = ap_df %>%
    group_by(pid) %>%
    mutate(
      dur_sec = na.val(dur_sec, 0),
      path_sec = cumsum(dur_sec)
    ) %>%
    ungroup()

  sec_df = ap_df %>%
    group_by(runid) %>%
    summarize(
      num_paths = n(),
      sum_sec = sum(path_sec)
    ) %>%
    arrange(desc(sum_sec))

}



drf_copy_org_data = function(project_dir=drf$project_dir, ap_df=drf$ap_df, move_from_mod=TRUE, drf=NULL, overwrite=FALSE) {
  restore.point("drf_copy_org_data")
  stop()
  require_project_dir(project_dir)
  if (!dir.exists(project_dir)) stop()
  data_files = ap_df %>%
    filter(cmd_type %in% c("load","merge")) %>%
    pull(found_path) %>%
    unique()

  dest_files = file.path(project_dir,"drf/org_data", data_files)
  if (!overwrite) {
    has = file.exists(dest_files)
    dest_files = dest_files[!has]
    data_files = data_files[!has]
  }
  if (length(dest_files)==0) return(invisible(NULL))

  dirs = unique(dirname(dest_files))
  for (dir in dirs) {
    if (!dir.exists(dir))
      dir.create(dir, recursive=TRUE, showWarnings = FALSE)
  }

  # 1. Try to copy from mod folder
  mod_files = file.path(project_dir, "mod", data_files)
  has = which(file.exists(mod_files))
  if (move_from_mod) {
    file.rename(from=mod_files[has],to=dest_files[has])
  } else {
    file.copy(from=mod_files[has],to=dest_files[has], overwrite=overwrite)
  }
  if (length(has)==length(dest_files)) return()


  # 2. if not all files were in mod then copy from org
  has = file.exists(dest_files)
  dest_files = dest_files[!has]
  data_files = data_files[!has]

  org_files = file.path(project_dir, "org", data_files)
  has = which(file.exists(org_files))
  file.copy(from=mod_files[has],to=dest_files[has], overwrite=overwrite)

  has = file.exists(dest_files)
  if (!all(has)) {
    cat("\nThe data set(s) ", paste0(data_files[!has], collapse=", "), " were not found in mod or org folder and could not be copied to drf/org_data.")
  }
  return()
}
