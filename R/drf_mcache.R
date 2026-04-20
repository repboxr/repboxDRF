example = function() {
  drf_mcache_info()
}

drf_mcache_info = function() {
  cache = getOption("repboxDRF.mcache")
  if (is.null(cache)) {
    cat("\nNo drf_cache stored.\n")
    return(invisible())
  }
  cat("\nCache for ", cache$project_dir)

  cat("\ndata_for_runid:\n")
  print(names(cache$data_for_runid))

  cat("\ndata_for_file:\n")
  print(names(cache$data_for_file))

  cat("\nfile_mcache_cand:\n")
  cat(paste0(cache$file_mcache_cand$file, collapse=", "))
  cat("\n")

  cat("\nrunid_mcache_cand:\n runids:")
  cat(paste0(cache$runid_mcache_cand$runid, collapse=", "))
  cat("\n")

  invisible()
}

drf_mcache_object = function(project_dir) {
  getOption("repboxDRF.mcache")
}

drf_has_mcache = function(runid=NULL,file=NULL, project_dir = NULL) {
  drf_has_mcache(runid,file, project_dir)
}

drf_cached_data = function(runid=NULL, file=NULL, project_dir = NULL) {
  drf_get_mcache_data(runid,file, project_dir)
}


drf_init_mcache = function(project_dir,file_mcache_cand = NULL, runid_mcache_cand=NULL,  clear=FALSE) {
  restore.point("drf_init_mcache")
  cache = getOption("repboxDRF.mcache")

  if (!clear & isTRUE(cache$project_dir == project_dir)) return(cache)
  cache = new.env(parent=emptyenv())

  cache$project_dir = project_dir
  cache$data_for_runid = new.env(hash=TRUE, parent=emptyenv())
  cache$data_for_file = new.env(hash=TRUE, parent=emptyenv())
  cache$file_mcache_cand = file_mcache_cand
  cache$runid_mcache_cand = runid_mcache_cand
  options(repboxDRF.mcache = cache)
  invisible(cache)
}

drf_enable_mcache = function(drf, clear = FALSE, use_file_cache = TRUE, use_runid_cache = TRUE) {
  restore.point("drf_enable_mcache")


  if (!has_col(drf$path_df, "found_path")) {
    drf$path_df = drf_add_path_df_cols_for_cache(drf=drf)
  }

  #drf_require(has_col(path_df, "found_path"), "path_df has not column 'found_path' make sure drf_add_path_df_cols_for_cache was called.", drf)

  project_dir = drf$project_dir
  drf_init_mcache(project_dir = project_dir, clear = clear)

  if (use_file_cache) {
    file_mcache_cand = drf_find_file_mcache_cand(drf = drf)
    drf_set_file_mcache_cand(file_mcache_cand, project_dir = project_dir)
  }

  if (use_runid_cache) {
    runid_mcache_cand = drf_find_runid_mcache_cand(drf = drf)
    drf_set_runid_mcache_cand(runid_mcache_cand, project_dir = project_dir)
  }

  invisible(drf)
}


drf_set_file_mcache_cand = function(cache_cand,project_dir=NULL, field="file_mcache_cand") {
  restore.point("drf_set_file_mcache_cand")
  cache = getOption("repboxDRF.mcache")
  new_cache = FALSE
  if (is.null(cache))  {
    new_cache = TRUE
  } else if (!is.null(project_dir)) {
    if (!isTRUE(cache$project_dir == project_dir))
      new_cache = TRUE
  }
  if (new_cache) {
    cache = drf_init_mcache(project_dir=project_dir)
  }
  cache[[field]] = cache_cand
  invisible(cache)
}

drf_set_runid_mcache_cand = function(cache_cand,project_dir=NULL, field="runid_mcache_cand") {
  drf_set_file_mcache_cand(cache_cand,project_dir, field)
}


drf_store_if_mcache_cand = function(data, runid=NULL, file=NULL, project_dir=NULL, overwrite=FALSE) {
  restore.point("drf_save_if_mcache_cand")
  if (!overwrite) {
    if (drf_has_mcache(runid, file, project_dir)) return(FALSE)
  }
  cache = getOption("repboxDRF.mcache")
  if (!is.null(runid)) {
    do_store = runid %in% cache$runid_mcache_cand$runid
  } else if (!is.null(file)) {
    do_store = file %in% cache$file_mcache_cand$file
  }
  if (!do_store) return(FALSE)
  drf_store_mcache(data, runid=runid, file=file, project_dir=project_dir)
}


drf_has_mcache = function(runid=NULL, file=NULL, project_dir = NULL) {
  cache = getOption("repboxDRF.mcache")
  if (is.null(cache)) return(FALSE)
  if (!is.null(project_dir)) {
    if (!isTRUE(cache$project_dir == project_dir))
      return(FALSE)
  }
  if (!is.null(runid)) {
    exists(paste0("r", runid), cache$data_for_runid)
  } else if (!is.null(file)) {
    exists(paste0(file), cache$data_for_file)
  } else {
    stop("provide runid or file")
  }
}


drf_get_mcache_data = function(runid=NULL,file=NULL, project_dir = NULL) {
  restore.point("drf_get_mcache_data")
  cache = getOption("repboxDRF.mcache")
  if (is.null(cache)) return(NULL)
  if (!is.null(project_dir)) {
    if (!isTRUE(cache$project_dir == project_dir)) return(NULL)
  }
  if (!is.null(runid)) {
    cname = paste0("r", runid)
    if (!exists(cname, cache$data_for_runid)) return(NULL)
    get(cname, envir=cache$data_for_runid)
  } else if (!is.null(file)) {
    cache$data_for_file[[file]]
  } else {
    stop("provide runid or file")
  }
}

drf_store_mcache = function(data, runid=NULL, file=NULL, project_dir=NULL) {
  cache = drf_init_mcache(project_dir, clear=FALSE)
  if (!is.null(runid)) {
    cache$data_for_runid[[paste0("r", runid)]] = data
  } else if (!is.null(file)) {
    cache$data_for_file[[file]] = data
  } else {
    stop("provide runid or file")
  }
  invisible(cache)
}

drf_find_file_mcache_cand = function(path_df=drf$path_df, run_df = drf$run_df, drf=NULL) {
  restore.point("drf_find_file_mcache_cand")
  names(path_df)
  drf_require(has_col(path_df, "found_path"), "path_df has not column 'found_path' make sure drf_add_path_df_cols_for_cache was called.", drf)

  runids = unique(path_df$runid)
  path_df %>%
    filter(runid %in% runids, found_path != "") %>%
    group_by(found_path) %>%
    summarize(num_pid = n_distinct(pid)) %>%
    unique() %>%
    rename(file=found_path)
}


# Maximum cache:
# whenever the result of a runid can be used twice and is not dominated
# cache the result
#
# The result of a runid can be used twice if at least two pid use the same runid
# It is not dominated if there is no other larger runid that uses exactly the
# same pid. We then rather want to cache the result from the larger runid
# Not that execution sequence will never violate the order of runids
drf_find_runid_mcache_cand = function(path_df=drf$path_df,ignore_load=TRUE, drf=NULL) {
  restore.point("drf_find_max_cache_runids")

  df = path_df %>%
    # only use runid that
    filter(is_load_mod)
  if (ignore_load) {
    df = df %>% filter(cmd_type != "load")
  }

  df %>%
    group_by(runid) %>%
    summarize(
      num_pid = n(),
      pid_str = paste0(sort(pid), collapse=",")
    ) %>%
    # there at least two pathes with the same runid
    filter(num_pid >= 2) %>%
    # get for every unique path combination
    # the largest runid
    group_by(pid_str) %>%
    filter(runid == max(runid))
}



drf_path_df_to_split_df = function(path_df) {
  split_df  = path_df %>%
    group_by(runid) %>%
    mutate(
      earliest_pid = min(pid)
    ) %>%
    group_by(pid, earliest_pid) %>%
    slice(n())
}


