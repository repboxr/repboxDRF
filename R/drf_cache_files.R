example = function() {
  project_dir = "~/repbox/projects/aejapp_1_3_4"
  drf = drf_load(project_dir)

  run_df = drf$run_df
  names(run_df)
  path_df = drf$path_df
  cached_runids = drf_get_cached_runids(project_dir)
}


drf_has_cache_file = function(project_dir, runid) {
  cache_dir = file.path(project_dir, "drf/cached_dta")
  files = paste0(cache_dir, "/", runid, "_cache.dta")
  file.exists(files)
}

drf_load_cache_file = function(project_dir, runid) {
  cache_dir = file.path(project_dir, "drf/cached_dta")
  file = paste0(cache_dir, "/", runid, "_cache.dta")
  haven::read_dta(file)
}


# runids that currently have file caches
drf_get_cached_runids = function(project_dir=drf$project_dir, drf) {
  cache_dir = file.path(project_dir, "drf/cached_dta")
  files = list.files(cache_dir,full.names = FALSE)
  as.integer(str.left.of(files,"_"))
}



drf_import_stata_caches = function(drf, move = TRUE) {
  restore.point("drf_import_stata_caches")
  src_dir = file.path(drf$project_dir, "repbox", "stata", "cached_dta")
  dest_dir = file.path(drf$project_dir, "drf", "cached_dta")

  drf$run_df$has_file_cache = rep(FALSE, NROW(drf$run_df))
  drf$run_df$drf_cache_file = rep(NA_character_, NROW(drf$run_df))

  if (!dir.exists(src_dir)) return(drf)

  cache_info_file = file.path(drf$project_dir, "repbox", "stata", "cache_files.Rds")
  if (!file.exists(cache_info_file)) return(drf)

  cache_df = readRDS(cache_info_file)
  if (NROW(cache_df) == 0) return(drf)

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  cache_df$drf_cache_file = file.path(dest_dir,paste0(cache_df$runid, "_cache.dta"))

  for (i in seq_len(NROW(cache_df))) {
    src_file = file.path(src_dir, cache_df$cache_file[i])
    dest_file = cache_df$drf_cache_file[i]

    if (file.exists(src_file)) {
      if (move) {
        file.rename(src_file, dest_file)
      } else {
        file.copy(src_file, dest_file, overwrite = TRUE)
      }
    }
  }

  #drf$cache_df = cache_df %>% filter(!is.na(runid))
  return(drf)
}

drf_available_file_caches = function(drf, path_df=NULL) {
  restore.point("drf_available_file_caches")
  cache_dir = file.path(drf$project_dir, "drf", "cached_dta")
  cache_files = list.files(cache_dir, glob2rx("*.dta"),full.names = FALSE)

  available_caches = as.integer(str.left.of(basename(cache_files),"_cache.dta"))

  if (!is.null(path_df)) {
    available_caches = intersect(available_caches, path_df$runid)
  }
  available_caches
}

drf_find_save_cache = function(path_df, c_runids, dep_df=drf$dep_df, drf) {
  restore.point("drf_find_save_cache")

  # xi dependencies don't invalidate a cache
  # only r() and e() dependencies matter
  dep_df = dep_df %>% filter(dep_type != "xi")

  is_save = function(c_runid) {
     skipped_runids = path_df$runid[path_df$runid <= c_runid]
    remaining_runids = path_df$runid[path_df$runid > c_runid]

    has_dep = any(dep_df$runid %in% remaining_runids & dep_df$source_runid %in% skipped_runids)
    !has_dep
  }


  for (c_runid in rev(sort(c_runids))) {
    if (is_save(c_runid)) {
      return(c_runid)
    }
  }
  return(NULL)

}


drf_apply_caches = function(drf, just_pids=NULL) {
  restore.point("drf_apply_caches")

  cache_dir = file.path(drf$project_dir, "drf", "cached_dta")
  cache_files = list.files(cache_dir, glob2rx("*.dta"),full.names = FALSE)

  available_caches = as.integer(str.left.of(basename(cache_files),"_cache.dta"))

  if (length(available_caches) == 0) return(drf)

  path_df = drf$path_df
  run_df = drf$run_df
  pids = unique(path_df$pid)
  if (!is.null(just_pids)) {
    pids = intersect(pids, just_pids)
  }

  new_path_df_list = vector("list", length(pids))

  for (i in seq_along(pids)) {
    current_pid = pids[i]
    pdf = path_df[path_df$pid == current_pid, ]

    # Check if there are save caches for this specific path
    # A cache is save if there is no e() or r()
    # dependency on an earlier command
    c_runids = intersect(pdf$runid, available_caches)
    c_runid = drf_find_save_cache(pdf, c_runids, dep_df=drf$dep_df)
    cache_applied = FALSE
    if (!is.null(c_runid)) {
      # Truncate the path to start EXACTLY at the cached runid
      pdf = pdf[pdf$runid >= c_runid, ]

      # Mark run_df globally so code generators know this runid can act as a load point
      cache_row_idx = match(c_runid, run_df$runid)
      drf$run_df$has_file_cache[cache_row_idx] = TRUE

      # Match to cache filename
      cache_filename = paste0(cache_dir,"/", c_runid, "_cache.dta")
      drf$run_df$drf_cache_file[cache_row_idx] = cache_filename[1]

      cache_applied = TRUE
    }
    new_path_df_list[[i]] = pdf
  }

  drf$path_df = bind_rows(new_path_df_list)
  return(drf)
}
