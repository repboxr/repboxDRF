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

  drf$cache_df = cache_df %>% filter(!is.na(runid))
  return(drf)
}

drf_is_cache_safe = function(skipped_df, remaining_df) {
  # Heuristic 1: A cache replaces the dataset context but NOT macro memory context.
  # So, if skipped commands defined any memory elements (local, global, scalar, matrix),
  # it's unsafe to skip them, as remaining code might silently fail or behave incorrectly.
  unsafe_defs = grepl("^\\s*(local|global|scalar|matrix)\\b", skipped_df$cmdline)
  if (any(unsafe_defs)) return(FALSE)

  # Heuristic 2: If remaining commands query r(), e(), or matrices,
  # they might depend on estimation commands that were skipped.
  unsafe_uses = grepl("\\b[re]\\(|\\bmatrix\\b", remaining_df$cmdline)
  if (any(unsafe_uses)) return(FALSE)

  return(TRUE)
}

drf_apply_caches = function(drf) {
  restore.point("drf_apply_caches")
  if (is.null(drf$cache_df) || NROW(drf$cache_df) == 0) return(drf)

  dest_dir = file.path(drf$project_dir, "drf", "cached_dta")
  available_caches = drf$cache_df$runid[file.exists(drf$cache_df$drf_cache_file)]

  if (length(available_caches) == 0) return(drf)

  path_df = drf$path_df
  run_df = drf$run_df
  pids = unique(path_df$pid)

  new_path_df_list = vector("list", length(pids))

  for (i in seq_along(pids)) {
    current_pid = pids[i]
    pdf = path_df[path_df$pid == current_pid, ]

    # Check if there are applicable caches for this specific path
    path_caches = intersect(pdf$runid, available_caches)

    if (length(path_caches) > 0) {
      path_caches = sort(path_caches, decreasing = TRUE)
      cache_applied = FALSE

      for (c_runid in path_caches) {
        skipped_df = run_df[run_df$runid %in% pdf$runid[pdf$runid < c_runid], ]
        remaining_df = run_df[run_df$runid %in% pdf$runid[pdf$runid > c_runid], ]

        if (drf_is_cache_safe(skipped_df, remaining_df)) {
          # Truncate the path to start EXACTLY at the cached runid
          pdf = pdf[pdf$runid >= c_runid, ]

          # Mark run_df globally so code generators know this runid can act as a load point
          cache_row_idx = match(c_runid, run_df$runid)
          drf$run_df$has_file_cache[cache_row_idx] = TRUE

          # Match to cache filename
          cache_filename = drf$cache_df$drf_cache_file[drf$cache_df$runid == c_runid]
          drf$run_df$drf_cache_file[cache_row_idx] = cache_filename[1]

          cache_applied = TRUE
          break
        }
      }
      new_path_df_list[[i]] = pdf
    } else {
      new_path_df_list[[i]] = pdf
    }
  }

  drf$path_df = bind_rows(new_path_df_list)
  return(drf)
}
