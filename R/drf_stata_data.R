# Functions related to data set
# already needed when generating Stata code
drf_run_df_add_data_paths = function(run_df = drf$run_df, drf_dir = drf$drf_dir, drf = NULL) {
  restore.point("drf_run_df_add_data_paths")
  project_dir = drf$project_dir

  im_file = file.path(project_dir, "repbox", "stata", "intermediate_data.Rds")
  if (file.exists(im_file)) {
    im_df = readRDS(im_file)
  } else {
    im_df = NULL
  }

  run_df$is_intermediate = FALSE
  run_df$im_source_path = NA_character_
  run_df$org_data_path = NA_character_

  for (i in seq_len(NROW(run_df))) {
    fp = run_df$found_path[i]
    if (!nzchar(fp) || is.na(fp)) next

    is_im = FALSE
    if (!is.null(im_df) && NROW(im_df) > 0 && run_df$cmd_type[i] %in% c("load", "merge")) {
      # Find latest save of this file before current runid
      past_saves = im_df[im_df$file_path == fp & im_df$runid < run_df$runid[i], ]
      if (NROW(past_saves) > 0) {
        latest_save = past_saves[which.max(past_saves$runid), ]
        is_im = TRUE

        if (isTRUE(latest_save$has_copy)) {
          source_physical = file.path(project_dir, "repbox", "stata", "intermediate_data", latest_save$copy_path)
          dest_rel = latest_save$copy_path
        } else {
          source_physical = file.path(project_dir, "mod", latest_save$file_path)
          dest_rel = latest_save$copy_path
        }

        run_df$is_intermediate[i] = TRUE
        run_df$im_source_path[i] = source_physical
        run_df$org_data_path[i] = file.path(drf_dir, "im_data", dest_rel)
      }
    }

    if (!is_im) {
      run_df$org_data_path[i] = file.path(drf_dir, "org_data", fp)
    }
  }

  run_df$has_org_data = FALSE
  for (i in seq_len(NROW(run_df))) {
     if (is.na(run_df$org_data_path[i])) next
     if (run_df$is_intermediate[i]) {
        run_df$has_org_data[i] = file.exists(run_df$im_source_path[i])
     } else {
        run_df$has_org_data[i] = file.exists(file.path(project_dir, "mod", run_df$found_path[i])) |
                                 file.exists(file.path(project_dir, "org", run_df$found_path[i]))
     }
  }

  drf_cache_file = file.path(drf_dir, "cached_dta", paste0(run_df$runid,"_cache.dta"))
  has_file_cache = file.exists(drf_cache_file)
  run_df$drf_cache_file = ifelse(has_file_cache, drf_cache_file, NA_character_)
  run_df$has_file_cache = has_file_cache
  run_df

}


drf_data_load_needs_clear = function(run_df=NULL, cmd=run_cmd$cmd, cmd_type=run_df$cmd_type) {
  cmd_type %in% c("load")
}

# This function just replaces the data path
drf_replace_run_df_code_data_path = function(run_df =drf$run_df, drf_dir = drf$drf_dir, drf = NULL, cmd_types = c("load","merge"), rows=NULL, prefer_cache = FALSE) {
  restore.point("drf_replace_run_df_org_data_path")

  if (!has_col(run_df, "org_data_path")) {
    run_df = drf_run_df_add_data_paths(run_df, drf_dir=drf_dir, drf=drf)
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
  temp = run_df[rows,]
  org_rows = rows[run_df$has_org_data[rows]]
  add_clear = drf_data_load_needs_clear(run_df[org_rows,])
  run_df$code[org_rows] = replace_stata_cmdline_path(run_df$code[org_rows], paste0('"',run_df$org_data_path[org_rows],'"'),add_clear=add_clear)

  # TO DO: cache replace:
  # If we load a cache, we load the cache file with a complete new load command
  # e.g. use "cachefile.dta", clear

  # ...
  run_df
}

drf_copy_org_data = function(project_dir=drf$project_dir, run_df=drf$run_df, runids=drf_runids(drf),  move_from_mod=TRUE, drf=NULL, overwrite=FALSE) {
  restore.point("drf_copy_org_data")

  require_project_dir(project_dir)
  if (!dir.exists(project_dir)) stop()

  load_df = run_df %>%
    filter(runid %in% runids, cmd_type %in% c("load","merge")) %>%
    filter(nzchar(found_path) & !is.na(found_path)) %>%
    select(found_path, org_data_path, is_intermediate, im_source_path) %>%
    distinct()

  if (NROW(load_df) == 0) return(invisible(NULL))

  # 1. Copy intermediate data
  im_df = load_df %>% filter(is_intermediate)
  if (NROW(im_df) > 0) {
    for (i in seq_len(NROW(im_df))) {
      src = im_df$im_source_path[i]
      dest = im_df$org_data_path[i]
      if (!overwrite && file.exists(dest)) next
      if (!file.exists(src)) {
        cat("\nIntermediate data source not found: ", src)
        next
      }
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (move_from_mod && startsWith(src, file.path(project_dir, "mod"))) {
        file.rename(src, dest)
      } else {
        file.copy(src, dest, overwrite = overwrite)
      }
    }
  }

  # 2. Copy original data
  org_df = load_df %>% filter(!is_intermediate)
  if (NROW(org_df) > 0) {
    dest_files = org_df$org_data_path
    data_files = org_df$found_path

    if (!overwrite) {
      has = file.exists(dest_files)
      dest_files = dest_files[!has]
      data_files = data_files[!has]
    }

    if (length(dest_files) > 0) {
      dirs = unique(dirname(dest_files))
      for (d in dirs) {
        dir.create(d, recursive=TRUE, showWarnings = FALSE)
      }

      mod_files = file.path(project_dir, "mod", data_files)
      has = file.exists(mod_files)
      if (any(has)) {
        if (move_from_mod) {
          file.rename(mod_files[has], dest_files[has])
        } else {
          file.copy(mod_files[has], dest_files[has], overwrite=overwrite)
        }
      }

      still_missing = !file.exists(dest_files)
      if (any(still_missing)) {
        org_files = file.path(project_dir, "org", data_files[still_missing])
        has = file.exists(org_files)
        if (any(has)) {
          file.copy(org_files[has], dest_files[still_missing][has], overwrite=overwrite)
        }
      }

      final_missing = !file.exists(dest_files)
      if (any(final_missing)) {
        cat("\nThe data set(s) ", paste0(data_files[final_missing], collapse=", "),
            " were not found in mod or org folder and could not be copied to drf/org_data.")
      }
    }
  }

  return(invisible(NULL))
}

