example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"
  drf = drf_load(project_dir)
  drf$pids
  drf$path_df %>%
    group_by(pid) %>%
    summarize(ncmd = n())
  pid = 188

  # test caches
  file_mcache_cand = drf_find_file_mcache_cand(drf=drf)
  runid_mcache_cand = drf_find_runid_mcache_cand(drf=drf)
  drf_set_file_mcache_cand(file_mcache_cand, project_dir)
  drf_set_runid_mcache_cand(runid_mcache_cand, project_dir)

  class(drf_cache_object())
  names(drf_cache_object())

  drf_cache_info()

  data = drf_get_data(pid, drf=drf,update_rcode = TRUE)




}

#' The main function for metareg regressions
#' get the data set for a particular regression specified with runid

#' @param runid The runid of the command (usually regression) for which the data set shall be obtained
#' @param drf The drf object loaded with drf_load(project_dir)
#' @param filtered Shall the data set already be filtered according to the 'if' and 'in' conditions in the Stata command. Default TRUE
#' @param before Only for commands that modify data. If TRUE (default) return data set before the command modifies the data
#' @param update_rcode Set TRUE if the repbox code for R translation has been updated and the old R code is no longer up-to-date. Mostly relevant during development of the repbox package.
#' @param exec_env in which environment shall the R code that loads and modifies the data be evaluated, default a new env.
#' @param pid Alias for runid in this command (pid stands for path ID which is the runid of the final command in a path from path_df)

drf_get_data = function(runid=pid, drf, filtered=TRUE, before=TRUE, update_rcode=FALSE, exec_env = new.env(parent = globalenv()), pid=NULL) {
  restore.point("drf_get_data")
  if (is.null(runid)) {
    stop("Specify a runid (or pid as synonym).")
  }
  runids = drf_runids(drf)
  if (!runid %in% runids) {
    stop("runid is not part of any DRF path. We only build paths that lead to a successfully run regression.")
  }
  path_df = drf$path_df
  pid = first(path_df$pid[path_df$runid == runid])

  path_df_full = path_df[path_df$pid == pid,]

  if (before) {
    path_df_sub = path_df_full[path_df_full$runid < runid,]
  } else {
    path_df_sub = path_df_full[path_df_full$runid <= runid,]
  }

  exec_runids = path_df_sub$runid
  run_df = drf$run_df
  if (!has_col(run_df, "rcode") | update_rcode) {
    # Ensure full context is passed to rcode generation, including the target `pid`
    run_df = drf_run_df_create_rcode(run_df, runids=path_df_full$runid)
  }
  rows = match(exec_runids, run_df$runid)
  rows = rows[run_df$rcode[rows] != ""]

  rcode = run_df$rcode[rows]

  if (length(rcode)==0) {
    stop("No R code found for getting data. That looks like a bug.")
  }

  # --- INJECT CACHE LOAD CODE IF APPLICABLE ---
  if (length(exec_runids) > 0) {
    first_runid = exec_runids[1]
    first_row = match(first_runid, run_df$runid)
    if (!is.na(first_row) && isTRUE(run_df$has_file_cache[first_row])) {
      drf_rel_path = paste0("cached_dta/", basename(run_df$drf_cache_file[first_row]))
      cache_load_code = paste0(
        'data = drf_load_data(project_dir, "', drf_rel_path ,'")\n',
        'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
        'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
      )
      rcode[1] = cache_load_code
    }
  }
  # --------------------------------------------

  if (filtered) {
    filter_code = drf_get_filter_code(pid, drf)
    if (length(filter_code) > 0) {
      rcode = c(rcode, filter_code)
    }
  }

  # Simple execution of R code
  rcode_call = parse(text = paste0(rcode, collapse="\n"))
  exec_env$project_dir = drf$project_dir
  eval(rcode_call, envir = exec_env)

  # Crucial: Always return the manipulated data frame explicitly
  exec_env$data
}

