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

  class(drf_mcache_object())
  names(drf_mcache_object())

  drf_mcache_info()

  data = drf_get_data(pid, drf=drf,update_rcode = TRUE)




}

#' The main function for metareg regressions
#' get the data set for a particular regression specified with runid

#' @param runid The runid of the command (usually regression) for which the data set shall be obtained
#' @param drf The drf object loaded with drf_load(project_dir)
#' @param update_rcode Set TRUE if the repbox code for R translation has been updated and the old R code is no longer up-to-date. Mostly relevant during development of the repbox package.
#' @param exec_env in which environment shall the R code that loads and modifies the data be evaluated, default a new env.
#' @param pid Alias for runid in this command (pid stands for path ID which is the runid of the final command in a path from path_df)
drf_get_data = function(runid=pid, drf, update_rcode=FALSE, exec_env = new.env(parent = globalenv()), filtered=TRUE, pid=NULL, use_mcache=TRUE) {
  restore.point("drf_get_data")
  project_dir = drf$project_dir
  if (is.null(runid)) {
    stop("Specify a runid (or pid as synonym).")
  }
  runids = drf_runids(drf)
  if (!runid %in% runids) {
    stop("runid is not part of any DRF path. We only build paths that lead to a successfully run regression.")
  }

  # check if we have a direct cache for the runid
  path_df = drf$path_df
  pid = first(path_df$pid[path_df$runid == runid])

  # is final regression data set: we may get memory cache
  if (runid==pid & filtered) {

    if (use_mcache) {
      data = drf_get_mcache_data(runid = runid, project_dir = drf$project_dir)
      if (!is.null(data)) {
        return(data)
      }
    }
    if (drf_has_cache_file(project_dir, runid)) {
      data = drf_load_cache_file(project_dir, runid)
      if (use_mcache) {
        drf_store_if_mcache_cand(data, runid = runid, project_dir = drf$project_dir)
      }
      return(data)
    }
  }


  path_df_full = path_df[path_df$pid == pid,]
  path_df_sub = path_df_full[path_df_full$runid < runid,]

  mcache_runid = NULL
  if (use_mcache) {
    mcache_runid = drf_get_best_runid_mcache(drf, path_df_sub)
  }
  if (!is.null(mcache_runid)) {
    path_df_sub = path_df_sub[path_df_sub$runid > mcache_runid,]
    exec_env$data = drf_get_mcache_data(runid=mcache_runid, project_dir = drf$project_dir)
  }

  exec_runids = path_df_sub$runid
  run_df = drf$run_df
  if (!has_col(run_df, "rcode") | update_rcode) {
    # Ensure full context is passed to rcode generation, including the target `pid`
    run_df = drf_run_df_create_rcode(run_df, runids=path_df_full$runid)
  }
  rows = match(exec_runids, run_df$runid)
  # don't remove those rows might be used for cache
  #rows = rows[run_df$rcode[rows] != ""]

  run_df = run_df[rows,]

  rcode = run_df$rcode

  if (length(rcode)==0) {
    stop("No R code found for getting data. That looks like a bug.")
  }

  # INJECT CACHE LOAD CODE IF APPLICABLE ---
  if (NROW(run_df) > 0 & is.null(mcache_runid)) {
    first_runid = run_df$runid[1]
    if (isTRUE(run_df$has_file_cache[1])) {
      drf_rel_path = paste0("cached_dta/", basename(run_df$drf_cache_file[1]))
      cache_load_code = paste0(
        'data = drf_load_data(project_dir, "', drf_rel_path ,'")\n',
        'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
        'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
      )
      rcode[1] = cache_load_code
    }
  }

  if (filtered) {
    filter_code = drf_get_filter_code(pid, drf)
    if (length(filter_code) > 0) {
      rcode = c(rcode, filter_code)
    }
  }

  # Simple execution of R code
  if (FALSE) {
    data = drf_eval_r_code_stepwise(project_dir, rcode, exec_env)
  }

  rcode_call = parse(text = paste0(rcode, collapse="\n"))
  exec_env$project_dir = drf$project_dir
  eval(rcode_call, envir = exec_env)
  data = exec_env$data

  if (use_mcache & filtered) {
    drf_store_if_mcache_cand(data, runid = runid, project_dir = drf$project_dir)
  }

  data
}

# eval r code stepwise with try catch: return error info
drf_eval_r_code_stepwise = function(project_dir, rcode, env) {
  restore.point("drf_eval_r_code_stepwise")
  env$project_dir = drf$project_dir
  for (i in seq_along(rcode)) {
    code = rcode[i]
    if (trimws(code)=="") next
    rcode_call = parse(text = paste0(rcode, collapse="\n"))
    eval(rcode_call, envir = env)
  }
  if (FALSE) {
    li = as.list(env)
  }
  env$data

  s2r_eval_if_in
}
