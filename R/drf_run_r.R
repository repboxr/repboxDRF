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

#' The main function for metareg regressions
#' get the data set for a particular regression specified with runid
#' @param runid The runid of the command (usually regression) for which the data set shall be obtained
#' @param drf The drf object loaded with drf_load(project_dir)
#' @param update_rcode Set TRUE if the repbox code for R translation has been updated and the old R code is no longer up-to-date. Mostly relevant during development of the repbox package.
#' @param exec_env in which environment shall the R code that loads and modifies the data be evaluated, default a new env.
#' @param pid Alias for runid in this command (pid stands for path ID which is the runid of the final command in a path from path_df)
#' @param adapt_path_to_caches Whether the drf path for the runid shall be adapted to current cache files before building the data preparation pipeline.
drf_get_data = function(runid=pid, drf, update_rcode=FALSE, exec_env = new.env(parent = globalenv()), filtered=TRUE, pid=NULL, use_mcache=TRUE, adapt_path_to_caches=TRUE) {
  restore.point("drf_get_data")
  project_dir = drf$project_dir

  if (is.null(runid)) {
    stop("Specify a runid (or pid as synonym).")
  }

  if (adapt_path_to_caches) {
    drf = drf_apply_caches(drf, just_pids = runid)
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
    run_df = drf_run_df_create_rcode(run_df, runids=path_df_full$runid, drf=drf)
  }
  rows = match(exec_runids, run_df$runid)
  run_df = run_df[rows,]

  rcode = run_df$rcode

  if (length(rcode)==0) {
    stop("No R code found for getting data. That looks like a bug.")
  }

  # If there is an error in the translation path return NULL
  drf = drf_sync_r_err_runids(drf)

  check_runids = run_df$runid
  if (NROW(run_df) > 0 && isTRUE(run_df$has_file_cache[1])) {
    check_runids = check_runids[-1]
  }

  if (any(check_runids %in% drf$r_err_runids)) {
    cat("\nSkip as R translation error on path was noted earlier.\n")
    return(NULL)
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

    # Add possible scalar definitions needed for filter
    scalar_code = NULL
    if (pid %in% drf$scalar_code$runid) {
      rows = which(drf$scalar_code$runid == pid)
      scalar_code = drf$scalar_code$scalar_r_code[rows]
    }
    rcode = c(rcode, scalar_code, filter_code)
  }

  res = drf_eval_create_data_r_code(project_dir = drf$project_dir, rcode=rcode,runid=runid, exec_env=exec_env)

  data = res$data

  # Don't return data if a new translation error was detected
  if (res$has_err) {
    err_lines = res$err_line[res$err_line<=NROW(run_df)]
    runids = run_df$runid[err_lines]
    drf = drf_sync_r_err_runids(drf,runids)
    return(NULL)
  }

  if (use_mcache & filtered & !is.null(data)) {
    drf_store_if_mcache_cand(data, runid = runid, project_dir = drf$project_dir)
  }

  data
}

# We store info about runids whose r translation threw an error
# those runids will then be omitted from the translation to save
# tryCatch time, which can be surprisingly time consuming.
drf_sync_r_err_runids = function(drf, runids=NULL) {
  err_dir = file.path(drf$project_dir, "drf/r_err_runids")

  if (dir.exists(err_dir)) {
    file_runids = as.integer(list.files(err_dir,pattern = "[0-9]+", full.names=FALSE))
    file_runids = file_runids[!is.na(file_runids)]
  } else {
    file_runids = NULL
  }
  drf_runids = union(drf$r_err_runids, runids)

  to_file_err_runids = setdiff(drf_runids, file_runids)
  for (runid in to_file_err_runids){
    if (!dir.exists(err_dir)) dir.create(err_dir, recursive = TRUE)
    writeLines("", file.path(err_dir, runid))
  }
  drf$r_err_runids = union(drf_runids, file_runids)
  drf
}



drf_eval_create_data_r_code = function(project_dir, rcode, runid=NULL, exec_env=NULL) {
  restore.point("drf_eval_create_data_r_code")
  env = new.env(parent = globalenv())
  env$project_dir = project_dir
  env$data = exec_env[["data"]]

  rcode_call = parse(text = paste0(rcode, collapse="\n"))

  err = NULL
  tryCatch(
    eval(rcode_call, envir = env),
    error = function(e) {
      err <<- e
      NULL
    }
  )
  if (!is.null(err)) {
    env = new.env(parent = globalenv())
    res = drf_eval_create_data_r_code_stepwise(project_dir, rcode, env, runid=runid)
    data = res$data
    has_err = res$has_err
    err_line = res$err_line
  } else {
    data = env$data
    has_err = FALSE
    err_line = NULL
  }
  return(list(data=data, has_err=has_err, err_line = err_line))
}


drf_eval_create_data_r_code_stepwise = function(project_dir, rcode, env, runid=NULL) {
  restore.point("drf_eval_r_code_stepwise")

  env$data = NULL
  env$project_dir = project_dir

  rcode = trimws(rcode)

  has_err = FALSE
  err_line = NULL

  for (i in seq_along(rcode)) {
    #cat("\n",i, " ", rcode[i])
    err = NULL
    if (rcode[i]=="") next

    tryCatch(
      {
        expr = parse(text=rcode[i])
        eval(expr, envir = env)
      },
      error = function(e) {
        err <<- e
        NULL
      }
    )
    if (!is.null(err) && !has_err) {
      has_err = TRUE
      err_line = i
      code = rcode[i]
      msg = paste0(
        "runid=", runid,
        " has error in drf_get_data (R translation of data preparation):\n\n",
        code, "\n\n",
        conditionMessage(err)
      )
      repbox_problem(
        msg,
        type = "r_trans_get_data",
        fail_action = "msg",
        project_dir = project_dir,
        runid = runid
      )
      return(list(data=NULL, has_err=TRUE, err_line=err_line))
    }
  }

  list(data = env$data, has_err=FALSE, err_line=NULL)
}


# drf_eval_create_data_r_code = function(project_dir, rcode, runid=NULL, exec_env=NULL) {
#   restore.point("drf_eval_create_data_r_code")
#   env = new.env(parent = globalenv())
#   env$project_dir = project_dir
#   env$data = exec_env[["data"]]
#
#   rcode_call = parse(text = paste0(rcode, collapse="\n"))
#
#   err = NULL
#   tryCatch(
#     eval(rcode_call, envir = env),
#     error = function(e) {
#       err <<- e
#       NULL
#     }
#   )
#   if (!is.null(err)) {
#     env = new.env(parent = globalenv())
#     res = drf_eval_create_data_r_code_stepwise(project_dir, rcode, env, runid=runid)
#     data = res$data
#     has_err = res$has_err
#     err_lines = res$err_lines
#   } else {
#     data = env$data
#     has_err = FALSE
#     err_lines = NULL
#   }
#   return(list(data=data, has_err=has_err, err_lines = err_lines))
# }
#
#
# drf_eval_create_data_r_code_stepwise = function(project_dir, rcode, env, runid=NULL) {
#   restore.point("drf_eval_r_code_stepwise")
#
#   env$data = NULL
#   env$project_dir = project_dir
#
#   rcode = trimws(rcode)
#
#   has_err = FALSE
#   err_lines = NULL
#
#   for (i in seq_along(rcode)) {
#     #cat("\n",i, " ", rcode[i])
#     err = NULL
#     if (rcode[i]=="") next
#
#     tryCatch(
#       {
#         expr = parse(text=rcode[i])
#         eval(expr, envir = env)
#       },
#       error = function(e) {
#         err <<- e
#         NULL
#       }
#     )
#     if (!is.null(err)) {
#       err_lines = c(err_lines, i)
#     }
#
#     if (!is.null(err) && !has_err) {
#       has_err = TRUE
#
#       code = rcode[i]
#       msg = paste0(
#         "runid=", runid,
#         " has error in drf_get_data (R translation of data preparation):\n\n",
#         code, "\n\n",
#         conditionMessage(err)
#       )
#       repbox_problem(
#         msg,
#         type = "r_trans_get_data",
#         fail_action = "msg",
#         project_dir = project_dir,
#         runid = runid
#       )
#     }
#   }
#
#   list(data = env$data, has_err=has_err, err_lines=err_lines)
# }
