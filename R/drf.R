example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)

}


# To do: variant that is faster for selected paths
drf_load = function(project_dir, parcels=list()) {
  restore.point("drf_load")
  project_dir = normalizePath(project_dir)
  drf = list(project_dir = project_dir, drf_dir = file.path(project_dir, "drf"), parcels = parcels)
  drf$parcels = repboxDB::repdb_load_parcels(project_dir, c("stata_run_cmd", "r_trans"), parcels=parcels)
  drf$run_df = drf_make_run_df(drf=drf,add_rcode = TRUE)
  drf$path_df = drf_load_path_df(drf=drf)
  drf$path_df = drf_add_path_df_cols_for_cache(drf=drf)
  drf$runids = drf_runids(drf)
  drf$pids= drf_pids(drf)
  drf$scalar_map = read_rds_or_null(file.path(project_dir, "drf/scalar_map.Rds"))
  drf = drf_scalar_map_to_scalar_code(drf)
  drf$dep_df = read_rds_or_null(file.path(project_dir, "drf/dep_df.Rds"))

  drf
}

drf_pids = function(drf, path_df=drf$path_df) {
  if (!is.null(drf[["runids"]])) drf$pids
  unique(drf$path_df$pid)
}

drf_runids = function(drf, path_df=drf$path_df) {
  if (!is.null(drf[["runids"]])) drf$runids
  unique(drf$path_df$runid)
}


drf_make_run_df = function(project_dir = drf$project_dir, parcels=drf$parcels, drf = list(parcels=list()), add_rcode = TRUE) {
  restore.point("drf_make_run_df")
  parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)
  run_df = parcels$stata_run_cmd

  start_time = run_df$start_time[1]
  run_df$ok = !is.true(run_df$errcode != 0) & !is.true(run_df$missing_data)
  run_df$start_sec = sec_since_start(run_df$start_time, start_time)
  run_df$end_sec = sec_since_start(run_df$end_time, start_time)
  run_df$dur_sec =run_df$end_sec - run_df$start_sec

  cmd_types = drf_stata_cmd_types_vec()
  run_df$cmd_type = cmd_types[run_df$cmd]

  # I think stata2r deals with xi automatically
  # so probably no need anymore to store such cols in run_df
  if (FALSE) {
    rows = run_df$cmd_type == "reg"
    inds = which(has.substr(run_df$cmdline[rows]," _I"))
    run_df$is_I_reg = FALSE
    run_df$is_I_reg[rows[inds]] = TRUE

    rows = run_df$cmd_type %in% c("reg","mod")
    inds = which(stri_detect_regex(run_df$cmdline[rows],"(^|[ \\:])[ ]*xi[ \\:]"))
    run_df$is_xi = FALSE
    run_df$is_xi[inds] = TRUE
  }

  # Resolve explicit intermediate tracking before generating scripts / caches
  run_df = drf_run_df_add_data_paths(run_df, drf_dir = file.path(project_dir, "drf"), drf = drf)

  if (add_rcode) {
    parcels = repboxDB::repdb_load_parcels(project_dir, "r_trans", parcels=parcels)
    rt_df = parcels$r_trans
    if (is.null(rt_df)) {
      stop("\nNo r_trans parcel exists generate it.")
    }
    if (!is.null(rt_df)) {
      run_df = remove_cols(run_df, "rcode")
      run_df = left_join(run_df, rt_df, by="runid") %>%
        mutate(rcode = na.val(rcode, ""))
    }
  }

  run_df
}

# Needs to be refined. We do not yet know how to handle regressions
# that modify the data set.
# We probably need a special data modification translation
drf_add_path_df_cols_for_cache = function(path_df = drf$path_df, run_df=drf$run_df, drf=NULL, overwrite=FALSE) {
  restore.point("drf_mark_path_df_is_load_mod")
  if (!overwrite) {
    if (has_col(path_df, "is_load_mod")) return(path_df)
  }
  cols = setdiff(names(path_df), c("cmd_type","found_path", "is_xi", "is_I_reg","is_load_mod"))
  path_df = path_df[,cols]
  path_df %>%
    #left_join(run_df %>% select(runid, cmd_type,  found_path, is_xi, is_I_reg), by="runid") %>%
    left_join(run_df %>% select(runid, cmd_type,  found_path), by="runid") %>%
    mutate(is_load_mod = cmd_type %in% c("load","mod","merge"))

}

drf_require = function(cond, msg, drf) {
  if (!isTRUE(all(cond))) {
    stop(msg)
  }
}

