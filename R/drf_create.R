# Creates a new DRF
# can be run after a reproduction run

example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_create(project_dir, overwrite=TRUE)

}

has_drf = function(project_dir) {
  file.exists(file.path(project_dir,"drf/path_df.fst"))
}

drf_create = function(project_dir, parcels=list(), acmds = drf_acmds(), overwrite=FALSE, move_from_mod=TRUE) {
  restore.point("drf_init")

  if (!overwrite & has_drf(project_dir)) {
    return(NULL)
  }
  project_dir = normalizePath(project_dir)

  drf = list(project_dir = project_dir, drf_dir = file.path(project_dir, "drf"), parcels = parcels, acmds=acmds)
  drf$parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)


  run_df = drf_make_run_df(drf=drf)
  if (is.null(run_df)) {
    cat(("\nNo stata_run_cmd parcel exists, cannot create drf.\n"))
    return(NULL)
  }

  drf$run_df = run_df

  drf$pid = drf_find_pid(drf$run_df, drf$acmds)

  drf$path_df = drf_make_paths(drf)
  drf$index_df = drf_save_path_df(drf=drf)

  drf$ap_df = drf_path_df_to_ap_df(drf$path_df, drf$run_df)

  drf_copy_org_data(drf=drf, move_from_mod=move_from_mod)

  invisible(drf)
}

drf_make_run_df = function(project_dir = drf$project_dir, parcels=drf$parcels, drf = list(parcels=list())) {
  restore.point("drf_make_run_df")
  parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)
  run_df = parcels$stata_run_cmd$stata_run_cmd

  start_time = run_df$start_time[1]
  run_df$ok = !is.true(run_df$errcode != 0) & !is.true(run_df$missing_data)
  run_df$start_sec = sec_since_start(run_df$start_time, start_time)
  run_df$end_sec = sec_since_start(run_df$end_time, start_time)
  run_df$dur_sec =run_df$end_sec - run_df$start_sec

  cmd_types = drf_stata_cmd_types_vec()
  run_df$cmd_type = cmd_types[run_df$cmd]

  rows = run_df$cmd_type == "reg"
  inds = which(has.substr(run_df$cmdline[rows]," _I"))
  run_df$is_I_reg = FALSE
  run_df$is_I_reg[rows[inds]] = TRUE

  rows = run_df$cmd_type %in% c("reg","mod")
  inds = which(stri_detect_regex(run_df$cmdline[rows],"(^|[ \\:])[ ]*xi[ \\:]"))
  run_df$is_xi = FALSE
  run_df$is_xi[inds] = TRUE

  run_df
}

# Analysis commands
drf_acmds = function() {
  # Don't use post regression commands for normal paths
  unique(c(repboxStata::stata_cmds_reg(), repboxStata::stata_cmds_quasireg()))
  #unique(c(repboxStata::stata_cmds_reg(), repboxStata::stata_cmds_postreg_comp(), repboxStata::stata_cmds_quasireg()))

}



drf_find_pid = function(run_df,acmds = drf_acmds()) {
  restore.point("drf_find_pid")
  run_df$runid[(run_df$cmd %in% acmds) & run_df$ok]
}

require_project_dir = function(project_dir) {
  if (is.null(project_dir)) stop("You must supply a valid project_dir")
  if (!dir.exists(project_dir)) stop("You must supply an existing project_dir")
}


require_drf_dir = function(drf_dir) {
  if (is.null(drf_dir)) stop("You must supply a valid drf_dir")
  project_dir = dirname(drf_dir)
  if (dir.exists(project_dir)) stop("You must supply a valid drf_dir for an existing project_dir.")

}
