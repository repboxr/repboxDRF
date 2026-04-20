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
  restore.point("drf_create")

  if (!overwrite & has_drf(project_dir)) {
    return(NULL)
  }
  project_dir = normalizePath(project_dir)

  drf = list(project_dir = project_dir, drf_dir = file.path(project_dir, "drf"), parcels = parcels, acmds=acmds)
  drf$parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)


  run_df = drf_make_run_df(drf=drf,add_rcode = FALSE)
  if (is.null(run_df)) {
    cat(("\nNo stata_run_cmd parcel exists, cannot create drf.\n"))
    return(NULL)
  }

  drf$run_df = run_df

  drf = drf_add_scalar_map(drf)
  drf = drf_add_dep_df(drf)

  drf$pids = drf_find_pid(drf$run_df, drf$acmds)

  drf$path_df = drf_make_paths(drf)
  drf$runids = drf_runids(drf)

  drf_copy_org_data(drf=drf, move_from_mod=move_from_mod)

  # Incorporate Caches cleanly
  drf = drf_import_stata_caches(drf, move = move_from_mod)
  drf = drf_apply_caches(drf)

  # Save path_df index AFTER caches have definitively resolved the shortest paths
  drf$index_df = drf_save_path_df(drf=drf)


  drf = drf_make_r_trans_parcel(drf)

  invisible(drf)
}


# gets run_df from parcels including up-to-date cmd_type column
repbox_get_run_df = function(project_dir, parcels=list()) {
  parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)
  run_df = parcels$stata_run_cmd
  cmd_types = drf_stata_cmd_types_vec()
  run_df$cmd_type = cmd_types[run_df$cmd]
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
