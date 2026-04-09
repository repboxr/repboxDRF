example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"
  project_dir = "~/repbox/projects/test"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  drf$sc_df = drf_stata_code_df(drf, path_merge = "load_natural")
  drf$rc_df = drf_rcode_df(drf)
}


drf_make_r_trans_parcel = function(drf) {
  restore.point("drf_make_r_trans_parcel")
  run_df = drf_run_df_create_rcode(drf=drf)
  rt_df = run_df %>%
    filter(rcode != "") %>%
    select(runid, rcode)

  drf$parcels[["r_trans"]] = rt_df
  repdb_save_parcels(drf$parcels["r_trans"],file.path(drf$project_dir, "repdb"),check=TRUE)
  drf

}

# Writes stata code skeleton for direct replication of one or
# multiple regression commands
# The regression commands themselves will be palceholder of form
# {{runid-3562}}

# TO DO: omit unneccesary previous reg steps.
# They are currently always included in path since
# later regressions may need them if r() or something is used from it.



drf_run_df_create_rcode = function(run_df=drf$run_df, runids=drf_runids(drf), drf=NULL) {
  restore.point("drf_run_df_create_rcode")

  if (!has_col(run_df, "rcode")) {
    run_df$rcode = rep("", NROW(run_df))
  }
  if (!is.null(runids)) {
    rows = match(runids, run_df$runid)
  } else {
    rows = seq_len(NROW(run_df))
  }
  rows = sort(unique(rows[!is.na(rows)]))

  update_rows = rows
  # if (!overwrite) {
  #   cat("\ndrf_run_df_create_rcode with ovewrite=FALSE might be problematic with respect to row indices. Better make just a single translation.\n")
  #   update_rows = rows[run_df$rcode[rows] == ""]
  # }

  if (length(update_rows)==0) return(run_df)

  # Translate the execution trace up to the max row so `stata2r` maintains full context
  #max_row = max(update_rows)
  stata_code = run_df$cmdline[update_rows]

  # IMPORTANT: Replace internal newlines with spaces to keep 1-to-1 mapping with rows
  stata_code = gsub("\n", " ", stata_code, fixed = TRUE)

  r_df = stata2r::do_to_r(stata_code, return_df = TRUE)

  translated_code = r_df$r_code
  run_df$rcode[update_rows] = ifelse(is.na(translated_code), "", translated_code)

  # Overwrite 'load' commands with repbox's own data loading logic
  inds = update_rows[run_df$cmd_type[update_rows] %in% c("load")]
  if (length(inds)>0) {
    code = paste0(
      'data = drf_load_data(project_dir, "', file.path(run_df$found_path[inds]) ,'")\n',
      'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
      'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
    )
    run_df$rcode[inds] = code
  }
  run_df$rcode = na.val(run_df$rcode, "")

  run_df
}


drf_rcode_df = function(drf,runids=NULL, path_merge = c("none", "load", "natural", "load_natural")[4], update_rcode = FALSE) {
  restore.point("drf_rcode_df")

  # perform path merge like as for stata code
  sc_df = drf_stata_code_df(drf, runids=runids, path_merge=path_merge)
  runids = unique(rc_df$runid)

  run_df = drf$run_df
  if (update_rcode) {
    run_df = drf_run_df_create_rcode(run_df, runids=runids, overwrite=TRUE)
  }

  run_df = drf$run_df %>%
    filter(runid %in% runids)

  rc_df = rc_df %>%
    left_join(run_df %>% select(runid, cmdline,rcode), by="runid") %>%
    mutate(code = rcode, pre = "", post="")
  rc_df
}



