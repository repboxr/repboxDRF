example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  code_df = drf_stata_code_df(drf, path_merge = "load_natural")

}


# Writes stata code skeleton for direct replication of one or
# multiple regression commands
# The regression commands themselves will be palceholder of form
# {{runid-3562}}

# TO DO: omit unneccesary previous reg steps.
# They are currently always included in path since
# later regressions may need them if r() or something is used from it.
drf_stata_code_df = function(drf,runids=NULL, path_merge = c("none", "load", "natural", "load_natural")[4]) {
  restore.point("drf_stata_code_skel")
  pids = runids
  path_df = drf$path_df
  if (!is.null(pids)) {
    path_df = path_df %>%
      filter(pid %in% pids)
  }
  pids = unique(path_df$pid)
  if (length(pids)<=1) path_merge = "none"

  # Pick only those run commands that are used
  # in the selected paths
  run_df = drf$run_df
  run_df = run_df %>% semi_join(path_df, by="runid")

  # Create code for every used command line
  run_df$code = run_df$cmdline

  # Adapt data sets in load and merge commands
  run_df = drf_replace_run_df_code_data_path(run_df = run_df, drf=drf)
  run_df$data_path = run_df$org_data_path

  path_li = split(path_df, path_df$pid)
  code_li = NULL
  pid = pids[1]
  if (path_merge == "none") {
    code_li = lapply(pids, function(pid) {
      pdf = path_li[[as.character(pid)]]
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="")
    })
    code_df = bind_rows(code_li)
    return(code_df)
  }
  ps_df = path_df %>%
    group_by(pid) %>%
    summarize(
      first_runid = min(runid),
      last_runid = max(runid),
    ) %>%
    left_join(run_df %>% select(first_runid=runid, data_path), by="first_runid")

  data_df = ps_df %>%
    group_by(data_path) %>%
    summarize(
      data_runid = min(first_runid),
      data_num_paths = n()
    ) %>%
    arrange(data_runid)

  ps_df = ps_df %>%
    left_join(data_df, by="data_path")

  merge_load = path_merge %in% c("load", "load_natural")
  merge_natural = path_merge %in% c("natural", "load_natural")

  if (merge_load) {
    ps_df = ps_df %>%
      ungroup() %>%
      arrange(data_runid, first_runid, last_runid) %>%
      mutate(
        restore_data = is.true(lag(data_runid)==data_runid),
        preserve_data = !restore_data & data_num_paths > 1
      )
  } else {
    ps_df = ps_df %>%
      arrange(first_runid, last_runid) %>%
      mutate(restore_data = FALSE, preserve_data=FALSE)
  }

  pids = ps_df$pid
  if (!merge_natural) {
    code_li = lapply(pids, function(pid) {
      pdf = path_li[[as.character(pid)]]
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)

      ps = ps_df[ps_df$pid==pid,]
      if (ps$preserve_data) {
        rdf$code[1] = paste0(rdf$code[1],"\npreserve")
        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = paste0("* Restore previously loaded data set", basename(ps$data_path), "\nrestore")
        rdf$aux_cmd_type[1] = paste0("restore")
      }
    })
    code_df = bind_rows(code_li)
    return(code_df)
  }

  # merge_natural==TRUE
  # means that if in the original script regressions are run consequutively
  # after each other, we will also do this in this generated script

  code_li = vector("list", length(pids))
  opdf = NULL
  counter = 0

  while (counter < length(pids)) {
    counter = counter+1
    cat("\n", counter, "of", length(pids), pid,"\n")
    pid = pids[counter]
    pdf = path_li[[as.character(pid)]]
    if (is.null(opdf) | NROW(opdf)>=NROW(pdf)) {
      restart = TRUE
    } else {
      restart = !all(opdf$runid == pdf$runid[1:NROW(opdf)])
    }

    if (restart) {
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)

      ps = ps_df[ps_df$pid==pid,]

      if (ps$preserve_data) {
        rdf$code[1] = paste0(rdf$code[1],"\npreserve")
        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = "* Restore previously loaded data set\nrestore"
        rdf$aux_cmd_type[1] = paste0("restore")
      }
      opdf = pdf


    } else if (!restart) {
      # continue with commands that continue previous path
      npdf = pdf %>% filter(runid > max(opdf$runid))
      opdf = pdf
      pdf = npdf

      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)
    }
    code_li[[counter]] = rdf
  }
  code_df = bind_rows(code_li)
  all(pids %in% code_df$pid)
  code_df
}
