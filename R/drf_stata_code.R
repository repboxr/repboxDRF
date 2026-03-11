example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  drf$sc_df = sc_df = drf_stata_code_df(drf, path_merge = "load_natural")


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

  restore_code = function(data_path) {
    # "preserve, restore"
    paste0("* Restore previously loaded data set", basename(data_path), "\nframe copy cache_frame default, replace")
  }
  preserve_code = function() {
    #"\npreserve"
    "\nframe copy default cache_frame, replace"
  }


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
    sc_df = bind_rows(code_li)
    return(sc_df)
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
        rdf$code[1] = paste0(rdf$code[1],preserve_code())
        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = restore_code(ps$data_path)
        rdf$aux_cmd_type[1] = paste0("restore")
      }
    })
    sc_df = bind_rows(code_li)
    return(sc_df)
  }

  # merge_natural==TRUE
  # means that if in the original script regressions are run consequutively
  # after each other, we will also do this in this generated script

  code_li = vector("list", length(pids))
  opdf = NULL
  counter = 0

  while (counter < length(pids)) {
    counter = counter+1
    #cat("\n", counter, "of", length(pids), pid,"\n")
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
        rdf$code[1] = paste0(rdf$code[1],preserve_code())

        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = restore_code(ps$data_path)
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
  sc_df = bind_rows(code_li)
  all(pids %in% sc_df$pid)
  sc_df
}

# Add to Stata code command that save data set caches as dta after some command
drf_add_code_store_cache = function(runids, sc_df=drf$sc_df,drf_dir = drf$drf_dir, drf = NULL) {
  restore.point("drf_add_code_store_cache")
  require_drf_dir(drf_dir)

  cache_dir = file.path(drf_dir, "cache_dta")
  if (!dir.exists(cache_dir)) dir.create(cache_dir)

  cache_files = file.path(cache_dir, paste0("cache_", runids, ".dta"))

  # Will only match first time runid appears in sc_df
  # but that is what we want: caching multiple times makes no sense
  rows = match(runids, sc_df$runid)

  txt = paste0(txt,'\nsave "', cache_files,'", replace\n')
  sc_df$post[rows] = paste0(txt, sc_df$post[rows])
  sc_df
}

drf_add_code_store_if_rows = function(runids, sc_df=drf$sc_df,drf_dir=drf$drf_dir, only_fun_if_rows = TRUE, drf = NULL, outdir = file.path(drf_dir, "if_rows")) {
  restore.point("drf_add_code_store_if_rows")

  rows = match(runids, sc_df$runid)
  cmdline = sc_df$cmdline[rows]

  has_if = stringi::stri_detect_regex(cmdline, "\\bif\\b")
  rows = rows[has_if]
  if (length(rows)==0) return(sc_df)
  cmdline = sc_df$cmdline[rows]

  tab = repboxStata::repbox.re.cmdlines.to.tab(cmdline)
  if_str = tab$if_arg

  # We don't want to save simple if conditions that don't call a
  # Stata function because we could translate them ourselves
  if (only_fun_if_rows) {
    use = has.substr(if_str, "(")
    rows = rows[use]
    if (length(rows)==0) return(sc_df)
    if_str = use
  }
  runids = sc_df$runid[rows]

  if (!dir.exists(outdir))
    dir.create(dir, recursive = TRUE)


  # TO CHECK restore / preserve or deletion of created row
  file = paste0(outdir, "/ifrows_", runids, ".dta")
  sc_df$pre[rows]  = paste0("\n
// Store row number from if condition
preserve
gen r0W_ox_zA___2G__nUm__ = _n
capture noisily keep if ",if_str, "
keep r0W_ox_zA___2G__nUm__
capture noisily save \"",file, "\", replace
restore\n",
    sc_df$pre[row])
  sc_df
}


drf_code_stata_path_header = function(sc_df, header_tpl = "\n***** Path for runid={{pid}} ****\n\n", to_col="pre", append_mode = c("overwrite", "left", "right")[1]) {
  drf_code_adapt(sc_df, function(code_df, ...) {
    stringi::stri_replace_all_fixed(header_tpl,"{{pid}}", code_df$pid)
  },to_col=to_col, append_mode=append_mode, just_path_pos="start")

}
