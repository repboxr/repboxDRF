example = function() {
  library(repboxDRF)
  project_dir = "~/repbox/projects_test/test"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  drf_add_loop_ignore(drf)

}

drf_load_loop_ignore_info = function(drf) {
  file = file.path(drf$project_dir, "drf/loig_df.Rds")
  if (!file.exists(file)) {
    stop(paste0(file, " does not exist. Re-run drf_create."))
  }
  drf$loig_df = readRDS(file)
  drf
}

drf_apply_loop_ignore = function(drf, path_df = drf$path_df) {
  if (is.null(drf$loig_df)) {
    drf = drf_load_loop_ignore_info(drf)
  }
  drf$path_df = anti_join(path_df, drf$loig_df, by = c("pid", "runid"))
  drf
}


# See metaregBase README on which data modification commands in a loop
# we will ignore by default.
#
# We can compute the loop_ignore.Rds once with the full drf
drf_add_loop_ignore = function(drf, save=TRUE) {
  restore.point("drf_add_loop_ignore")

  run_df = drf$run_df

  # Add loop info to run_df
  run_df = run_df %>%
    group_by(root_file_path, file_path, line) %>%
    mutate(is_repeated = n()>1) %>%
    ungroup() %>%
    mutate(
      new_loop = is_repeated & (!is.true(lag(is_repeated)))
    ) %>%
    mutate(
      loopid = cumsum(new_loop) * is_repeated
    ) %>%
    mutate(
      new_iter = loopid > 0 &
        (
         !is.true(lag(loopid)>0) |
         (is.true(line <= lag(line)) & is.true(lag(loopid)==loopid))
        )
    ) %>%
    group_by(loopid) %>%
    mutate(
      loop_iter = cumsum(new_iter)
    ) %>%
    ungroup()


  pids = drf$pids
  loop_pids_df = run_df %>%
    filter(runid %in% pids) %>%
    select(pid = runid, pid_loopid=loopid, pid_loop_iter=loop_iter) %>%
    filter(pid_loopid >0)

  pids = loop_pids_df$pid

  loig_df = drf$path_df %>%
    semi_join(loop_pids_df, by="pid") %>%
    left_join(loop_pids_df, by="pid") %>%
    left_join(run_df %>% select(runid, loopid, loop_iter)) %>%
    mutate(remove = loopid > 0 & loopid == pid_loopid & loop_iter < pid_loop_iter) %>%
    filter(remove)


  # Also remove data preparation steps from earlier loops
  # with regressions that are inside
  after_loig_df = drf$path_df %>%
    left_join(run_df %>% select(runid, loopid), by="runid") %>%
    group_by(pid) %>%
    mutate(pid_loopid = loopid[runid==pid]) %>%
    ungroup() %>%
    # only keep pid that are not in the same loop
    anti_join(loop_pids_df, by=c("pid", "pid_loopid")) %>%
    filter(loopid %in% loop_pids_df$pid_loopid)

  loig_df = bind_rows(loig_df, after_loig_df)

  if (save) {
    file = file.path(drf$project_dir, "drf/loig_df.Rds")
    dir = dirname(file)
    if (!dir.exists(dir)) dir.create(dir)
    saveRDS(loig_df, file)
  }

  drf$loig_df = loig_df
  drf
}
