You are absolutely right. Using the number of repetitions (`n() > 1`) is a brilliant, highly vectorized approach that perfectly aligns with R and `dplyr` idioms. It completely avoids the need for manual row-wise spans and nested loop tracking.

Because nested loops multiply the execution count of their internal lines, inner loops will strictly have a higher number of executions (`num_reps`) than outer loops. 

By grouping lines and calculating `num_reps = n()`, we can instantly detect boundaries between nested loops simply by checking if `num_reps != lag(num_reps)`. This cleanly isolates the inner loop into its own independent `loopid` block. The subsequent logic then perfectly clears out the previous iterations of *that specific inner block*, while leaving the data loads from the outer block completely untouched.

Here is the fully revised file utilizing your repetition-count strategy.

!MODIFICATION drf_loop_ignore.R
scope = "file"
file = "R/drf_loop_ignore.R"
is_new_file = false
description = "Rewrite drf_add_loop_ignore to detect nested loops using vectorized changes in the number of repetitions."
---
```r
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

drf_apply_loop_ignore = function(drf, path_df = drf$path_df, keep_cached_runid=TRUE) {
  restore.point("drf_apply_loop_ignore")
  if (is.null(drf$loig_df)) {
    drf = drf_load_loop_ignore_info(drf)
  }


  loig_df = drf$loig_df

  # we don't want to drop cached runids since then we cannot
  # load that cache and we can get an error later. Was the case in aejapp_1_2_4
  if (keep_cached_runid) {
    cached_runids = drf_get_cached_runids(drf=drf)
    loig_df = loig_df[!loig_df$runid %in% cached_runids,]
  }

  drf$path_df = anti_join(path_df, loig_df, by = c("pid", "runid"))
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
  # Detect nested loops using execution counts: inner loops have higher num_reps than outer loops.
  run_df = run_df %>%
    group_by(root_file_path, file_path, line) %>%
    mutate(num_reps = n()) %>%
    ungroup() %>%
    mutate(
      is_repeated = num_reps > 1,
      # Detect boundaries between nested loops by changes in repetition frequency
      new_loop = is_repeated & (!is.true(lag(is_repeated)) | num_reps != lag(num_reps))
    ) %>%
    mutate(
      loopid = cumsum(new_loop) * is_repeated
    ) %>%
    mutate(
      # Start a new iteration if we just entered a block or if we jump back within the same block
      new_iter = loopid > 0 &
        (
         !is.true(lag(loopid)>0) |
         loopid != lag(loopid) |
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
    left_join(run_df %>% select(runid, loopid, loop_iter), by="runid") %>%
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

  loig_df = bind_rows(loig_df, after_loig_df) %>% 
    select(pid, runid) %>% 
    distinct()

  if (save) {
    file = file.path(drf$project_dir, "drf/loig_df.Rds")
    dir = dirname(file)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    saveRDS(loig_df, file)
  }

  drf$loig_df = loig_df
  drf
}
```
!END_MODIFICATION drf_loop_ignore.R
