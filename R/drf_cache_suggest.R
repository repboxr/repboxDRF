# We may want to add caches to speed up reproductions
# Criteria are:
# - no of data preparation steps avoided
# - no of regressions affected
# - Not yet considered: approximate size of cached data set

example = function() {
  library(repboxDRF)
  project_dir = "~/repbox/projects_test/test"
  drf = drf_load(project_dir)
  drf_suggest_cache_runids(drf, max_caches = 3, min_score = 5)
}

drf_suggest_cache_runids = function(drf,max_caches = Inf,min_score=100, prepare_drf=TRUE, verbose=TRUE, extra_caches = NULL) {
  restore.point("drf_suggest_cache_runids")

  if (prepare_drf) {
    drf = repboxDRF::drf_apply_caches(drf)
    drf = repboxDRF::drf_apply_loop_ignore(drf)
  }
  path_df = drf$path_df

  if (!is.null(extra_caches)) {
    path_df = drf_cut_path_df_for_cache(path_df, extra_caches)
  }

  num_cache = 0
  cached_runids = NULL
  while(TRUE) {
    res = drf_suggest_best_cache_runid(path_df)
    if (NROW(res)==0) break
    if (res$score < min_score) break

    runid = res$runid
    cached_runids = c(cached_runids,runid)
    path_df = drf_cut_path_df_for_cache(path_df, runid)
    num_cache = num_cache +1
    if (verbose) {
      cat(paste0(if (num_cache<=1) "\n", "Cache proposed at runid=",runid," (score=", round(res$score), " regs=",res$num_pids, " skipped cmds=", res$num_before ,")\n"))
    }
    if (length(num_cache)>= max_caches) break
  }
  cached_runids
}

drf_suggest_best_cache_runid = function(path_df,  num_pid_exp=0.5) {
  restore.point("drf_suggest_best_cache_runid")
  project_dir = drf$project_dir

  pids = unique(path_df$pid)
  df = path_df %>%
    # we don't want to cache at a regression
    # since if there is a regression filter
    # the filtered data set is saved
    filter(!runid %in% pids) %>%
    group_by(pid) %>%
    mutate(num_before = (1:n())-1) %>%
    group_by(runid) %>%
    summarize(
      pid_group = paste0(sort(unique(pid)), collapse="|"),
      num_pids = n(),
      num_before = max(num_before)
    )

  # get last runid for each pid_group
  df = df %>%
    group_by(pid_group) %>%
    top_n(1, runid) %>%
    ungroup() %>%
    mutate(score = num_pids^num_pid_exp * num_before)

  if (NROW(df)==0) return(df)

  best_ind = which.max(df$score)
  df[best_ind,]

}


# remove in all paths (a path is identified by path_df$pid)
# where cache_runid is on the path
# all runid < cache_runid
drf_cut_path_df_for_cache = function(path_df, cache_runids) {
  restore.point("drf_cut_path_df_for_cache")

  if (length(cache_runids) == 0) {
    return(path_df)
  }

  cache_pos = path_df %>%
    filter(runid %in% cache_runids) %>%
    group_by(pid) %>%
    summarize(cache_runid = max(runid), .groups = "drop")

  if (NROW(cache_pos) == 0) {
    return(path_df)
  }

  path_df %>%
    anti_join(
      cache_pos,
      by = join_by(pid, runid < cache_runid)
    )
}
