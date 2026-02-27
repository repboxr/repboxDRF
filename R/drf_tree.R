# get tree structure from path_df that mpidks overlapping steps

drf_path_df_to_split_df = function(path_df) {
  split_df  = path_df %>%
    group_by(runid) %>%
    mutate(
      earliest_pid = min(pid)
    ) %>%
    group_by(pid, earliest_pid) %>%
    slice(n())
}
