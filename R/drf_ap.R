# ap_df is an augmented path_df with more columns
# we may compute it during inference instead of precomputation

drf_path_df_to_ap_df = function(path_df, run_df) {
  restore.point("drf_path_to_drf_df")
  ap_df = path_df %>%
    left_join(run_df, by=c("runid"))

}

drf_mpidk_all_ap_df_mod = function(ap_df) {
  restore.point("drf_mpidk_all_ap_df_mod ")
  ap_df$is_mod = ap_df$cmd_type=="mod"

  # Currently: if the regression has an _I term but
  # uses not itself a xi command we need to keep
  # previous xi commands on the path
  I_rows = which(ap_df$is_I_reg & !ap_df$is_xi)
  if (length(i_rows)>0)  {
    ap_df %>%
      group_by(pid) %>%
      mutate(
        is_mod = is_mod | (last(is_I_reg & ! is_xi) & is_xi)
      )
  }
  ap_df
}
