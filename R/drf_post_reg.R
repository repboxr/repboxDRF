#' Get the associated regression runid for post-regression commands
#'
#' Uses a fast heuristic: maps each post_reg command to the most recent
#' preceding reg or quasi_reg command in the execution sequence (run_df).
#' Note: This relies on execution sequence and does not yet trace 'estimates restore' calls.
drf_get_post_reg_reg_runid = function(run_df) {
  restore.point("drf_get_post_reg_reg_runid")

  reg_df = run_df %>%
    dplyr::filter(cmd_type %in% c("reg", "quasi_reg")) %>%
    dplyr::select(reg_runid = runid)

  post_df = run_df %>%
    dplyr::filter(cmd_type == "post_reg") %>%
    dplyr::select(runid)

  if (NROW(post_df) == 0) return(dplyr::tibble(runid = integer(0), reg_runid = integer(0)))
  if (NROW(reg_df) == 0) return(post_df %>% dplyr::mutate(reg_runid = NA_integer_))

  post_df = post_df %>%
    dplyr::left_join(reg_df, by = dplyr::join_by(dplyr::closest(runid > reg_runid)))

  post_df
}
