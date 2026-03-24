# Code to filter the data set according to an if or in condition
# in a regression command

# At some point we may want to generalize it to non regression commands

# FILE: repboxDRF/R/drf_filter_code.R

drf_get_filter_code = function(runid, drf, parcels=drf$parcels) {
  restore.point("drf_get_filter_code")

  parcels = repboxDB::repdb_load_parcels(drf$project_dir, "reg_cmdpart", parcels=drf$parcels)
  cmdpart = parcels$reg_cmdpart

  cp = cmdpart[cmdpart$runid == runid, ]
  if (NROW(cp) == 0) {
    cat("\nNot any existing cmdpart stored for runid=", runid, ". Cannot get if and in conditions.")
    return(character(0))
  }
  if_str = cp$content[cp$part == "if_str"]
  in_str = cp$content[cp$part == "in_str"]

  filter_code = character(0)

  if (length(if_str) > 0 && nzchar(if_str[1])) {
    # Get the context to accurately translate the if expression natively via stata2r
    path_df = drf$path_df
    pid = first(path_df$pid[path_df$runid == runid])
    if (is.na(pid)) pid = runid

    path_df_full = path_df[path_df$pid == pid & path_df$runid <= runid,]
    stata_code = drf$run_df$cmdline[match(path_df_full$runid, drf$run_df$runid)]

    # Protect newlines to avoid row index shifting
    stata_code = gsub("\n", " ", stata_code, fixed = TRUE)

    cmd_df = stata2r::do_parse(stata_code)
    cmd_df = stata2r::mark_data_manip_cmd(cmd_df)

    line_num = NROW(cmd_df)

    # STRIP 'if' KEYWORD
    # The 'if_str' from cmdpart includes the 'if' keyword, e.g. "if u_sample == 1".
    # Strip it before passing to translation.
    if_expr = sub("^\\s*if\\s+", "", if_str[1], ignore.case = TRUE)

    r_if_cond = stata2r::translate_stata_expression_with_r_values(
      if_expr, line_num, cmd_df, list(is_by_group = FALSE)
    )

    if (!is.na(r_if_cond)) {
      r_if_cond_quoted = encodeString(r_if_cond, quote = '"')
      filter_code = c(filter_code, paste0("data = stata2r::scmd_keep(data, r_if_cond = ", r_if_cond_quoted, ")"))
    } else {
      repboxUtils::repbox_problem(
        msg = paste0("Failed to translate if condition for runid ", runid),
        type = "filter_translation_error",
        project_dir = drf$project_dir,
        fail_action = "warn"
      )
    }
  }

  if (length(in_str) > 0 && nzchar(in_str[1])) {
    # STRIP 'in' KEYWORD
    # The 'in_str' includes 'in', e.g. "in 1/100". Strip it!
    in_expr = sub("^\\s*in\\s+", "", in_str[1], ignore.case = TRUE)

    r_in_range = stata2r::s2r_in_str_to_r_range_str(in_expr)
    if (!is.na(r_in_range)) {
      r_in_range_quoted = encodeString(r_in_range, quote = '"')
      in_code = paste0("data = stata2r::scmd_keep(data, r_in_range = ", r_in_range_quoted, ")")
      filter_code = c(filter_code, in_code)
    }
  }

  return(filter_code)
}
