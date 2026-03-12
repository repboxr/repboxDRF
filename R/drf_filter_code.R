# Code to filter the data set according to an if or in condition
# in a regression command

# At some point we may want to generalize it to non regression commands

drf_get_filter_code = function(runid, drf, parcels=drf$parcels) {
  restore.point("drf_get_filter_code")

  # Load cmdpart parcel to get the stored if/in strings for this runid
  parcels = repboxDB::repdb_load_parcels(drf$project_dir, "reg_cmdpart", parcels=drf$parcels)
  cmdpart = parcels$reg_cmdpart

  cp = cmdpart[cmdpart$runid == runid, ]
  if (NROW(cp) == 0) {
    cat("\nNot any existing cmdpart stored for runid=", runid, ". Try to parse again to get if and in conditions")
    library(repboxStataReg)
    cp = cmdparts_of_stata_reg(cmdline)
  }
  if_str = cp$content[cp$part == "if_str"]
  in_str = cp$content[cp$part == "in_str"]

  filter_code = character(0)

  # 1. Translate `if_str` via stata2r dummy keep command
  if (length(if_str) > 0 && nzchar(if_str[1])) {
    fake_cmd = paste0("keep ", if_str[1])

    # stata2r natively translates `keep if...` to `dat <- dat %>% filter(...)`
    r_df = try(stata2r::do_to_r(fake_cmd)$r_df, silent = TRUE)

    if (!inherits(r_df, "try-error") && !is.null(r_df) && NROW(r_df) > 0) {
      filter_code = c(filter_code, r_df$r_code[1])
    } else {
      repbox_problem(
        msg = paste0("Failed to translate if condition for runid ", runid),
        type = "filter_translation_error",
        project_dir = drf$project_dir,
        fail_action = "warn"
      )
    }
  }

  # 2. Translate `in_str` natively (since it's just basic row subsetting)
  if (length(in_str) > 0 && nzchar(in_str[1])) {
    in_val = trimws(sub("^in\\s+", "", in_str[1]))
    if (grepl("/", in_val)) {
      parts = strsplit(in_val, "/")[[1]]
      start = trimws(parts[1])
      end = trimws(parts[2])
      if (tolower(end) == "l") end = "NROW(dat)" # Stata's 'L' means last row
      in_code = paste0("dat <- dat[", start, ":", end, ", , drop=FALSE]")
    } else {
      in_code = paste0("dat <- dat[", in_val, ", , drop=FALSE]")
    }
    filter_code = c(filter_code, in_code)
  }

  return(filter_code)
}

extract_stata_if_in_str = function(cmdline) {

  if_str = cp$content[cp$part == "if_str"]
  in_str = cp$content[cp$part == "in_str"]


}
