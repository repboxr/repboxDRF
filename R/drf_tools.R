


#' Replace file paths in cleaned Stata command lines
#'
#' @param cmdline Character vector of cleaned Stata commands (one per line)
#' @param replacement String to insert in place of the file path
#' @return Character vector of commands with paths replaced
replace_stata_cmdline_path = function(cmdline, replacement = '"`r(my_custom_path)\'"', add_clear=TRUE) {
  restore.point("replace_stata_cmdline_paths")

  if (length(replacement)>1 & length(replacement)!= length(cmdline)) {
    stop("cmdline and replacement must have same length.")
  }

  # 1. Parse already-cleaned lines directly into a 'tab' object
  tab = repboxStata::repbox.re.cmdlines.to.tab(cmdline)

  # Because repbox.re.cmdline.to.tab resolves quotes back into the 'tab'
  # dataframe directly, we just provide an empty placeholder dataframe.
  empty_ph = data.frame(ph = character(0), content = character(0))

  # 2. Extract paths into a new placeholder object
  res_paths = repboxStata::replace.files.and.paths.with.ph(tab, empty_ph)

  # 3. If no file paths were found, return the original commands
  if (nrow(res_paths$ph) == 0) {
    return(cmdline)
  }

  # 4. Modify the extracted paths
  fph = res_paths$ph
  fph$content = replacement

  # 5. Re-inject the new paths back into the text
  final_cmds = replace.ph.keep.lines(res_paths$txt, fph)

  add_clear = rep(add_clear, length.out=NROW(final_cmds))
  has_clear = "clear" %in% tolower(tab$opts)
  add_clear = add_clear & !has_clear

  add_str = case_when(
    add_clear & nzchar(tab$opts) ~ " clear",
    add_clear ~ ", clear",
    TRUE ~ ""
  )
  final_cmds = paste0(final_cmds, add_str)


  return(final_cmds)
}



