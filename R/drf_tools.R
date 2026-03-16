
drf_code_adapt = function(code_df, fun, ..., to_col="code", append_mode = c("overwrite", "left", "right")[1], vectorized=FALSE, just_cmd_types=NULL,just_runids=NULL, just_path_pos = c("start","end")[2], once_per_runid=TRUE){
  restore.point("drf_adapt_code")

  rows = NULL
  if (!is.null(just_runids)) {
    runids = just_runids
  } else if (!is.null(just_cmd_types)) {
    runids = unique(code_df$runid[code_df$cmd_type %in% cmd_types])
  } else if (just_path_pos=="end") {
    rows = which(code_df$pid==code_df$runid)
    runids = code_df$runid[rows]
    once_per_runid = TRUE
  } else if (just_path_pos=="start") {
    code_df = code_df %>%
      group_by(pid) %>%
      mutate(path_pos = 1:n()) %>%
      ungroup()
    rows = which(code_df$path_pos==1)
    runids = code_df$runid[rows]
  } else if (is.null(runid)) {
    stop("Please provide just_runids, just_cmd_types or just_path_pos")
  }

  if (is.null(rows))
    rows = match(runids, code_df$runid)

  if (vectorized) {
    new_code = do.call(fun, args=list(code_df = code_df[rows,], ...))
  } else {
    new_code = sapply(rows, function(row) {
      do.call(fun, args=list(code_df = code_df[row,], ...))
    })
  }

  restore.point("inbetweenlksjds")

  if (!once_per_runid) {
    ind = match(code_df$runid, runids)
    to_rows = which(!is.na(ind))
    new_code = new_code[ind[to_rows]]
  } else {
    to_rows = rows
  }
  if (append_mode=="overwrite") {
    code_df[[to_col]][to_rows] = new_code
  } else if (append_mode == "left") {
    code_df[[to_col]][to_rows] = paste0(new_code,code_df[[to_col]][to_rows])
  } else if (append_mode == "right") {
    code_df[[to_col]][to_rows] = paste0(code_df[[to_col]][to_rows], new_code)
  } else {
    stop(paste0("unknown append_mode = ", appen_mode))
  }
  code_df

}






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



