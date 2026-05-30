
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



drf_stata_ensure_use_clear = function(cmdline) {
  restore.point("drf_stata_ensure_use_clear")

  if (length(cmdline) == 0) {
    return(cmdline)
  }

  cmd = stringi::stri_trim_both(cmdline)

  is_use_or_import = stringi::stri_detect_regex(
    cmd,
    "^(u|us|use|import|insheet|infix|infile)\\b",
    case_insensitive = TRUE
  )

  if (!any(is_use_or_import)) {
    return(cmdline)
  }

  has_clear = stringi::stri_detect_regex(
    cmd,
    "(^|[,[:space:]])clear([,[:space:]]|$)",
    case_insensitive = TRUE
  )

  add = is_use_or_import & !has_clear
  if (!any(add)) {
    return(cmdline)
  }

  has_comma = stringi::stri_detect_fixed(cmd[add], ",")

  cmd[add] = ifelse(
    has_comma,
    paste0(cmd[add], " clear"),
    paste0(cmd[add], ", clear")
  )

  cmdline[add] = cmd[add]
  cmdline
}


#' Replace file paths in cleaned Stata command lines
#'
#' @param cmdline Character vector of cleaned Stata commands one per line
#' @param replacement String to insert in place of the file path
#' @return Character vector of commands with paths replaced
replace_stata_cmdline_path = function(cmdline, replacement = '"`r(my_custom_path)\'"', add_clear=TRUE) {
  restore.point("replace_stata_cmdline_paths")

  if (length(replacement)>1 & length(replacement)!= length(cmdline)) {
    stop("cmdline and replacement must have same length.")
  }

  # Ensure replacement is correctly vectorized
  replacement = rep(replacement, length.out = length(cmdline))

  tab = repboxStata::repbox.re.cmdlines.to.tab(cmdline)

  empty_ph = data.frame(ph = character(0), content = character(0))
  res_paths = repboxStata::replace.files.and.paths.with.ph(tab, empty_ph)

  if (nrow(res_paths$ph) == 0) {
    final_cmds = cmdline
  } else {
    fph = res_paths$ph
    fph$content = replacement
    final_cmds = replace.ph.keep.lines(res_paths$txt, fph)
  }

  # Fallback for commands where repboxStata static parsing didn't find the path
  failed_to_replace = (final_cmds == cmdline)
  if (any(failed_to_replace)) {
    for (i in which(failed_to_replace)) {
      cmd = final_cmds[i]
      # Escape potential '$' symbols in file paths for regex engine
      safe_repl = gsub("$", "\\$", replacement[i], fixed = TRUE)

      # 1. Try to replace first quoted string, as import/use paths are typically quoted
      if (stringi::stri_detect_regex(cmd, '"[^"]+"')) {
        final_cmds[i] = stringi::stri_replace_first_regex(cmd, '"[^"]+"', safe_repl)
      }
      # 2. Look for 'using <path>' unquoted
      else if (stringi::stri_detect_regex(cmd, "\\busing\\s+([^\\s,]+)", case_insensitive=TRUE)) {
        final_cmds[i] = stringi::stri_replace_first_regex(cmd, "(?i)\\b(using\\s+)([^\\s,]+)", paste0("$1", safe_repl))
      }
      # 3. Look for bare command followed by unquoted path e.g. `import excel filename.xlsx, clear`
      else if (stringi::stri_detect_regex(cmd, "^\\s*(import|use|insheet|infix|infile)\\b", case_insensitive=TRUE)) {
        final_cmds[i] = stringi::stri_replace_first_regex(cmd, "(?i)^(\\s*(?:import|use|insheet|infix|infile)\\b(?:\\s+(?:excel|delimited|sas|spss))?\\s+)([^\\s,]+)", paste0("$1", safe_repl))
      }
    }
  }

  add_clear = rep(add_clear, length.out = length(final_cmds))

  if (any(add_clear)) {
    final_cmds[add_clear] = drf_stata_ensure_use_clear(final_cmds[add_clear])
  }

  final_cmds
}



#' Replace file paths in cleaned Stata command lines
#'
#' @param cmdline Character vector of cleaned Stata commands one per line
#' @param replacement String to insert in place of the file path
#' @return Character vector of commands with paths replaced
replace_stata_cmdline_path_old = function(cmdline, replacement = '"`r(my_custom_path)\'"', add_clear=TRUE) {
  restore.point("replace_stata_cmdline_paths")

  if (length(replacement)>1 & length(replacement)!= length(cmdline)) {
    stop("cmdline and replacement must have same length.")
  }

  tab = repboxStata::repbox.re.cmdlines.to.tab(cmdline)

  empty_ph = data.frame(ph = character(0), content = character(0))
  res_paths = repboxStata::replace.files.and.paths.with.ph(tab, empty_ph)

  if (nrow(res_paths$ph) == 0) {
    final_cmds = cmdline
  } else {
    fph = res_paths$ph
    fph$content = replacement
    final_cmds = replace.ph.keep.lines(res_paths$txt, fph)
  }

  add_clear = rep(add_clear, length.out = length(final_cmds))

  if (any(add_clear)) {
    final_cmds[add_clear] = drf_stata_ensure_use_clear(final_cmds[add_clear])
  }

  final_cmds
}



