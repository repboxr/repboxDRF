# scalar dependencies between Stata commans
example = function() {
  library(repboxDRF)
  project_dir = "~/repbox/projects_test/test"
  drf = drf_load(project_dir)
  drf = drf_add_scalar_map(drf)
  drf = drf_add_dep_df(drf)
}

drf_add_scalar_map = function(drf) {
  restore.point("drf_add_scalar_map")
  project_dir = drf$project_dir
  drf$parcels = repboxDB::repdb_load_parcels(project_dir, "stata_scalar", drf$parcels)
  scalar_df = drf$parcels$stata_scalar
  drf$scalar_map = NULL
  if (NROW(scalar_df)==0) {
    return(drf)
  }
  run_df = drf$run_df
  scalar_vars = unique(scalar_df$scalar_var)
  scalar_rx = paste0("\\b(", paste0(scalar_vars, collapse="|"), ")")

  # rows in run_df that use at least one of the scalars
  rows = which(stringi::stri_detect_regex(run_df$cmdline,scalar_rx) & run_df$cmd != "scalar")
  if (NROW(scalar_df)==0) {
    return(drf)
  }

  runids = run_df$runid[rows]
  ma_li = stringi::stri_extract_all_regex(run_df$cmdline[rows], scalar_rx, simplify = FALSE)

  scalar_map = bind_rows(lapply(seq_along(runids), function(i) {
    as.data.frame(list(runid=runids[i], scalar_var = unique(ma_li[[i]])))
  }))

  drf$scalar_map = scalar_map %>%
    left_join(
      scalar_df %>% select(scalar_var, source_runid=runid,scalar_val),
      by = join_by(
        scalar_var,
        closest(runid >= source_runid)
      ),
      relationship = "many-to-one"
    )

  drf_dir = file.path(drf$project_dir,"drf")
  if (!dir.exists(drf_dir)) dir.create(drf_dir)
  outfile = file.path(drf_dir,"scalar_map.Rds")
  saveRDS(drf$scalar_map, outfile)
  drf = drf_scalar_map_to_scalar_code(drf)

  drf
}

drf_scalar_map_to_scalar_code = function(drf) {
  scalar_map = drf$scalar_map
  if (is.null(scalar_map)) {
    drf$scalar_code = tibble(runid=integer(0), scalar_stata_code = character(0), scalar_r_code=character(0))
    return(drf)
  }
  drf$scalar_code = scalar_map %>%
    group_by(runid) %>%
    summarize(
      scalar_stata_code = paste0("scalar ", scalar_var, " = ", scalar_val,"\n", collapse=""),
      scalar_r_code = paste0(scalar_var, " = ", scalar_val,"\n", collapse="")
    )
  drf

}


#' Compute r(), e() or xi dependencies between run_df commands
drf_add_dep_df = function(drf) {
  restore.point("drf_make_deps_df")
  run_df = drf$run_df

  cmdlines = run_df$cmdline

  run_df$code = run_df$cmdline

  rx_or = function(els) {
    els = stringi::stri_replace_all_fixed(els, "(","\\(")
    els = stringi::stri_replace_all_fixed(els, ")","\\)")
    paste0("\\b(", paste0(els, collapse="|"), ")")
  }

  make_r_cmds = stata_make_r_cmds()

  run_df = run_df %>% mutate(
    uses_e =  stringi::stri_detect_regex(code, rx_or(c("e(sample)", "e(N)", "e(r2)", "e(df_r)", "e(rmse)", "e(b)", "e(V)"))),
    uses_r =  stringi::stri_detect_regex(code, "\\br\\("),
    uses_xi = stringi::stri_detect_fixed(code, "\\b_I[a-zA-Z0-9_]+"),
    makes_e = run_df$cmd_type %in% c("reg","quasi_reg"),
    makes_r = cmd %in% stata_make_r_cmds()
  )

  run_df$makes_xi = run_df$cmd == "xi"
  rows = which(run_df$cmd_type %in%  c("reg","quasi_reg"))
  mxi = stringi::stri_detect_regex(run_df$code[rows], "\\bxi\\:")
  run_df$makes_xi[rows[mxi]] = TRUE


  # e() dependencies (via regression commands)
  make_df = run_df %>%
    filter(cmd_type %in% c("reg","quasi_reg")) %>%
    select(source_runid=runid)

  edep_df = run_df %>%
    filter(stringi::stri_detect_regex(cmdline, rx_or(c("e(sample)", "e(N)", "e(r2)", "e(df_r)", "e(rmse)", "e(b)", "e(V)")))) %>%
    select(runid) %>%
    mutate(dep_type = "e") %>%
    left_join(make_df,by = join_by(closest(runid > source_runid)),relationship = "many-to-one")

  # r() dependencies (via commands like summarize)
  make_df = run_df %>%
    filter(cmd %in% stata_make_r_cmds()) %>%
    select(source_runid=runid)

  rdep_df = run_df %>%
    filter(stringi::stri_detect_regex(cmdline, "\\br\\(")) %>%
    select(runid) %>%
    mutate(dep_type = "r") %>%
    left_join(make_df,by = join_by(closest(runid > source_runid)),relationship = "many-to-one")

  # xi dependencies (via xi or xi:)
  makes_xi = run_df$cmd == "xi"
  rows = which(run_df$cmd_type %in%  c("reg","quasi_reg"))
  mxi = stringi::stri_detect_regex(run_df$cmdline[rows], "\\bxi\\:")
  makes_xi[rows[mxi]] = TRUE

  make_df = data.frame(source_runid = run_df$runid[makes_xi])

  xidep_df = run_df %>%
    filter(stringi::stri_detect_regex(cmdline, "\\b_I[a-zA-Z0-9_]+")) %>%
    select(runid) %>%
    mutate(dep_type = "xi") %>%
    left_join(make_df,by = join_by(closest(runid > source_runid)),relationship = "many-to-one")

  drf$dep_df = bind_rows(edep_df, rdep_df, xidep_df)
  outfile = file.path(drf$project_dir,"drf/dep_df.Rds")

  save_rds_create_dir(drf$dep_df, outfile)

  drf
}



stata_make_r_cmds = function() {

  abbr = function(long, short) {
    stringi::stri_sub(long, 1, nchar(short):nchar(long))
  }

  cmds = c(
    abbr("summarize", "su"),
    abbr("centile", "cen"),
    abbr("count", "cou"),
    "correlate",
    "corr",
    "pwcorr",
    "tabstat",
    abbr("describe", "desc"),
    "ds",
    abbr("codebook", "codeb"),
    abbr("inspect", "ins"),
    abbr("levelsof", "lev"),
    abbr("duplicates", "dup"),
    abbr("oneway", "onew"),
    abbr("ttest", "tt"),
    abbr("prtest", "prt"),
    abbr("sdtest", "sdt"),
    abbr("ranksum", "ra"),
    abbr("signrank", "signr"),
    abbr("signtest", "signt"),
    abbr("kwallis", "kwa"),
    "tab1",
    "tab2",
    "tabi",
    "pctile"
  )

  sort(unique(cmds))
}
#
# # list of stata commands that generate particular r() values, like r(mean), r(median) etc
# stata_r_cmds = function() {
#   add_cmd = function(long, short, ...) {
#     if (long != short) {
#       cmds = stringi::stri_sub(long, 1, nchar(short):nchar(long))
#     } else {
#       cmds = long
#     }
#     list(cmd = list(cmds), r_var = list(c(...)))
#   }
#
#   li = list(
#     add_cmd(
#       "summarize", "su",
#       "N", "sum_w", "mean", "Var", "sd", "min", "max", "sum",
#       "p1", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99",
#       "skewness", "kurtosis"
#     ),
#
#     add_cmd(
#       "centile", "cen",
#       "N", "n_cent", "c_#", "lb_#", "ub_#", "centiles"
#     ),
#
#     add_cmd(
#       "count", "cou",
#       "N"
#     ),
#
#     add_cmd(
#       "correlate", "corr",
#       "N", "rho", "cov_12", "Var_1", "Var_2", "sum_w", "C"
#     ),
#
#     add_cmd(
#       "pwcorr", "pwcorr",
#       "N", "rho", "C"
#     ),
#
#     add_cmd(
#       "tabstat", "tabstat",
#       "StatTotal", "Stat#", "name#"
#     ),
#
#     add_cmd(
#       "describe", "desc",
#       "N", "k", "width", "changed", "datalabel", "varlist", "sortlist"
#     ),
#
#     add_cmd(
#       "ds", "ds",
#       "varlist"
#     ),
#
#     add_cmd(
#       "codebook", "codeb",
#       "cons", "labelnotfound", "notlabeled",
#       "str_type", "str_leading", "str_trailing", "str_embedded", "str_embedded0"
#     ),
#
#     add_cmd(
#       "inspect", "ins",
#       "N", "N_neg", "N_0", "N_pos", "N_negint", "N_posint", "N_unique", "N_undoc"
#     ),
#
#     add_cmd(
#       "levelsof", "lev",
#       "N", "r", "levels"
#     ),
#
#     add_cmd(
#       "duplicates", "dup",
#       "N", "unique_value", "N_drop"
#     ),
#
#     add_cmd(
#       "oneway", "onew",
#       "N", "df_m", "F", "df_r", "mss", "rss", "chi2bart", "df_bart", "ANOVA"
#     ),
#
#     add_cmd(
#       "ttest", "tt",
#       "N_1", "N_2",
#       "mu_1", "mu_2", "mu_diff", "mu_combined",
#       "sd", "sd_1", "sd_2", "sd_diff",
#       "se", "se_1", "se_2", "se_combined",
#       "lb_1", "ub_1", "lb_2", "ub_2", "lb_diff", "ub_diff", "lb_combined", "ub_combined",
#       "t", "df_t", "p_l", "p", "p_u", "level"
#     ),
#
#     add_cmd(
#       "prtest", "prt",
#       "N", "N1", "N2",
#       "P", "P1", "P2", "P_diff",
#       "se", "se1", "se2", "se_diff", "se_diff0",
#       "lb", "ub", "lb1", "ub1", "lb2", "ub2", "lb_diff", "ub_diff",
#       "z", "p_l", "p", "p_u", "level",
#       "K", "M", "rho", "CV_cluster"
#     ),
#
#     add_cmd(
#       "sdtest", "sdt",
#       "N", "p_l", "p_u", "p", "F",
#       "sd", "sd_1", "sd_2",
#       "df", "df_1", "df_2", "chi2"
#     ),
#
#     add_cmd(
#       "ranksum", "ra",
#       "N", "N_1", "N_2", "z", "Var_a",
#       "group1", "sum_obs", "sum_exp",
#       "p", "p_l", "p_u", "p_exact"
#     ),
#
#     add_cmd(
#       "signrank", "signr",
#       "N", "N_pos", "N_neg", "N_tie", "z", "Var_a",
#       "sum_pos", "sum_neg",
#       "p", "p_l", "p_u", "p_exact", "p_l_exact", "p_u_exact"
#     ),
#
#     add_cmd(
#       "signtest", "signt",
#       "N", "N_pos", "N_neg", "N_tie", "p", "p_l", "p_u"
#     ),
#
#     add_cmd(
#       "kwallis", "kwa",
#       "df", "chi2", "chi2_adj"
#     ),
#
#     add_cmd(
#       "tab1", "tab1",
#       "N", "r", "collection"
#     ),
#
#     add_cmd(
#       "tab2", "tab2",
#       "N", "r", "c", "chi2", "p", "p_exact", "p1_exact",
#       "chi2_lr", "p_lr", "CramersV", "gamma", "ase_gam", "taub", "ase_taub",
#       "collection"
#     ),
#
#     add_cmd(
#       "tabi", "tabi",
#       "N", "r", "c", "chi2", "p", "p_exact", "p1_exact",
#       "chi2_lr", "p_lr", "CramersV", "gamma", "ase_gam", "taub", "ase_taub",
#       "collection"
#     )
#   )
#
#   library(dplyr)
#
#   stata_r_cmds_df = dplyr::bind_rows(li) %>%
#     tidyr::unnest(cmd) %>%
#     tidyr::unnest(r_var) %>%
#     dplyr::distinct()
#
#   stata_r_cmds_df
# }
