example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"
  project_dir = "~/repbox/projects/test"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  drf$sc_df = drf_stata_code_df(drf, path_merge = "load_natural")
}


drf_make_r_trans_parcel = function(drf) {
  restore.point("drf_make_r_trans_parcel")
  run_df = drf_run_df_create_rcode(drf=drf)
  rt_df = run_df %>%
    filter(rcode != "") %>%
    select(runid, rcode)

  drf$parcels[["r_trans"]] = rt_df
  repdb_save_parcels(drf$parcels["r_trans"],file.path(drf$project_dir, "repdb"),check=TRUE)
  drf

}

drf_get_dependency_load_code = function(r_id, drf) {
  restore.point("drf_get_dependency_load_code")
  if (is.null(drf$dep_df) || NROW(drf$dep_df) == 0) return(character(0))

  my_deps = drf$dep_df %>%
    dplyr::filter(runid == r_id, dep_type %in% c("e", "r"), !is.na(source_runid))

  if (NROW(my_deps) == 0) return(character(0))

  m_name = my_deps$macro_name
  s_runid = my_deps$source_runid

  prefix = stringi::stri_sub(m_name, 1, 1)
  inner = stringi::stri_replace_all_regex(m_name, "^[er]\\(|\\)$", "")

  is_esample = (m_name == "e(sample)")
  ext = ifelse(is_esample, ".dta", ".txt")

  # Forward slashes work cleanly in R across OS
  outfile = paste0(prefix, "_", s_runid, "_", inner, ext)
  var_name = paste0(prefix, "_", inner)

  # Vectorized creation of the load code
  load_code = paste0("stata2r::set_stata2r_env(", var_name,' = repboxDRF::drf_load_e_r_dependency(project_dir,', '"', outfile, '","', m_name, '"))')

  return(load_code)
}

drf_load_e_r_dependency = function(project_dir, file, macro_name) {
  restore.point("drf_load_e_r_dependency")
  file_type = tools::file_ext(file)
  path = file.path(project_dir,"drf/stata_e_r", file)

  if (!file.exists(path)) {
    repboxUtils::repbox_problem(paste0('Missing dependency file: ', file), type='missing_dep', project_dir=project_dir, fail_action='warn')
    return(NULL)
  }

  if (file_type == "dta") {
    res = haven::read_dta(path)
    if ("__esample" %in% names(res)) {
      res = res[["__esample"]]
    }
  } else {
    res = as.numeric(readLines(path, warn=FALSE)[1])
  }

  res
}

drf_run_df_create_rcode = function(run_df=drf$run_df, runids=drf_runids(drf), scalar_code = drf$scalar_code, drf=NULL) {
  restore.point("drf_run_df_create_rcode")

  if (!has_col(run_df, "rcode")) {
    run_df$rcode = rep("", NROW(run_df))
  }
  if (!is.null(runids)) {
    rows = match(runids, run_df$runid)
  } else {
    rows = seq_len(NROW(run_df))
  }
  rows = sort(unique(rows[!is.na(rows)]))

  update_rows = rows

  if (length(update_rows)==0) return(run_df)

  stata_code = run_df$cmdline[update_rows]

  stata_code = gsub("\n", " ", stata_code, fixed = TRUE)

  r_df = stata2r::do_to_r(stata_code, return_df = TRUE)

  translated_code = r_df$r_code
  run_df$rcode[update_rows] = ifelse(is.na(translated_code), "", translated_code)


  # Overwrite 'load' commands with repbox's own data loading logic
  inds = update_rows[run_df$cmd_type[update_rows] %in% c("load")]

  # Also overwrite the VERY FIRST execution row if we truncated the path at a file cache
  if (!is.null(runids) && length(runids) > 0) {
    first_runid = min(runids)
    first_row = match(first_runid, run_df$runid)
    if (!is.na(first_row) && isTRUE(run_df$has_file_cache[first_row])) {
      inds = unique(c(inds, first_row))
    }
  }

  if (length(inds)>0) {
    for (idx in inds) {
      if (isTRUE(run_df$has_file_cache[idx]) && idx == match(min(runids), run_df$runid)) {
        drf_rel_path = paste0("cached_dta/", basename(run_df$drf_cache_file[idx]))
      } else {
        drf_rel_path = ifelse(run_df$is_intermediate[idx],
                              paste0("im_data/", sub("^.*?im_data/", "", run_df$org_data_path[idx])),
                              paste0("org_data/", run_df$found_path[idx]))
      }

      code = paste0(
        'data = drf_load_data(project_dir, "', drf_rel_path ,'")\n',
        'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
        'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
      )
      run_df$rcode[idx] = code
    }
  }

  run_df$rcode = na.val(run_df$rcode, "")

  # Load locally saved Stata dependency values into the R environment
  if (!is.null(drf) && !is.null(drf$dep_df) && NROW(drf$dep_df) > 0) {
    for (idx in update_rows) {
      r_id = run_df$runid[idx]
      load_code = drf_get_dependency_load_code(r_id, drf)
      if (length(load_code) > 0) {
        run_df$rcode[idx] = paste0(paste(load_code, collapse="\n"), "\n", run_df$rcode[idx])
      }
    }
  }

  if (NROW(scalar_code)>0) {
    run_df = run_df %>%
      left_join(scalar_code %>% select(runid, scalar_r_code), by="runid") %>%
      mutate(scalar_r_code = na.val(scalar_r_code, "")) %>%
      mutate(rcode = ifelse(rcode=="", rcode, paste0(scalar_r_code, rcode))) %>%
      select(-scalar_r_code)
  }

  run_df
}


#
# drf_rcode_df = function(drf,runids=NULL, path_merge = c("none", "load", "natural", "load_natural")[4], update_rcode = FALSE) {
#   restore.point("drf_rcode_df")
#
#   # perform path merge like as for stata code
#   sc_df = drf_stata_code_df(drf, runids=runids, path_merge=path_merge)
#   runids = unique(rc_df$runid)
#
#   run_df = drf$run_df
#   if (update_rcode) {
#     run_df = drf_run_df_create_rcode(run_df, runids=runids)
#   }
#
#   run_df = drf$run_df %>%
#     filter(runid %in% runids)
#
#   rc_df = rc_df %>%
#     left_join(run_df %>% select(runid, cmdline,rcode), by="runid") %>%
#     mutate(code = rcode, pre = "", post="")
#   rc_df
# }
#
#
#
