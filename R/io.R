
# To do: variant that is faster for selected paths
drf_load = function(project_dir, parcels=list()) {
  project_dir = normalizePath(project_dir)
  drf = list(project_dir = project_dir, drf_dir = file.path(project_dir, "drf"), parcels = parcels)
  drf$parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)
  drf$run_df = drf_make_run_df(drf=drf)
  drf$path_df = drf_load_path_df(drf=drf)
  drf
}

drf_make_path_index = function(path_df) {
  path_df$.ROW = 1:NROW(path_df)
  index_df = path_df %>%
    group_by(pid) %>%
    summarize(
      start_row = .ROW[1]
    ) %>%
    bind_rows(tibble(pid=Inf, start_row = NROW(path_df+1)))



}
drf_load_path_index = function(project_dir) {
  index_df = read_rds_or_null(file.path(project_dir, "drf/path_index.Rds"))
  index_df
}

drf_load_path = function(project_dir, pid, index_df = drf_load_path_index(pid))  {

  row = which(index_df$pid==row)
  from = index_df$start_row[row]
  to= index_df$start_row[row+1]-1
  file = file.path(project_dir, "drf/path_df.fst")

  fst::read.fst(file,from = from,to=to)

}

drf_load_path_df = function(project_dir=drf$project_dir, drf=NULL)  {
  file = file.path(project_dir, "drf/path_df.fst")
  if (!file.exists(file)) return(NULL)
  fst::read.fst(file)
}

drf_save_path_df = function(project_dir=drf$project_dir, path_df=drf$path_df, drf)  {
  restore.point("drf_save_path_df")
  index_df = drf_make_path_index(path_df)
  dir = file.path(project_dir, "drf")
  if (!dir.exists(dir)) dir.create(dir)
  saveRDS(index_df, file.path(dir, "path_index.Rds"))

  file = file.path(project_dir, "drf/path_df.fst")
  fst::write.fst(path_df, file)
  return(index_df)

}
