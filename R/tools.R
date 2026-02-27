sec_since_start = function(time_str, start_time_str) {
  restore.point("sec_since_start")
  if (length(start_time_str) != 1L) stop("start_time_str must be length 1.")

  ppidse_hms_sec = function(x) {
    # Fast fixed-position ppidse of "HH:MM:SS"
    h = as.integer(stringi::stri_sub(x, 1L, 2L))
    m = as.integer(stringi::stri_sub(x, 4L, 5L))
    s = as.integer(stringi::stri_sub(x, 7L, 8L))
    3600L * h + 60L * m + s
  }

  t_sec = ppidse_hms_sec(time_str)
  start_sec = ppidse_hms_sec(start_time_str)

  dt = t_sec - start_sec
  dt = dt + 86400L * (dt < 0L)
  dt
}
