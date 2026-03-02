# metareg base
# use drf format to rerun regressions
# was previously in repboxReg
# uses repboxDRF, metaregTools and aicoder

mr_base_code_reg_stata = function(runid, run_df=NULL, outdir=NULL, ...) {
  restore.point("mr_base_reg_stata_code")

  library(repboxReg)
  library(repboxStata)

  stata_code = run_df$cmdline[runid]

  extra_code = ""
  outfile = paste0(outdir, "/reg_", step, "__sb.dta")
  scalar_outfile = paste0(outdir, "/regscalar_", step, "__sb.txt")
  macro_outfile = paste0(outdir, "/regmacro_", step, "__sb.txt")

  cmd = mr$step.df$cmd[step]
  # Command for which marginal effects are stored
  if (cmd %in% stata_cmds_with_margin()) {
    extra_code = paste0('
capture noisily margins, atmeans dydx(*) post
capture noisily parmest, saving("',outdir, "/reg_", step, "__sb_mem.dta",'")
capture noisily repbox_write_reg_scalars "',outdir, "/regscalar_", step, "__sb_mem.txt",'"
capture noisily repbox_write_reg_macros "',outdir, "/regmacro_", step, "__sb_mem.txt",'"
')
  } else if (cmd == "dprobit") {
    extra_code = paste0('capture noisily repbox_write_dprobit_coef_se "',outdir, "/dprobit_", step, ".csv\n")
  } else if (cmd %in% stata_cmds_with_exp_coef()) {
    extra_code = paste0('capture noisily estout . using "', outdir,'/reg_', step, '__sb_exp.tsv", cells("b se t p ci_l ci_u") replace eform\n')
  }

  code = paste0(
    "capture noisily ", stata_code,
    '\n
capture noisily parmest, label saving("',outfile,'", replace)
capture noisily repbox_write_reg_scalars "', scalar_outfile,'"
capture noisily repbox_write_reg_macros "', macro_outfile,'"
', extra_code, if_store_code
  )
  code
}
