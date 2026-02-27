# Helper function to make vector function

helper_make_drf_stata_cmd_types_vec = function() {
  cmd_types = drf_stata_cmd_types()
  types = names(cmd_types)
  res = unlist(lapply(types, function(type) {
    if (type=="need_cache") return(NULL)
    cmds = cmd_types[[type]]
    if (type=="possible_mod") type="mod"
    res = rep(type, length(cmds))
    names(res) = cmds
    res
  }))
  cat(paste0(names(res),'="', res,'"', collapse=", "))


}


drf_stata_cmd_types = function() {
  #
  # IMPORTANT: IF YOU ADD A COMMAND HERE
  # ALSO ADD IT IN stata.data.cmd.types.vec
  # Otherwise strange errors occur
  #
  list(
    load = c("u","us", "use","insheet","infix","import","sysuse","guse","gzuse"),
    preserve = c("preserve"),
    restore = c("restore"),
    # Commands that modify data and can be translated to R
    mod = c("g","ge","gen","generate","replace","drop","keep","rename","merge","egen","xtset","tsset", "xtile","pctile","append","tabulate","tabul","tabu", "tab","ta","encode","predict","xi", "collapse", "sort","so","tab1", "winsor"),
    scalar = c("scalar"),
    # Commands that modify data but cannot be translated to R
    # predict also needs cache because it may use
    # results from previous regression
    need_cache = c("merge","xtile","pctile","append","tabulate","tabul","tabu", "tab","ta","encode","predict","xi","collapse","sort","so","tab1", "winsor"),

    # Commands that under certain conditions modify the data
    # tabulate creates vpidiables if called with the gen option
    possible_mod = c("tabulate","tabul","tabu", "tab","ta","tab1"),
    reg = repboxStata::stata_cmds_reg(),
    quasi_reg = repboxStata::stata_cmds_quasireg(),
    post_reg = repboxStata::stata_cmds_postreg()

  )
}

drf_stata_cmd_types_vec = function() {
  return(c(
u="load", us="load", use="load", insheet="load", infix="load", import="load", sysuse="load", guse="load", gzuse="load", preserve="preserve", restore="restore", g="mod", ge="mod", gen="mod", generate="mod", replace="mod", drop="mod", keep="mod", rename="mod", merge="mod", egen="mod", xtset="mod", tsset="mod", xtile="mod", pctile="mod", append="mod", tabulate="mod", tabul="mod", tabu="mod", tab="mod", ta="mod", encode="mod", predict="mod", xi="mod", collapse="mod", sort="mod", so="mod", tab1="mod", winsor="mod", tabulate="mod", tabul="mod", tabu="mod", tab="mod", ta="mod", tab1="mod", reg="reg", areg="reg", ivregress="reg", ivreg="reg", ivreg2="reg", sureg="reg", reghdfe="reg", reg2hdfe="reg", xtreg="reg", xtivreg2="reg", xtlogit="reg", xtprobit="reg", xttobit="reg", regress="reg", cgmreg="reg", intreg="reg", boxcox="reg", qreg="reg", truncreg="reg", cnsreg="reg", eivreg="reg", nl="reg", rreg="reg", bsqreg="reg", sqreg="reg", iqreg="reg", vwls="reg", sem="reg", gsem="reg", glm="reg", cloglog="reg", logit="reg", logistic="reg", blogit="reg", glogit="reg", binreg="reg", scobit="reg", probit="reg", dprobit="reg", ivprobit="reg", bprobit="reg", gprobit="reg", hetprobit="reg", heckprobit="reg", biprobit="reg", tobit="reg", ivtobit="reg", clogit="reg", obprobit="reg", ologit="reg", heckoprobit="reg", rologit="reg", asroprobit="reg", slogit="reg", mlogit="reg", asclogit="reg", nlogit="reg", asmprobit="reg", mprobit="reg", poisson="reg", ivpoisson="reg", nbreg="reg", gnbreg="reg", tpoisson="reg", tnbreg="reg", zip="reg", zinb="reg", exlogistic="reg", expoisson="reg", arch="reg", frontier="reg", reg3="reg", heckman="reg", etregress="reg", etpoisson="reg", arima="reg", arfima="reg", newey="reg", var="reg", svar="reg", vec="reg", dfactor="reg", ppmlhdfe="reg", svyreg="reg", ppml="reg", rd="quasi_reg", rdrobust="quasi_reg", psmatch2="quasi_reg", leebounds="quasi_reg", a2reg="quasi_reg", xtabond2="quasi_reg", altrdrobust="quasi_reg", hausman="quasi_reg", stcox="quasi_reg", xtivreg="quasi_reg", ivreghdfe="quasi_reg", condivreg="quasi_reg", oprobit="quasi_reg", xtpoisson="quasi_reg", newey2="quasi_reg", hetprob="quasi_reg", reg2hdfespatial="quasi_reg", outreg2="post_reg", estadd="post_reg", estimates="post_reg", predict="post_reg", test="post_reg", matrix="post_reg", est="post_reg", outreg="post_reg", eststo="post_reg", testparm="post_reg", lincom="post_reg", esttab="post_reg", margins="post_reg", estout="post_reg", estat="post_reg", nlcom="post_reg", plotcoeffs="post_reg", mfx="post_reg", boottest="post_reg", regsave="post_reg", center_estimates="post_reg", estimate="post_reg", est2vec="post_reg", suest="post_reg", fitstat="post_reg", parmest="post_reg", post="post_reg", estpost="post_reg", savereg="post_reg", wild="post_reg", rivtest="post_reg", sigstar2="post_reg", modl="post_reg", svmat="post_reg", sig_p="post_reg", ttest="post_reg", lincomestadd="post_reg", coefplot="post_reg", eret2="post_reg", get_coef="post_reg", cgmwildboot="post_reg", vareffects="post_reg", bootwildct="post_reg", post_param="post_reg", avplot="post_reg", addtotable="post_reg", margin="post_reg", vce2way="post_reg", save_results="post_reg", ivstack="post_reg", predictnl="post_reg", spatdiag="post_reg", parmby="post_reg", testnl="post_reg", b_xt="post_reg", V_xt="post_reg", p_vals="post_reg", inteff="post_reg", est2tex="post_reg", meff="post_reg", marginsplot="post_reg", iv_stack="post_reg", dfuller="post_reg", ivhettest="post_reg", pValueFormatting="post_reg", estwrite="post_reg", dfbeta="post_reg", margeff="post_reg", modltbl="post_reg", outsheet="post_reg", outtex="post_reg", lincomest="post_reg", mfx2="post_reg", addstars="post_reg", scalar="scalar"
    ))
}



pide_possible_mod_cmd_real_mod = function(run.df) {
  restore.point("pide_possible_mod_cmd_real_mod_cmd")
  is.mod = rep(TRUE, NROW(run.df))

  tab.rows = which(run.df$cmd %in% c("tabulate","tabul","tabu", "tab","ta"))
  if (length(tab.rows)>0) {
    opts = str.right.of(run.df$cmdline[tab.rows],",")
    opts = trimws_pidound(opts,"\\(")
    rx = paste0("(", c("gen","gene","gener","generate"),"\\()", collapse="|")
    is.gen = grepl(rx, opts)
    is.mod[tab.rows] = is.gen
  }
  is.mod
}




