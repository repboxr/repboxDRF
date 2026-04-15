This is an excellent realization. Your suggestions drastically simplify and robustify the caching system. 

Here is the revised plan based on our discussion:

### 1. Removing the Fake `runid` Hack (Using `has_file_cache`)
Instead of appending pseudo-rows to `run_df`, we just keep the original `run_df` untouched but add two columns: `has_file_cache` and `drf_cache_file`.
*   **Path Truncation:** We scan the path (`path_df`). If we find a safe cache, we truncate the path so it *starts* exactly at the cached `runid` (e.g., the `merge` command).
*   **Code Generation:** When `drf_run_df_create_rcode` or `drf_stata_code_df` sees that the *very first command* of a requested path has a file cache, it overrides the normal translation (e.g., `merge ...`) and instead outputs a standard data load command (`use cache.dta, clear` or `drf_load_data()`).
This natively supports overlapping paths without breaking them because the replacement *only* happens if the cached command is the earliest step in the requested execution path.

### 2. Dependency Safety Check
We use a highly conservative but simple safety heuristic in R:
1.  **Macro Definitions:** Since a `.dta` cache inherently saves all generated variables (`gen`, `egen`), the only state lost by skipping commands is Stata memory: **scalars, locals, globals, and matrices**. If `skipped_df` contains any definitions for these, we reject the cache.
2.  **Return Usage:** If the `remaining_df` uses `r(...)` or `e(...)` or `matrix`, we reject the cache just in case they rely on a skipped estimation/summary command. 
If both checks pass, the cache is 100% safe to apply. (We don't need the heavy `stata2r` machinery if we apply this strict regex heuristic, keeping DRF lightweight).

### 3. File Pruning
As agreed, I have omitted the unused file pruning. Storage is cheap, and maintaining the original structure is safer.

Below are the exact file modifications to implement this clean, robust caching architecture.

***

!MODIFICATION repbox_always_cache_cmd in repboxStata/R/stata_cmd_lists.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/stata_cmd_lists.R"
insert_bottom = true
description = "Add function repbox_always_cache_cmd to specify auto-cached commands"
---
```r
repbox_always_cache_cmd = function() {
  c("merge")
}
```
!END_MODIFICATION repbox_always_cache_cmd in repboxStata/R/stata_cmd_lists.R

!MODIFICATION injection.cache_always in repboxStata/R/inject.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/inject.R"
insert_bottom = true
description = "Add function to inject data saving after specific cache commands"
---
```r
injection.cache_always = function(txt, lines, do) {
  restore.point("injection.cache_always")
  cache.dir = file.path(do$project_dir, "repbox", "stata", "cached_dta")
  
  cache.file = paste0(cache.dir, "/cache_", do$donum, "_", lines, "_`repbox_local_cmd_count'.dta")
  paste0('
', end.injection(do$donum, lines, "RUNCMD", do),'
* CACHE INJECTION START
', post.injection(txt, lines, do=do),'
capture noisily save "', cache.file, '", replace
* CACHE INJECTION END
')
}
```
!END_MODIFICATION injection.cache_always in repboxStata/R/inject.R

!MODIFICATION inject.do in repboxStata/R/inject.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/inject.R"
function_name = "inject.do"
description = "Inject cache saving logic after specified cache commands in the main injection loop"
---
```r
inject.do = function(do, reg.cmds = get.regcmds(), save.changed.data=1, opts=rbs.opts()) {
  restore.point("inject.do")

  project_dir=do$project_dir
  id = tools::file_path_sans_ext(basename(do$file))
  repbox.dir = file.path(project_dir,"repbox/stata")

  tab = do$tab[[1]]
  tab$org_cmd = ""


  do$tab[[1]]$add.path.correction = tab$cmd %in% c("use","u","us","saveold", "save","sa","sav", "import","export","mkdir","erase","rm","guse","gsave","gzuse","gzsave") |
    !is.na(tab$using) |
    !is.na(tab$saving) |
    (tab$cmd %in% c("graph","gr","gra") & tab$cmd2 %in% c("export","save")) |
    (tab$cmd %in% c("estimates","est","estim","estimate") & tab$cmd2 %in% c("save","use")) |
    (tab$cmd %in% "adopath" & tab$cmd2 %in% c("+")) |
    (tab$cmd %in% c("putexcel") & tab$cmd2 %in% c("set")) |
    (tab$cmd == "cd" & trimws(tab$txt)!="cd")

  ph = do$ph[[1]]
  tab = do$tab[[1]]

  tab$run.max = NA_integer_
  if (!is.null(opts$loop.log.cmd.max)) {
    rows = tab$in.program == 1 | tab$in_loop == 1
    tab$run.max[rows] = opts$loop.log.cmd.max
  }
  do$tab[[1]] = tab


  tab$commented.out = FALSE
  tab$add.capture=FALSE

  org.txt = txt = replace.ph.keep.lines(tab$txt,ph)

  new.txt = txt

  block.rows = tab$opens_block & (!(is.na(tab$quietly)) | !is.na(tab$capture))
  if (sum(block.rows) >0) {
    cmds = tab$cmd[block.rows]
    rows = !is.na(tab$capture[block.rows])
    cmds[rows] = trimws(tab$capture[block.rows][rows])

    rows = !is.na(tab$quietly[block.rows])
    cmds[rows] = trimws(tab$quietly[block.rows][rows])

    new.txt[block.rows] = stringi::stri_replace_first(new.txt[block.rows],fixed=cmds, replacement = paste0(cmds, " noisily"))
  }

  rows = startsWith(trimws(new.txt),"quietly:") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "quietly:") %>% trimws()

  rows = startsWith(trimws(new.txt),"quietly ") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "quietly ") %>% trimws()
  rows = startsWith(trimws(new.txt),"qui ") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "qui ") %>% trimws()

  rows = which(tab$cmd == "table")
  if (length(rows)>0) {
    is.pre.table = is.pre.Stata17.table.command(tab$txt[rows])
    if (is.pre.table) {
      new.txt[rows] = paste0("version 16: ", new.txt[rows])
    }
  }

  lines = which(tab$add.path.correction)
  new.txt[lines] = inject.path.correction.change.cmd(new.txt[lines], lines, do=do)

  lines = which(!(
    is.true(tab$opens_block) | tab$in.program >= 2 |
      tab$cmd %in% c("}","foreach","forvalues","forval", "if","else","end", "while")
  ))

  new.txt[lines] = paste0("capture:  noisily: ",  new.txt[lines])
  tab$add.capture[lines] = TRUE

  do$tab[[1]] = tab

  no.study.lines = which( (trimws(tab$cmd) %in% c("}","end","if","else")) | tab$in.program >= 2 | endsWith(trimws(tab$txt),"}"))

  no.study.lines = union(no.study.lines,
    which(
      (tab$opens_block & tab$in.program == 1) |
      (tab$opens_block & lag(tab$in_loop %in% c(1,2)))
    )
  )

  if (!opts$report.inside.program) {
    no.study.lines = union(no.study.lines, which(tab$in.program == 1))
  }

  special.lines = NULL

  if (do$does.include & do$use.includes) {
    incl.df = do$incl.df[[1]]
    incl.df = adapt.incl.df.for.stata.vars(incl.df,do$project_dir)

    incl.do.df = filter(incl.df, cmd=="do" | cmd == "run")

    lines = incl.do.df$line

    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      incl.do.df$find.file.code,
      '\ncapture: noisily: do "',incl.do.df$repbox.file,'", nostop',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )

    incl.do.df = filter(incl.df, cmd=="include")
    lines = incl.do.df$line
    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      '\ninclude "',incl.do.df$repbox.file,'"',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )
  }

  before.inject.txt = new.txt

  lines = which(tab$cmd == "clear" & tab$arg_str=="all")
  if (length(lines)>0) {
    cat(paste0("\nReplace ", length(lines)," 'clear all' command in ", do$dofile," with 'clear' to prevent loss of repbox global variables.\n"))
    new.txt[lines] = "clear"
  }


  lines = setdiff(
    which(tab$cmd %in% c("save", "sa", "sav", "saveold", "gsave", "gzsave","erase","rm")),
    no.study.lines
  )
  new.txt[lines] = paste0(
    inject.intermediate.data.pre(lines, do, opts),
    new.txt[lines]
  )

  lines = setdiff(
    which(tab$cmd %in% c("erase","rm")),
    no.study.lines
  )
  new.txt[lines] = paste0(
    inject.intermediate.data.pre(lines, do, opts),
    new.txt[lines]
  )

  lines = setdiff(which(tab$cmd %in% c("use","u","us", "save","sa", "sav", "saveold", "clear","import","guse","gsave","gzuse","gzsave","rm","erase")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.use.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  # CACHE INJECTIONS
  cache_cmds = repbox_always_cache_cmd()
  lines = setdiff(which(tab$cmd %in% cache_cmds), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.cache_always(txt[lines], lines, do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)


  lines = setdiff(which(tab$cmd %in% c("preserve","restore")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.preserve.restore(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  lines = setdiff(which(tab$cmd %in% c("esttab") & !is.na(tab$using)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.esttab.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  lines = setdiff(which(tab$in_loop ==2), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.loop(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  gcmds = get.graphcmds()
  ngcmds = get.nographcmds()
  lines = setdiff(which(tab$cmd %in% gcmds & !(tab$cmd %in% ngcmds$cmd & tab$cmd2 %in% ngcmds)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.graph.save(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  if (opts$report.inside.program) {
    lines = which(tab$cmd == "program")
    new.txt[lines] = paste0(new.txt[lines],'\ndisplay "!.REPBOX.CUSTOM.PROGRAM>*"')
  }

  if (opts$extract.reg.info) {
    if (!require(repboxStataReg)) {
      cat("\nInjection of specific regression information is planned for a new package repboxReg. That package does not yet exist.\n")
      opts$extract.reg.info = FALSE
    }
  }

  if (opts$extract.reg.info) {
    lines = reg.rows = setdiff(which(tab$cmd %in% reg.cmds), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  } else {
    lines = reg.rows = setdiff(which(tab$cmd %in% reg.cmds), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg.simple(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }

  if (isTRUE(opts$extract.scalar.vals)) {
    lines = reg.rows = setdiff(which(tab$cmd %in% "scalar"), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.scalar(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }

  lines = which(startsWith(new.txt, "set maxvar"))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  lines = which(tab$cmd %in% c("br","browse", "pause","cls","stop") | (tab$cmd == "set" & is.true(startsWith(tab$cmd2,"trace"))))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  lines = which(tab$cmd %in% c("log","translate") )
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  if (isTRUE(opts$comment.out.install)) {
    lines = which(has.substr(new.txt, "ssc ") & has.substr(new.txt, " install "))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE

    lines = which(has.substr(new.txt, "sysdir ") & has.substr(new.txt, " set "))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE

  }

  if (!do$use.includes) {
    lines = which(tab$cmd %in% c("do","include","run"))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE
  }

  lines = setdiff(seq_len(NROW(tab)), c(special.lines, no.study.lines))
  inj.txt = injection.other(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  lines = setdiff(which(!tab$commented.out), no.study.lines)
  inj.txt = pre.injection(txt[lines],lines,do)
  new.txt[lines] = paste0(inj.txt,new.txt[lines])

  lines = setdiff(which(!is.na(tab$run.max)), no.study.lines)
  new.txt[lines] = inject.loop.max.run(new.txt[lines], before.inject.txt[lines], lines, do)

  tab$new.txt = new.txt

  org.file = do$file
  do.dir = dirname(org.file)
  org.base = basename(org.file)
  new.base = paste0("repbox_", org.base)
  new.file = file.path(do.dir, new.base)

  log.file = normalizePath(file.path(repbox.dir,"logs", paste0("log_", do$donum,".log")), mustWork=FALSE,winslash = "/")

  incl.log.file = normalizePath(file.path(repbox.dir,"logs", paste0("include_", do$donum,".log")), mustWork=FALSE, winslash = "/")

  log.name = paste0("repbox_log_", do$donum)

  start.timer.file = paste0(project_dir,"/repbox/stata/timer/start.txt")
  end.timer.file = paste0(project_dir,"/repbox/stata/timer/end.txt")

  txt = c(paste0('
file open repbox_timer_file using "', start.timer.file,'", write append
file write repbox_timer_file "', do$donum,';`c(current_time)\';`c(current_date)\'"
file write repbox_timer_file _n
file close repbox_timer_file

if "$repbox_cmd_count" == "" {
  set_defaults _all
  set more off
  global repbox_cmd_count = 0
  global repbox_root_donum = ', do$donum,'
  log using \"',log.file,'\", replace name(',log.name,')
  ', adopath.injection.code(project_dir),'
}
else {
  log using \"',incl.log.file,'\", replace name(',log.name,')
}
'),
          new.txt,
          paste0('
display "#~# FINISHED DO",
capture log close ', log.name,'

file open repbox_timer_file using "', end.timer.file,'", write append
file write repbox_timer_file "', do$donum,';`c(current_time)\';`c(current_date)\'"
file write repbox_timer_file _n
file close repbox_timer_file
'
          ))

  writeLines(txt, new.file)
  return(list(do=do,txt=invisible(txt)))
}
```
!END_MODIFICATION inject.do in repboxStata/R/inject.R

!MODIFICATION repbox_project_run_stata in repboxStata/R/repboxStata.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/repboxStata.R"
function_name = "repbox_project_run_stata"
description = "Ensure cached_dta directory is created before Stata runs"
---
```r
repbox_project_run_stata = function(project_dir, opts=repbox_stata_opts(), parcels=list(), ...) {
  restore.point("repbox_project_run_stata")
  options(dplyr.summarise.inform = FALSE)
  options(repbox.stata.options=opts)
  verbose = opts$verbose

  project = basename(project_dir)
  sup.dir = file.path(project_dir, "mod")
  setwd(sup.dir)
  repbox.dir = file.path(project_dir,"repbox/stata")

  res.file = file.path(repbox.dir,"repbox_results.Rds")
  if (!opts$force & file.exists(res.file)) {
    cat(paste0("\nStata replication results already exist for ", project_dir, "\n"))
    return(invisible(parcels))
  }

  if (opts$just.extract) {
    cat("\nJust extract results of previous run of Stata do files...\n")
    repbox_stata_extract(project_dir)
    return(invisible(parcels))
  }

  if (opts$check.stata.paths.and.ado) {
    check_stata_paths_and_ado(on_fail="error")
  }

  if (!dir.exists(repbox.dir)) dir.create(repbox.dir,recursive = TRUE)

  writeLines(
    "mode,found_file,org_file,sup_dir,cmd,default_ext,wdir",
    file.path(repbox.dir,"find_path_log.csv")
  )

  cmd.file = file.path(repbox.dir, "stata_cmd.csv")
  if (file.exists(cmd.file)) file.remove(cmd.file)

  try(remove.macosx.dirs(project_dir),silent = TRUE)

  do.files = list.files(sup.dir,glob2rx("*.do"),full.names = TRUE,recursive = TRUE)
  do.files = do.files[!startsWith(basename(do.files),"repbox_")]

  if (!is.null(opts[["just.files"]])) {
    do.files = do.files[basename(do.files) %in% opts$just.files]
  }

  if (verbose)
    cat("\nReplicate ", project, " with ", NROW(do.files), " do files.\n")

  if (NROW(do.files)==0) {
    if (verbose) cat("\nNo do files to analyse")
    return(invisible(parcels))
  }

  do.df = lapply(do.files, parse.sup.do, project_dir=project_dir) %>% bind_rows()

  do.df = add.includes.to.do.df(do.df)
  do.df$use.includes = opts$use.includes

  do.df = set.do.df.run.prio(do.df)

  do.df$project_dir = project_dir

  do.df$donum = seq_len(NROW(do.df))

  which.do = seq_len(NROW(do.df))

  if (opts$install.missing.modules) {
    for (i in which.do) {
      cat(paste0("\nCheck stata modules: ",i, " of ", length(which.do)))
      do.df$tab[[i]] =  tab.install.missing.modules(do.df$tab[[i]])
    }
  }

  incl.which.do = which.do[do.df$is.included[which.do]]

  if (!opts$use.includes) incl.which.do = NULL

  clear.and.create.dir(file.path(repbox.dir,"logs"))
  clear.and.create.dir(file.path(repbox.dir,"dta"))
  clear.and.create.dir(file.path(repbox.dir,"cached_dta"))
  clear.and.create.dir(file.path(repbox.dir,"cmd"))
  clear.and.create.dir(file.path(repbox.dir,"output"))
  clear.and.create.dir(file.path(repbox.dir,"timer"))

  repbox_intermediate_init(project_dir = project_dir, opts = opts)

  if (opts$extract.reg.info | !opts$keep.old.reg.info) {
    clear.and.create.dir(file.path(repbox.dir,"tsv"))
  } else {
    if (!dir.exists(file.path(repbox.dir,"tsv")))
      dir.create(file.path(repbox.dir,"tsv"))
  }

  incl.inject.res = lapply(incl.which.do, function(i) {
    do = do.df[i,]
    cat(paste0("\n inject code for included ", do$dofile))
    res = inject.do(do)
    res
  })

  if (opts$set.stata.defaults.perma) {
    setup.do.file = system.file("misc/stata_setup.do",package = "repboxStata")
    file.copy(setup.do.file, file.path(project_dir,"repbox/stata"))
    run_stata_do(file.path(project_dir,"repbox/stata/stata_setup.do"), verbose=FALSE)
  }

  run.start.time = Sys.time()
  which.do = setdiff(which.do, incl.which.do)

  do.li = lapply(seq_along(which.do), function(i) {
    do.i = which.do[i]
    do = do.df[do.i,]
    cat(paste0("\n",i, " of ", length(which.do), " inject and run"))
    do = stata.inject.and.run(do, opts=opts,start.time = run.start.time)
  })

  i = 2
  incl.extract.do.li = lapply(incl.which.do, function(i) {
    do = do.df[i,]
    log.file = file.path(repbox.dir,"logs",paste0("include_", do$donum,".log"))
    if (!file.exists(log.file)) {
      repbox_problem(type="included_do_no_log", msg=paste0("\n run ", do$dofile, " (no existing log even though it should have been included)\n"), fail_action = "msg")
    }
    if (!file.exists(log.file) & opts$rerun_failed.included.do) {
      do$is.included = FALSE
      do = stata.inject.and.run(do, opts=opts,start.time = run.start.time)
      return(do)
    }
    do$timeout = NA
    do$runtime = NA
    do
  })

  dotab = bind_rows(c(do.li, incl.extract.do.li)) %>%
    arrange(donum)
  saveRDS(dotab, file.path(repbox.dir,"dotab.Rds"))

  res = repbox_stata_extract(project_dir, dotab, opts=opts)

  parcels$stata_scalar = res$scalar_df

  invisible(parcels)
}
```
!END_MODIFICATION repbox_project_run_stata in repboxStata/R/repboxStata.R

!MODIFICATION extract.stata.caches in repboxStata/R/extract.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/extract.R"
insert_bottom = true
description = "Extract runid mapping of cache files after replication is finished"
---
```r
extract.stata.caches = function(project_dir, run.df) {
  restore.point("extract.stata.caches")
  cache.dir = file.path(project_dir, "repbox", "stata", "cached_dta")
  if (!dir.exists(cache.dir)) return(NULL)

  files = list.files(cache.dir, pattern = "\\.dta$", full.names = FALSE)
  if (length(files) == 0) return(NULL)

  parts = strsplit(gsub("\\.dta$", "", files), "_")
  donum = as.integer(sapply(parts, `[`, 2))
  line = as.integer(sapply(parts, `[`, 3))
  counter = as.integer(sapply(parts, `[`, 4))

  cache_df = tibble(donum = donum, line = line, counter = counter, cache_file = files)
  cache_df = cache_df %>%
    left_join(run.df %>% select(runid, donum, line, counter), by = c("donum", "line", "counter")) %>%
    filter(!is.na(runid))

  saveRDS(cache_df, file.path(cache.dir, "cache_files.Rds"))
  return(cache_df)
}
```
!END_MODIFICATION extract.stata.caches in repboxStata/R/extract.R

!MODIFICATION extract.stata.results in repboxStata/R/extract.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/extract.R"
function_name = "extract.stata.results"
description = "Call extract.stata.caches before returning the extracted results"
---
```r
extract.stata.results = function(project_dir, dotab, opts = rbs.opts()) {
  restore.point("extract.stata.results")

  project = basename(project_dir)
  tab = lapply(seq_len(NROW(dotab)), function(i) {
    tab = dotab$tab[[i]]
    tab$project = project
    tab$donum = dotab$donum[[i]]
    select(tab, project, donum, line,orgline, cmd,  everything())
  }) %>% bind_rows()

  run.df = extract.stata.run.cmds(project_dir)
  log.df = extract.stata.logs(project_dir)
  run.df = left_join(run.df, log.df, by=c("donum","line","counter"))
  run.df = left_join(run.df, select(tab, donum, line, orgline, cmd, is.regcmd, in.program, opens_block), by=c("donum", "line"))
  run.df = arrange(run.df, rootdonum, counter, donum,line)
  run.df = adapt.run.df.error.and.log(run.df, project_dir)
  run.df = adapt.run.df.for.timeout(run.df, dotab, project_dir, opts=opts)
  run.df = add.has.data.to.run.df(run.df)

  run.df = extract.stata.do.output(project_dir, run.df, opts=opts)

  run.df$runid = seq_len(NROW(run.df))

  tab = add.tab.error.info(tab, run.df)

  agg = run.df %>%
    group_by(donum) %>%
    summarize(
      run.err = any(is.true(runerr))
    )
  dotab = left_join(dotab, agg, by=c("donum"))

  dotab = extract.do.runtimes(project_dir, dotab)

  data.use = stata.repbox.data.use.info(run.df=run.df, dotab=dotab)
  saveRDS(data.use, file.path(project_dir,"repbox/stata/do_data_use.Rds"))

  extract.stata.caches(project_dir, run.df)

  runid_repbox_map = run.df %>%
    select(runid, donum, line, counter)
  saveRDS(runid_repbox_map, file.path(project_dir, "repbox/stata/runid_repbox_map.Rds"))

  list(run.df=run.df,tab=tab, dotab=dotab)
}
```
!END_MODIFICATION extract.stata.results in repboxStata/R/extract.R

!MODIFICATION drf_cache_files.R
scope = "file"
file = "/home/rstudio/repbox/repboxDRF/R/drf_cache_files.R"
description = "Create logic for DRF to import Stata caches and adapt execution paths"
---
```r
drf_import_stata_caches = function(drf, move = TRUE) {
  restore.point("drf_import_stata_caches")
  src_dir = file.path(drf$project_dir, "repbox", "stata", "cached_dta")
  dest_dir = file.path(drf$project_dir, "drf", "cached_dta")

  drf$run_df$has_file_cache = rep(FALSE, NROW(drf$run_df))
  drf$run_df$drf_cache_file = rep(NA_character_, NROW(drf$run_df))

  if (!dir.exists(src_dir)) return(drf)

  cache_info_file = file.path(src_dir, "cache_files.Rds")
  if (!file.exists(cache_info_file)) return(drf)

  cache_df = readRDS(cache_info_file)
  if (NROW(cache_df) == 0) return(drf)

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  cache_df$drf_cache_file = paste0(cache_df$runid, "_cache.dta")

  for (i in seq_len(NROW(cache_df))) {
    src_file = file.path(src_dir, cache_df$cache_file[i])
    dest_file = file.path(dest_dir, cache_df$drf_cache_file[i])

    if (file.exists(src_file)) {
      if (move) {
        file.rename(src_file, dest_file)
      } else {
        file.copy(src_file, dest_file, overwrite = TRUE)
      }
    }
  }

  drf$cache_df = cache_df %>% filter(!is.na(runid))
  return(drf)
}

drf_is_cache_safe = function(skipped_df, remaining_df) {
  # Heuristic 1: A cache replaces the dataset context but NOT macro memory context.
  # So, if skipped commands defined any memory elements (local, global, scalar, matrix),
  # it's unsafe to skip them, as remaining code might silently fail or behave incorrectly.
  unsafe_defs = grepl("^\\s*(local|global|scalar|matrix)\\b", skipped_df$cmdline)
  if (any(unsafe_defs)) return(FALSE)

  # Heuristic 2: If remaining commands query r(), e(), or matrices, 
  # they might depend on estimation commands that were skipped.
  unsafe_uses = grepl("\\b[re]\\(|\\bmatrix\\b", remaining_df$cmdline)
  if (any(unsafe_uses)) return(FALSE)

  return(TRUE)
}

drf_apply_caches = function(drf) {
  restore.point("drf_apply_caches")
  if (is.null(drf$cache_df) || NROW(drf$cache_df) == 0) return(drf)

  dest_dir = file.path(drf$project_dir, "drf", "cached_dta")
  available_caches = drf$cache_df$runid[file.exists(file.path(dest_dir, drf$cache_df$drf_cache_file))]

  if (length(available_caches) == 0) return(drf)

  path_df = drf$path_df
  run_df = drf$run_df
  pids = unique(path_df$pid)

  new_path_df_list = vector("list", length(pids))

  for (i in seq_along(pids)) {
    current_pid = pids[i]
    pdf = path_df[path_df$pid == current_pid, ]
    
    # Check if there are applicable caches for this specific path
    path_caches = intersect(pdf$runid, available_caches)

    if (length(path_caches) > 0) {
      path_caches = sort(path_caches, decreasing = TRUE)
      cache_applied = FALSE

      for (c_runid in path_caches) {
        skipped_df = run_df[run_df$runid %in% pdf$runid[pdf$runid < c_runid], ]
        remaining_df = run_df[run_df$runid %in% pdf$runid[pdf$runid > c_runid], ]

        if (drf_is_cache_safe(skipped_df, remaining_df)) {
          # Truncate the path to start EXACTLY at the cached runid
          pdf = pdf[pdf$runid >= c_runid, ]
          
          # Mark run_df globally so code generators know this runid can act as a load point
          cache_row_idx = match(c_runid, run_df$runid)
          drf$run_df$has_file_cache[cache_row_idx] = TRUE
          
          # Match to cache filename
          cache_filename = drf$cache_df$drf_cache_file[drf$cache_df$runid == c_runid]
          drf$run_df$drf_cache_file[cache_row_idx] = cache_filename[1]

          cache_applied = TRUE
          break
        }
      }
      new_path_df_list[[i]] = pdf
    } else {
      new_path_df_list[[i]] = pdf
    }
  }

  drf$path_df = bind_rows(new_path_df_list)
  return(drf)
}
```
!END_MODIFICATION drf_cache_files.R

!MODIFICATION drf_create in repboxDRF/R/drf_create.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_create.R"
function_name = "drf_create"
description = "Execute cache import and applying directly after creating base DRF mechanics"
---
```r
drf_create = function(project_dir, parcels=list(), acmds = drf_acmds(), overwrite=FALSE, move_from_mod=TRUE) {
  restore.point("drf_create")

  if (!overwrite & has_drf(project_dir)) {
    return(NULL)
  }
  project_dir = normalizePath(project_dir)

  drf = list(project_dir = project_dir, drf_dir = file.path(project_dir, "drf"), parcels = parcels, acmds=acmds)
  drf$parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)


  run_df = drf_make_run_df(drf=drf,add_rcode = FALSE)
  if (is.null(run_df)) {
    cat(("\nNo stata_run_cmd parcel exists, cannot create drf.\n"))
    return(NULL)
  }

  drf$run_df = run_df

  drf$pids = drf_find_pid(drf$run_df, drf$acmds)

  drf$path_df = drf_make_paths(drf)
  drf$runids = drf_runids(drf)

  drf_copy_org_data(drf=drf, move_from_mod=move_from_mod)

  # Incorporate Caches cleanly
  drf = drf_import_stata_caches(drf, move = move_from_mod)
  drf = drf_apply_caches(drf)

  # Save path_df index AFTER caches have definitively resolved the shortest paths
  drf$index_df = drf_save_path_df(drf=drf)

  drf = drf_make_r_trans_parcel(drf)
  invisible(drf)
}
```
!END_MODIFICATION drf_create in repboxDRF/R/drf_create.R

!MODIFICATION drf_run_df_create_rcode in repboxDRF/R/drf_r_code.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_r_code.R"
function_name = "drf_run_df_create_rcode"
description = "Translate cache loading exclusively if it acts as the path origin"
---
```r
drf_run_df_create_rcode = function(run_df=drf$run_df, runids=drf_runids(drf), drf=NULL) {
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
        drf_rel_path = paste0("cached_dta/", run_df$drf_cache_file[idx])
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

  run_df
}
```
!END_MODIFICATION drf_run_df_create_rcode in repboxDRF/R/drf_r_code.R

!MODIFICATION drf_stata_code_df in repboxDRF/R/drf_stata_code.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_stata_code.R"
function_name = "drf_stata_code_df"
description = "Inject a `use cache` command into direct Stata replication formats if the path was truncated"
---
```r
drf_stata_code_df = function(drf,runids=NULL, path_merge = c("none", "load", "natural", "load_natural")[4]) {
  restore.point("drf_stata_code_skel")
  pids = runids
  path_df = drf$path_df
  if (!is.null(pids)) {
    path_df = path_df %>%
      filter(pid %in% pids)
  }
  pids = unique(path_df$pid)
  if (length(pids)<=1) path_merge = "none"

  restore_code = function(data_path) {
    paste0("* Restore previously loaded data set", basename(data_path), "\nframe copy cache_frame default, replace")
  }
  preserve_code = function() {
    "\nframe copy default cache_frame, replace"
  }


  run_df = drf$run_df
  run_df = run_df %>% semi_join(path_df, by="runid")

  run_df$code = run_df$cmdline

  run_df = drf_replace_run_df_code_data_path(run_df = run_df, drf=drf)
  run_df$data_path = run_df$org_data_path

  path_li = split(path_df, path_df$pid)
  code_li = NULL
  pid = pids[1]
  if (path_merge == "none") {
    code_li = lapply(pids, function(pid) {
      pdf = path_li[[as.character(pid)]]
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      
      # Handle path truncation due to cache
      if (isTRUE(rdf$has_file_cache[1])) {
        rdf$code[1] = paste0('use "drf/cached_dta/', rdf$drf_cache_file[1], '", clear')
        rdf$aux_cmd_type[1] = "load_cache"
      }
      
      rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type=na.val(aux_cmd_type,""))
    })
    sc_df = bind_rows(code_li)
    return(sc_df)
  }
  ps_df = path_df %>%
    group_by(pid) %>%
    summarize(
      first_runid = min(runid),
      last_runid = max(runid),
    ) %>%
    left_join(run_df %>% select(first_runid=runid, data_path), by="first_runid")

  data_df = ps_df %>%
    group_by(data_path) %>%
    summarize(
      data_runid = min(first_runid),
      data_num_paths = n()
    ) %>%
    arrange(data_runid)

  ps_df = ps_df %>%
    left_join(data_df, by="data_path")

  merge_load = path_merge %in% c("load", "load_natural")
  merge_natural = path_merge %in% c("natural", "load_natural")

  if (merge_load) {
    ps_df = ps_df %>%
      ungroup() %>%
      arrange(data_runid, first_runid, last_runid) %>%
      mutate(
        restore_data = is.true(lag(data_runid)==data_runid),
        preserve_data = !restore_data & data_num_paths > 1
      )
  } else {
    ps_df = ps_df %>%
      arrange(first_runid, last_runid) %>%
      mutate(restore_data = FALSE, preserve_data=FALSE)
  }

  pids = ps_df$pid
  if (!merge_natural) {
    code_li = lapply(pids, function(pid) {
      pdf = path_li[[as.character(pid)]]
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      
      # Cache implementation for unmerged steps
      if (isTRUE(rdf$has_file_cache[1])) {
        rdf$code[1] = paste0('use "drf/cached_dta/', rdf$drf_cache_file[1], '", clear')
      }
      
      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)

      ps = ps_df[ps_df$pid==pid,]
      if (ps$preserve_data) {
        rdf$code[1] = paste0(rdf$code[1],preserve_code())
        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = restore_code(ps$data_path)
        rdf$aux_cmd_type[1] = paste0("restore")
      }
      rdf
    })
    sc_df = bind_rows(code_li)
    return(sc_df)
  }

  code_li = vector("list", length(pids))
  opdf = NULL
  counter = 0

  while (counter < length(pids)) {
    counter = counter+1
    pid = pids[counter]
    pdf = path_li[[as.character(pid)]]
    if (is.null(opdf) | NROW(opdf)>=NROW(pdf)) {
      restart = TRUE
    } else {
      restart = !all(opdf$runid == pdf$runid[1:NROW(opdf)])
    }

    if (restart) {
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      
      if (isTRUE(rdf$has_file_cache[1])) {
        rdf$code[1] = paste0('use "drf/cached_dta/', rdf$drf_cache_file[1], '", clear')
      }
      
      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)

      ps = ps_df[ps_df$pid==pid,]

      if (ps$preserve_data) {
        rdf$code[1] = paste0(rdf$code[1],preserve_code())
        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = restore_code(ps$data_path)
        rdf$aux_cmd_type[1] = paste0("restore")
      }
      opdf = pdf

    } else if (!restart) {
      npdf = pdf %>% filter(runid > max(opdf$runid))
      opdf = pdf
      pdf = npdf

      rdf = run_df[run_df$runid %in% pdf$runid, ]
      
      # Unlikely inside a continued sequence, but keeps it safe
      if (isTRUE(rdf$has_file_cache[1])) {
        rdf$code[1] = paste0('use "drf/cached_dta/', rdf$drf_cache_file[1], '", clear')
      }
      
      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre="", post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)
    }
    code_li[[counter]] = rdf
  }
  sc_df = bind_rows(code_li)
  sc_df
}
```
!END_MODIFICATION drf_stata_code_df in repboxDRF/R/drf_stata_code.R
