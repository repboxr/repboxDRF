# A note on caches


## file caches
Data file caches are stored in `drf/cached_dta/{runid}_cache.dta`. 

They cache the data set **AFTER** the command was run. 

For a regression they store the filtered data set including the if / in condition.

## memore cache

Functions starting with `mcache_` We have memory caches for file_path. We then cache the loaded data set under that file path.

We also have memory data sets for runid, but only for those corresponding to a regression. There we store the if / in condition.

