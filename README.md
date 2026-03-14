# pemr

Read and manage data collected using various personal environmental monitor
(PEM) types.


### Available PEM support

Supported PEMs so far include ECM, UPAS and PATS+.


### Installation

You can install this package using the `devtools` package using:

`devtools::install_github("odeleongt/pemr")`


### Use

To use this package, load it and provide information about the PEM files,
monitor types, and any additional metadata you need:


```r
library(package = "pemr")

data_frame(
    files_col = c("path/to/file1.csv", "path/to/file2.csv", "path/to/file3.csv"),
    types_col = c("ecm", "upas", "patsp"),
    sites = c("site1", "site2", "site3")
  ) %>%
  mutate(
    data = map2(files_col, types_col, read_monitor)
  ) %>%
  collect_data(
    type = types_col, data_col = data,
    sites, files_col
  )
```


### Development

So far simple data management functionality is implemented.
Only the time stamp and measures for concentration (for ECM and PATS+) and flow
(for ECM and UPAS) are extracted from the data, but each sensor type collects
a wealth of additional data.

If you are interested in collaborating or want to suggest improvements and
fixes please do so in the issues page for this repository.

