# SFNRC

[![Build status](https://ci.appveyor.com/api/projects/status/cpjeexfdjcw7syd7?svg=true)](https://ci.appveyor.com/project/troyhill/sfnrc)
 [![Build Status](https://travis-ci.com/troyhill/SFNRC.svg?branch=master)](https://travis-ci.com/github/troyhill/SFNRC) [![codecov.io](https://codecov.io/github/troyhill/SFNRC/coverage.svg?branch=master)](https://codecov.io/github/troyhill/SFNRC?branch=master)

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SFNRC)](https://cran.r-project.org/package=SFNRC) 

The `SFNRC` R package supports integration and analysis of water quality and hydrology data in south Florida. The package provides an API for the South Florida Water Management District's DBHYDRO database. For users connected to the South Florida Natural Resources Center's servers, the package also provides direct access to DataForEver data.


## Install the package

Install the SFNRC R package from GitHub using the following commands in the R console:

```r
install.packages("devtools")
devtools::install_github("troyhill/SFNRC")

library(SFNRC)
```

### Tips

Installing this package from out-dated versions of R (NPS users) will cause an error. To change this behavior, adjust the `R_REMOTES_NO_ERRORS_FROM_WARNINGS` system variable before installation.

```r 
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
```

Until tryCatch statements are used in the DataForEver functions, `getDFE()` calls that fail may leave their database connections intact. It may be necessary at some point to kill all database connections. I use the `DBI` package for this (`odbc` is installed with the `SFNRC` package):

```r
if (!"DBI" %in% installed.packages()) {install.packages("DBI")}
### to kill all SQL connections
DBI::dbListConnections( DBI::dbDriver( drv = "MySQL"))
lapply( DBI::dbListConnections( DBI::dbDriver( drv = "MySQL")), odbc::dbDisconnect)             
```

## License

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.
