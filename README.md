# SFNRC

[![Build Status](https://travis-ci.org/troyhill/SFNRC.svg?branch=master)](https://travis-ci.org/troyhill/SFNRC) [![codecov.io](https://codecov.io/github/r-lib/SFNRC/coverage.svg?branch=master)](https://codecov.io/github/r-lib/SFNRC?branch=master)

The `SFNRC` R package supports integration and analysis of water quality and hydrology data in south Florida. The package is optimized for use on linux operating systems connected to the South Florida Natural Resources Center's intranet, conditions that provides command-line access to internal databases.


## Install the package

Install the SFNRC R package from GitHub using the following commands in the R console:

```r
install.packages("devtools")
devtools::install_github("troyhill/SFNRC", build_vignettes = TRUE)

library(SFNRC)
```

The linux version of the package comes with an introductory vignette showing some capabilities and usage examples:

```r
vignette("DataForEver")
```

Users of non-linux systems can view a pdf of the vignette [here](https://github.com/troyhill/SFNRC/blob/master/vignettes/DataForEver.pdf), although the DataForEver APIs only work on linux operating systems. 










## License

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.
