# SFNRC


## Install the package

Install the SFNRC R package from GitHub using the following commands in the R console:

```r
install.packages("devtools")
devtools::install_github("troyhill/SFNRC", build_vignettes = TRUE)

library(SFNRC)
```


## Troubleshooting installation

On some systems an initial call to library(SFNRC) generates the following error message: "Error: package ‘gstat’ could not be loaded"

This can be resolved by restarting R, removing the packages "gstat", "sp", and "maps", and reinstalling gstat with argument "dependencies = TRUE":

```r
remove.packages(c("gstat", "sp", "maps"))
install.packages("gstat", dependencies = TRUE)

library(SFNRC)
```



## License

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.
