#######
## IOTC SWO-MSE: Install packages for FLR

# Dependencies in CRAN
#install.packages(c("data.table", "foreach", "ggplot2"))
#install.packages("gridExtra")

# r4ss needs to be latest version >= 1.27.0 from github 
library(devtools)
devtools::install_github("r4ss/r4ss",force=T)

# Install FLR packages
Mydir <- "D:/IOTC/MSE-IOTC-SWO_JRC/git/SWO/om/pkgs/src/"

install.packages("r4ss", repos="http://flr-project.org/R")
install.packages(file.path(Mydir, "FLasher_*.tar.gz"), INSTALL_opts="--no-multiarch", repos = NULL, type="source")
install.packages(file.path(Mydir, "FLCore_*.tar.gz"), repos = NULL, type="source")
install.packages(file.path(Mydir, "mse_*.tar.gz"),INSTALL_opts="--no-multiarch", repos = NULL, type="source")
install.packages(file.path(Mydir, "r4ss_*.tar.gz"), repos = NULL, type="source")
install.packages(file.path(Mydir, "ss3om_*.tar.gz"),INSTALL_opts="--no-multiarch", repos = NULL, type="source")
install.packages(file.path(Mydir, "ioswomse_0.0.4.tar.gz"), INSTALL_opts="--no-multiarch", repos = NULL, type="source")


# Load
library(FLasher); citation("FLasher")
library(FLCore); citation("FLCore")
library(mse); citation("mse")
library(ss3om); citation("ss3om")
library(r4ss); citation("r4ss")

