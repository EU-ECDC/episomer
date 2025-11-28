# creating package and installing it
setwd("test")
print(getwd())
repos = "https://cran.asnr.fr/"
if(!("renv" %in% installed.packages()[,"Package"]))
  install.packages("renv", repos = repos)

package <- max(sort(list.files("../install")))
renv::init(bare = F)

if(!("devtools" %in% installed.packages()[,"Package"])) {
    print("installing devtools")
    renv::install("devtools")
}

print(sprintf("installing %s", package))
devtools::install_local(sprintf("../install/%s", package), dependencies=T)
renv::snapshot()
