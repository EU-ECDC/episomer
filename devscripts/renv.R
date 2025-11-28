# creating package and installing it
print(getwd())
repos = "https://cran.asnr.fr/"
if(!("renv" %in% installed.packages()[,"Package"]))
  install.packages("renv", repos = repos)

renv::init(bare = F, settings = list("ignored.packages" = c("episomer", "gtools", "taskscheduleR")))
renv::install("devtools")
devtools::install_local("./episomer", dependencies=F)
renv::snapshot()
