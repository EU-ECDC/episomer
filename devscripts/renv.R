# creating package and installing it
print(getwd())
repos = "https://cran.asnr.fr/"
if(!("renv" %in% installed.packages()[,"Package"]))
  install.packages("renv", repos = repos)

if(!dir.exists(file.path("renv")))
  renv::init(bare = T, settings = list("ignored.packages" = c("epitweetr", "gtools", "taskscheduleR")))
