# creating package and installing it
print(getwd())
repos = "https://cran.asnr.fr/"

if(!("devtools" %in% installed.packages()[,"Package"]))
  install.packages("devtools", repos = repos)

devtools::install_deps("epitweetr")
devtools::document("epitweetr")
renv::snapshot()
devtools::build_manual("epitweetr")
#devtools::build_vignettes()

if(!file.exists(file.path("install"))){
  dir.create(file.path("install"), showWarnings = FALSE)
}
if(!file.exists(file.path("manual"))){
  dir.create(file.path("manual"), showWarnings = FALSE)
}

if(any(grepl("epitweetr.*", sessionInfo())))
  detach("package:epitweetr", unload=TRUE)

installer_path <- c(
  devtools::build("epitweetr", binary=FALSE, vignettes=TRUE, manual=TRUE)
  ,devtools::build("epitweetr", binary=TRUE, vignettes=TRUE, manual=TRUE)
)

#install.packages(file.path(installer_path[[2]]), dependencies = FALSE)
devtools::load_all("epitweetr")

#moving installer
installer_name <- (
  c(paste("epitweetr_",packageVersion("epitweetr"),".tar.gz", sep = ""),
    if(.Platform$OS.type == "windows")
      paste("epitweetr_",packageVersion("epitweetr"),".zip", sep = "") 
    else
      paste("epitweetr_",packageVersion("epitweetr"),"_R_x86_64-pc-linux-gnu.tar.gz", sep = "") 
  )
)

file.rename(file.path(installer_path), file.path("install", installer_name))

manual_name <- paste("epitweetr_",packageVersion("epitweetr"),".pdf", sep = "") 
#moving manual
file.rename(file.path(manual_name), file.path("manual", manual_name))



