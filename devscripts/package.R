# creating package and installing it
print(getwd())
repos = "https://cran.asnr.fr/"

if(!("devtools" %in% installed.packages()[,"Package"]))
  install.packages("devtools", repos = repos)
fast <- TRUE

if(!fast) {
    devtools::install_deps("episomer")
    devtools::document("episomer")
    renv::snapshot()
    devtools::build_manual("episomer")
    devtools::build_vignettes("episomer")
}

if(!file.exists(file.path("install"))){
  dir.create(file.path("install"), showWarnings = FALSE)
}
if(!file.exists(file.path("manual"))){
  dir.create(file.path("manual"), showWarnings = FALSE)
}

if(any(grepl("episomer.*", sessionInfo())))
  detach("package:episomer", unload=TRUE)

installer_path <- c(
  devtools::build("episomer", binary=FALSE, vignettes=TRUE, manual=TRUE)
  ,devtools::build("episomer", binary=TRUE, vignettes=TRUE, manual=TRUE)
)

#install.packages(file.path(installer_path[[2]]), dependencies = FALSE)
devtools::load_all("episomer")

#moving installer
installer_name <- (
  c(paste("episomer_",packageVersion("episomer"),".tar.gz", sep = ""),
    if(.Platform$OS.type == "windows")
      paste("episomer_",packageVersion("episomer"),".zip", sep = "") 
    else
      paste("episomer_",packageVersion("episomer"),"_R_x86_64-pc-linux-gnu.tar.gz", sep = "") 
  )
)

file.rename(file.path(installer_path), file.path("install", installer_name))
if(!fast) {
    manual_name <- paste("episomer_",packageVersion("episomer"),".pdf", sep = "") 
    #moving manual
    file.rename(file.path(manual_name), file.path("manual", manual_name))
}


