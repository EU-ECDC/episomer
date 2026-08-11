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

# Installing
devtools::install_local(sprintf("install/%s", installer_name[[1]]), dependencies=T) 

if(Sys.getenv("RENV")!="" && ("renv" %in% installed.packages()[,"Package"]))
	renv::snapshot()
