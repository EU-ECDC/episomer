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
install_no_deps <- nzchar(Sys.getenv("INSTALL_NO_DEPS"))
lib <- .libPaths()[1]
if (install_no_deps && nzchar(Sys.getenv("R_LIBS_USER")))
  lib <- Sys.getenv("R_LIBS_USER")
remotes::install_local(sprintf("install/%s", installer_name[[1]]),
                       lib = lib, dependencies = !install_no_deps)

if(Sys.getenv("RENV")!="" && ("renv" %in% installed.packages()[,"Package"]))
	renv::snapshot()
