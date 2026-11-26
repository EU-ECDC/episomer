if(Sys.getenv("EPISOMER_RUN")=="APP") {
    episomer::episomer_app()
} else if(Sys.getenv("EPISOMER_RUN")=="ADMIN") {
    episomer::admin_app()
} else if(Sys.getenv("EPISOMER_RUN")=="DASH") {
    episomer::dashboard_app()
} else if(Sys.getenv("EPISOMER_RUN")=="FS") {
    #Ensuring setup has running
    episomer::setup(silent=T, launch=F)
    message("Launching the FS Loop")
    episomer::fs_loop()
} else if(Sys.getenv("EPISOMER_RUN")=="DETECT") {
    episomer::detect_loop()
} else if(Sys.getenv("EPISOMER_RUN")=="SEARCH") {
    episomer::search_loop()
} else {
    message("No startup instruction found. Please set environment variable EPISOMER_RUN with any of (ADMIN, DASH, APP, FS, DETECT or SEARCH)")
    message("Sleeping forever")
    Sys.sleep(Inf)
}

