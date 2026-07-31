if(Sys.getenv("EPISOMER_RUN")=="APP") {
    episomer::episomer_app()
} else if(Sys.getenv("EPISOMER_RUN")=="ADMIN") {
    episomer::admin_app()
} else if(Sys.getenv("EPISOMER_RUN")=="DASH") {
    episomer::dashboard_app()
} else if(Sys.getenv("EPISOMER_RUN")=="DATA") {
    #Ensuring setup is running
    episomer::setup(silent=T)
    message("Launghing the FS Loop")
    episomer::fs_loop()
} else if(Sys.getenv("EPISOMER_RUN")=="DETECT") {
    episomer::detect_loop()
} else if(Sys.getenv("EPISOMER_RUN")=="SEARCH") {
    episomer::search_loop()
} else {
    stop("No startup insstruction found. Please set environment variable EPISOMER_RUN with any of (ADMIN, DASH, APP, DATA, DETECT or SEARCH)")
}

