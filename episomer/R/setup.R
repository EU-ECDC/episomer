#' @title Run the setup of episomer
#' @description Perform configuration tasks needed for the proper work of episomer or change settings of these configuration
#' @param data_dir Character the data directory where episomer will store all files. If not provided and already set in the current session it will reused, default: NULL
#' @param silent Logical whether episomer should take default actions without asking the user default: FALSE
#' @return NULL.
#' @details Perform configuration tasks needed for the proper work of episomer or change settings of these configuration
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup(file.choose())
#' }
#' @rdname setup
#' @export
setup <- function(data_dir = NULL, silent = FALSE) {
    message("Welcome to the episomer setup wizard, you will go through a series of steps to ensure episomer works properly")

    ####################################################################################
    ### Making sure we know where is the data folder and calling setup_config  #########
    ####################################################################################
    if(is.null(data_dir)) {
        if(!silent 
            && exists("data_dir", where = conf)
            && (choose_between(
                    sprintf("Use '%s' as data folder?", conf$data_dir), 
                    list(
                        "1"="Yes", 
                        "2"="No, let me choose another directory"
                    ),
                    default = "1"
                ) == "1"
            )
        ) {
            data_dir <- conf$data_dir
        } else if(!silent){
            message("Please choose the location of the episomer data folder")
            data_dir <- file.choose()
        } else if (exists("data_dir", where = conf)) {
            data_dir <- conf$data_dir
        } else {
             stop("Cannot continue the episomer setup wizard, cannot determine the data_folder to use")
        }
        
    }
    setup_config(data_dir)
    
    ####################################################################################
    ###  MAIN wizard menu  #############################################################
    ####################################################################################
    #defining default actions
    defaults <- list(
        get_jars = ! tryCatch(check_java_deps(), warning=function(e) FALSE), 
        get_winutils = ! tryCatch(check_winutils(), warning=function(e) FALSE), 
        maven_url = conf$maven_repo,
        winutils_url = conf$winutils_url,
        get_geonames = !file.exists(get_geonames_txt_path()),
        assemble_geonames = ! file.exists(get_geonames_parquet_path()),
        index_geonames = ! tryCatch(check_geonames(), warning=function(e) FALSE),
        geonames_url = conf$geonames_url
    )
    default = "1"
    main_ans <- choose_between("Which action would you like perform?", 
        list(
            "1"="Run missing steps",
            "2"="Run all steps",
            "3"="Download dependencies",
            "4"="Update geonames",
            "5"="Update languages",
            "6"="Setup social media credentials",
            "7"="Run episomer services",
            "8"="Run the shiny app"
        ),
        silent,
        default
    )
    
    ####################################################################################
    ### Option 3: Downloading dependencies  ############################################
    ####################################################################################
    if(main_ans %in% list("1", "2", "3")) {
        pause(silent)
        if(main_ans == "2"|| defaults[["get_jars"]])
           default = "1"
        else
           default = "3"

        jar_ans = choose_between("Download JAVA packages?", 
            list(
                "1"=sprintf("yes, from %s", conf$maven_repo), 
                "2"="yes, from other repo",
                "3"="no"
            ), 
            silent,  
            default
        )
        download_jars <- (jar_ans %in% list(1, 2))
        
        if(jar_ans == "2") {
            repo = readline(sprintf("Please enter the maven URL to download as formatted in (%s)"), get_empty_config(conf$data_dir)$maven_repo)
            conf$maven_repo = repo
            save_config(sm_topics=FALSE)
        }
        pause(silent)
        if(main_ans == "2"|| defaults[["get_winutils"]])
           default = "1"
        else
           default = "3"
        if (.Platform$OS.type == "windows" && winutils) {
            winutil_ans = choose_between("Download WINUTILS?", 
                list(
                    "1"=sprintf("yes, from %s", conf$winutils_url), 
                    "2"="yes, from other URL",
                    "3"="no"
                ), 
                silent,  
                default
            )
            download_winutils <- (winutils_ans %in% list(1, 2))
            if(winutils_ans == "2") {
                wurl = readline("Please enter the winutils URL to download from")
                conf$winutils_url = wurl
                save_config(sm_topics=FALSE)
            }
            
        } else {
            download_winutils <- FALSE
        }
        pause(silent)
        message("Downloading dependencies")
        download_dependencies(jars = download_jars, winutils = download_winutils)
    }

    ####################################################################################
    ### Option 4. Update geonames  #####################################################
    ####################################################################################
    if(main_ans %in% list("1", "2", "4")) {
        # Downloading geonames
        pause(silent)
        if(main_ans == "2" || defaults[["get_geonames"]])
           default_get_geo = "1"
        else
           default_get_geo = "3"
        get_geo_ans = choose_between("Do you want to download geonames?", 
            list(
                "1"=sprintf("yes, from %s", conf$geonames_url), 
                "2"="yes, from other URL",
                "3"="no"
            ), 
            silent,  
            default_get_geo
        )
        get_geonames <- (get_geo_ans %in% list(1, 2))
        
        if(get_geo_ans == "2") {
            gurl = readline("Please enter the URL to download geonames from")
            conf$geonames_url <- gurl
            save_config(sm_topics=FALSE)
        }
        # Assembling geonames
        pause(silent)
        if(main_ans == "2" || defaults[["assemble_geonames"]])
           default_assemble_geo = "1"
        else
           default_assemble_geo = "2"
        ass_geo_ans = choose_between("Do you want to assemble geonames?", 
            list(
                "1"="yes", 
                "2"="no"
            ), 
            silent,  
            default_assemble_geo
        )
        ass_geonames <- (ass_geo_ans %in% list(1))
        
        # Indexing geonames
        pause(silent)
        if(main_ans == "2" || defaults[["index_geonames"]])
           default_index_geo = "1"
        else
           default_index_geo = "2"
        index_geo_ans = choose_between("Do you want to index geonames?", 
            list(
                "1"="yes", 
                "2"="no"
            ), 
            silent,  
            default_index_geo
        )
        index_geonames <- (index_geo_ans %in% list(1))
        message("") 
        update_geonames(download = get_geonames, assemble = ass_geonames, index = index_geonames) 

        
    }

}

pause <- function(silent=FALSE) { if(!silent) Sys.sleep(0.5) }

choose_between <- function(question, ops=list(), silent=FALSE, default=NULL) {
    if(length(options) > 0) {
        answered <- FALSE
        while(!answered) {
           pause(silent)
           message(sprintf("\n%s", question))
           pause(silent)
           message <- paste(lapply(names(ops), function(k) sprintf(".... %s <- %s%s", k, ops[[k]], ifelse(!is.null(default) && k == default, " (default)", ""))), collapse="\n")
  
           if(silent) {
               message(message)
               message(default)
               return(default)
           }
           ans <- readline(sprintf("%s\n your choice%s", message, ifelse(!is.null(default), " (or ↵ ):", ":")))
           if(!is.null(default) && ans == "") {
               message(default)
               return(default)
           }
           answered <- exists(ans, where=ops)
           if(!answered) {
               pause()
               message(sprintf("...\n...\nsorry, %s is not among the available options, \nplease try again", ans))
           }
        }
        return(ans)
    }
}
