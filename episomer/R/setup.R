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

    # Making sure we know where is the data folder and calling setup_config
    if(is.null(data_dir)) {
        if(!silent) {
            message("Please choose the location of the episomer data folder")
            data_dir <- file.choose()
        } else if (exists("data_dir", where = conf)) {
            data_dir <- conf$data_dir
        } else {
             stop("Cannot continue the episomer setup wizard, cannot determine the data_folder to use")
        }
        
    }
    setup_config(data_dir)
    
    #defining default actions
    defaults <- list(
        get_jars = ! tryCatch(check_java_deps(), warning=function(e) FALSE) 
    )
   
    ans <- choose_between("Which action would you like perform?", list(
        "0"="Run missing steps",
        "1"="Run all steps",
        "2"="Download jar dependencies"
    ))
    
    if((ans %in% list("1", "2")) || (ans == 0 && defaults[["get_jars"]])) {
       pause(silent)
       message("Downloading java dependencies and winutils if windows")
       download_dependencies()
    }
}

pause <- function(silent=FALSE) { if(!silent) Sys.sleep(0.5) }

choose_between <- function(question, ops=list()) {
    if(length(options) > 0) {
        answered <- FALSE
        while(!answered) {
           pause()
           message(sprintf("\n%s", question))
           pause()
           message <- paste(lapply(names(ops), function(k) sprintf(".... %s <- %s", k, ops[[k]])), collapse="\n")
           ans <- readline(sprintf("%s\n your choice:", message))
           answered <- exists(ans, where=ops)
           if(!answered) {
               pause()
               message(sprintf("...\n...\nsorry, %s is not among the available options, \nplease try again", ans))
           }
        }
        return(ans)
    }
}
