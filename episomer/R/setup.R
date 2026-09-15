#' @title Run the setup of episomer
#' @description Perform configuration tasks needed for the proper work of episomer or change settings of these configuration
#' @param data_dir Character the data directory where episomer will store all files. If not provided and already set in the current session it will reused, default: NULL
#' @param silent Logical whether episomer should take default actions without asking the user default: FALSE
#' @param launch Logical whether episomer should run automatically extra steps when selecting Option 1 or 2 in the wizard (setting credentials, running services and shiny app)
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
setup <- function(data_dir = NULL, silent = FALSE, launch = TRUE) {
    message("Welcome to the episomer setup wizard, you will go through a series of steps to ensure episomer works properly\n\n. Episomer tasks may be Stopped")

    if(!setup_can_run()) {
      stop("Missing requirements for episomer setup to run properly, please check previous warnings (probably related with missing java or wrong version)")
    }
    ####################################################################################
    ### Making sure we know where is the data folder and calling setup_config  #########
    ####################################################################################
    if(is.null(data_dir)) {
        if(!silent 
            && exists("data_dir", where = conf) 
            && (choose_between(
                    "Do you want to keep current data folder?", 
                    list(
                        "1"=sprintf("Yes, use '%s'", conf$data_dir), 
                        "2"="No, let me choose another directory"
                    ),
                    default = "1"
                ) == "1"
            )
        ) {
            data_dir <- conf$data_dir
        } else if(!silent){
            message("Please choose the location of the episomer data folder")
            if (.Platform$OS.type == "windows") {
              data_dir <- choose.dir()
            } else {
              data_dir <- file.choose()
            }
            if(is.null(data_dir) || is.na(data_dir)) {
               data_dir <- readline("Could not read the data dir. Please type the path of the folder?: \n")
            }
            if(is.null(data_dir) || length(data_dir)==0) 
               stop("Could not retrieve the data dir")

        } else if (exists("data_dir", where = conf)) {
            data_dir <- conf$data_dir
        } else if(Sys.getenv("EPI_HOME")!="") {
            data_dir <- Sys.getenv("EPI_HOME")
        } else {
             stop("Cannot continue the episomer setup wizard, cannot determine the data_folder to use")
        }
        
    }
    if(is.na(data_dir) || is.null(data_dir)) {
      stop("Could not determine the data dir, please pass it as a parameter while runing setup")
    }
 
    dir.create(data_dir, showWarnings=FALSE, recursive=TRUE)
    setup_config(data_dir, fs_remote = NULL)
    
    ####################################################################################
    ###  MAIN wizard menu  #############################################################
    ####################################################################################
    #defining default actions
    conf_langs <- unlist(lapply(conf$languages, `[[`, "code"))
    missing_langs <- conf_langs[!file.exists(unlist(lapply(conf$languages, `[[`, "vectors")))] 
    
    defaults <- list(
        get_jars = ! tryCatch(check_java_deps(), warning=function(e) FALSE), 
        get_winutils = ! tryCatch(check_winutils(), warning=function(e) FALSE), 
        maven_url = conf$maven_repo,
        winutils_url = conf$winutils_url,
        get_geonames = !file.exists(get_geonames_txt_path()),
        assemble_geonames = ! file.exists(get_geonames_parquet_path()),
        index_geonames = ! tryCatch(check_geonames(), warning=function(e) FALSE),
        geonames_url = conf$geonames_url,
        get_langs = missing_langs,
        remove_langs = list(),
        train_langs = get_lang_traing_need(),
        set_sm_credentials = !tryCatch(check_sm_auth(), warning=function(e) FALSE)
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
            "7"="Run / stop  episomer services",
            "8"="Run the shiny app",
            "9"="Do nothing and exit"
        ),
        silent,
        default
    )
    if(main_ans %in% c("1", "2"))
        auto <- TRUE
    else
        auto <- FALSE 
    
    ####################################################################################
    ### Option 9: Aborting or stopping services  #######################################
    ####################################################################################
    if(main_ans == 9) {
       alert("Aborting setup")
    } else if(!(main_ans %in% c(6, 7, 8))){
       message("Stopping any running service")
       stop_all_runner_tasks()
    }
    ####################################################################################
    ### Option 3: Downloading dependencies  ############################################
    ####################################################################################
    if(main_ans %in% list("1", "2", "3")) {
        pause(silent)
        if(main_ans == "2"|| defaults[["get_jars"]])
           default = "1"
        else
           default = "3"
        is_silent <- silent | (auto && default == "3")

        jar_ans <- choose_between("Download JAVA packages?", 
            list(
                "1"=sprintf("yes, from %s", conf$maven_repo), 
                "2"="yes, from other repo",
                "3"="no"
            ), 
            is_silent,  
            default
        )
        download_jars <- (jar_ans %in% list(1, 2))
        
        if(jar_ans == "2") {
            repo = readline(sprintf("Please enter the maven URL to download as formatted in (%s)", get_empty_config(conf$data_dir)$maven_repo))
            conf$maven_repo = repo
            save_config()
        }
        pause(silent)
        if(main_ans == "2"|| defaults[["get_winutils"]])
           default <- "1"
        else
           default <- "3"
        if (.Platform$OS.type == "windows") {
            is_silent <- silent | (auto && default == "3")
            winutils_ans <- choose_between("Download WINUTILS?", 
                list(
                    "1"=sprintf("yes, from %s", conf$winutils_url), 
                    "2"="yes, from other URL",
                    "3"="no"
                ), 
                is_silent,  
                default
            )
            download_winutils <- (winutils_ans %in% list(1, 2))
            if(winutils_ans == "2") {
                wurl <- readline("Please enter the winutils URL to download from")
                conf$winutils_url <- wurl
                save_config()
            }
            
        } else {
            download_winutils <- FALSE
        }
        pause(silent)
        if( download_jars || download_winutils) {
          message("Downloading dependencies")
          download_dependencies(jars = download_jars, winutils = download_winutils)
        }
    }

    ####################################################################################
    ### Option 4. Update geonames  #####################################################
    ####################################################################################
    if(main_ans %in% list("1", "2", "4")) {
        # Downloading geonames
        pause(silent)
        if(main_ans == "2" || defaults[["get_geonames"]])
           default_get_geo <- "1"
        else
           default_get_geo <- "3"
        is_silent <- silent | (auto && default_get_geo == "3")
        get_geo_ans <- choose_between("Do you want to download geonames?", 
            list(
                "1"=sprintf("yes, from %s", conf$geonames_url), 
                "2"="yes, from other URL",
                "3"="no"
            ), 
            is_silent,  
            default_get_geo
        )
        get_geonames <- (get_geo_ans %in% list(1, 2))
        
        if(get_geo_ans == "2") {
            gurl = readline("Please enter the URL to download geonames from")
            conf$geonames_url <- gurl
            save_config()
        }
        # Assembling geonames
        pause(silent)
        if(main_ans == "2" || defaults[["assemble_geonames"]])
           default_assemble_geo <- "1"
        else
           default_assemble_geo <- "2"
        is_silent <- silent | (auto && default_assemble_geo == "2")
        ass_geo_ans = choose_between("Do you want to assemble geonames?", 
            list(
                "1"="yes", 
                "2"="no"
            ), 
            is_silent,  
            default_assemble_geo
        )
        ass_geonames <- (ass_geo_ans %in% list(1))
        
        # Indexing geonames
        pause(silent)
        if(main_ans == "2" || defaults[["index_geonames"]])
           default_index_geo <- "1"
        else
           default_index_geo <- "2"
        is_silent <- silent | (auto && default_index_geo == "2")
        index_geo_ans = choose_between("Do you want to index geonames?", 
            list(
                "1"="yes", 
                "2"="no"
            ), 
            is_silent,  
            default_index_geo
        )
        index_geonames <- (index_geo_ans %in% list(1))
        message("")
        
        # Checking if there are missing dependenies
        if(!get_geonames && !file.exists(get_geonames_txt_path()) && (ass_geonames || index_geonames)) {
          message(alert("Cannot proceed to process assemble or index geonames. It has not been downloaded or marked for download"))
        } else if(!tryCatch(check_java_deps(), warning=function(e) FALSE) && (ass_geonames || index_geonames)) {
          message(alert("Cannot proceed to process assemble or index geonames. Java dependencies are missing. Please choose to download them"))
        } else if(!tryCatch(check_winutils(), warning=function(e) FALSE) && (ass_geonames || index_geonames)) {
          message(alert("Cannot proceed to process assemble or index geonames. Winutils is missing. Please choose to download it"))
        } else if(!file.exists(get_geonames_parquet_path()) && (!ass_geonames && index_geonames)) {
          message(alert("Cannot proceed to process index geonames. You have to choose to assemble it before"))
        } else {
          update_geonames(download = get_geonames, assemble = ass_geonames, index = index_geonames) 
        }
    }
    ####################################################################################
    ### Option 5. Update languages  ####################################################
    ####################################################################################
    if(main_ans %in% list("1", "2", "5")) {
        pause(silent)
        # Defining the list of languages to target
        end_answer <- "1"
        lang_ans <- "0"
        lang_to_remove = list()
        lang_df <- get_available_languages()
        all_langs <- setNames(lang_df$Label, lang_df$Code)
        url_langs <- setNames(lang_df$Url, lang_df$Code)
        while(lang_ans != end_answer) {
            curr_langs <- paste(lapply(conf$languages, function(l) sprintf("%s (%s)", l$name, l$code)), collapse=", ")
            lang_list <- setNames(
                            lapply(conf$languages, function(l) l$name),
                            lapply(conf$languages, function(l) l$code)
            )
            is_silent <- silent | (auto)
            lang_ans <- choose_between(sprintf("Currently episomer is configured to use %s, do you want to change languages?", curr_langs), 
                list(
                    "1"="No changes in the language list", 
                    "2"="Add languages",
                    "3"="Remove languages"
                ), 
                is_silent,  
                end_answer
            )
            # Removing languages
            if(lang_ans == "3") {
                if(length(lang_list)>1) {
                    remove_this <- choose_between(sprintf("Which language do you want to remove?"), 
                        c(lang_list, list("0"="Cancel")),
                        silent,  
                       "0"
                    )
                    if(remove_this != "0") {
                        lang_to_remove = c(lang_to_remove, remove_this)
                        message(sprintf("Removing language files for %s", remove_this))
                        pause(silent)
                        remove_config_language(remove_this)
                        remove_language_files(remove_this)
                        save_config()
                    }
                } else {
                    message("Cannot remove the last language. Episomer need at least one language to properly run. Please choose another option")
                    pause(silent)
                }
            }
            # Adding languages
            if(lang_ans == "2") {
                add_this <- choose_between(sprintf("Which language do you want to add?"), 
                    c(all_langs, list("0"="Cancel")),
                    silent,  
                    "0"
                )
                if(add_this != "0") {
                    message(sprintf("Language %s marked for download", add_this))
                    pause(silent)
                    add_config_language(add_this, all_langs[[add_this]])  
                }
            }
        }
         
        # Downloading languages
        conf_langs <- unlist(lapply(conf$languages, `[[`, "code"))
        missing_langs <- conf_langs[!file.exists(unlist(lapply(conf$languages, `[[`, "vectors")))] 
        do_nothing_index <- as.character(3+length(conf_langs)) 
        do_nothing_item = list()
        do_nothing_item[[do_nothing_index]] = "No languages to download"
        # Create languages folder if it does not exist
        if (!file.exists(file.path(conf$data_dir, "languages"))) {
            dir.create(file.path(conf$data_dir, "languages"))
        }

        if(main_ans == "2") 
           default_get_langs <- "3"
        else if(length(missing_langs) == 0)
           default_get_langs <- do_nothing_index
        else 
           default_get_langs <- "1"

        is_silent <- silent | (auto && default_get_langs == do_nothing_index)
        get_lang_ans = choose_between("Do you want to download languages? Which ones?", 
            c(
                list(
                    "1"=sprintf("Download missing languages (%s)", paste(missing_langs, collapse = ", ")), 
                    "2"=sprintf("Download all languages (%s)", paste(conf_langs, collapse = ", "))
                ),
                setNames(sapply(conf$languages, function(l) sprintf("Download %s (%s) only", l$name, l$code)), as.character((1:length(conf_langs))+2)),
                do_nothing_item
            ),
            is_silent,  
            default_get_langs
        )
        langs_to_download <- (
            if(get_lang_ans == "1") missing_langs
            else if (get_lang_ans =="2") conf_langs
            else if (get_lang_ans == do_nothing_index) list()
            else conf_langs[[as.integer(get_lang_ans)-2]]
        )
        for(lang in conf$languages) {
            if(lang$code %in% langs_to_download) {
                message(sprintf("Downloading %s language, from %s to %s", lang$name, url_langs[[lang$code]], lang$vectors))
                download_big_file(url_langs[[lang$code]], lang$vectors)
            }
        }
        
        # Training language models
        pause(silent)
        should_train = get_lang_traing_need() || length(lang_to_remove)>0 
        if(main_ans == "2" || should_train)
           default_train_lang = "1"
        else
           default_train_lang = "2"
        is_silent <- silent | (auto && default_train_lang == "2")
        train_lang_ans = choose_between("Do you want to launch language model retrain?", 
            list(
                "1"="yes", 
                "2"="no"
            ), 
            is_silent,  
            default_train_lang
        )
        do_training <- (train_lang_ans %in% list(1))
        if(do_training) {
            if(!tryCatch(check_java_deps(), warning=function(e) FALSE)) {
              message(alert("Cannot proceed to process to train language models. Java dependencies are missing. Please choose to download them"))
            } else if(!tryCatch(check_winutils(), warning=function(e) FALSE)) {
              message(alert("Cannot proceed to process to train language models. Winutils is missing. Please choose to download it"))
            } else if(!tryCatch(check_geonames(), warning=function(e) FALSE)) {
              message(alert("Cannot proceed to process to train language models. Geonames has not been successfully assemebled and indexed, please choose to do it"))
            } else {
              pause(silent)
              message("Going to launch model retraining, starting the episomer database")
              # The retraining needs a running database, and that database must not be taken for
              # the real service by whatever waits for the real service to become healthy before
              # starting - `depends_on: service_healthy` on search and detect in
              # docker/compose.yml, a readiness probe in Kubernetes. So it is bound somewhere else,
              # and both halves of "somewhere else" matter:
              #
              #  * a port of its own, conf$fs_setup_port, which is what keeps it out of sight of a
              #    health check watching conf$fs_port;
              #  * conf$fs_setup_host, "localhost" by default, so that reaching it needs no name
              #    resolution, no published port and no routing. Deliberately not conf$fs_url,
              #    which names an address other machines must reach - the opposite requirement; see
              #    the paragraph on this pair in get_empty_config(). That is the half Kubernetes
              #    made necessary: a Service does not route
              #    to a pod that is not ready, so the fs pod cannot reach *itself* under its
              #    service name until it serves the port the readiness probe watches - which it
              #    will not do until this retraining is over. Loopback is pod-internal and has none
              #    of that.
              #
              # Both travel to the database as command line arguments and neither is written to
              # properties.json. What this replaces shifted conf$fs_port by 1000, saved it, and
              # undid it in the finally below: a process killed outright - SIGKILL, an OOM kill, an
              # evicted pod - never runs a finally, so the shifted port stayed in the shared
              # configuration file and the next start shifted it again (8080, 9080, 10080...).
              # Everything left to undo here is session state, which a dying process cannot leave
              # behind.
              #
              # The configuration is saved first because the JVM reads properties.json itself
              # (Settings.load(), scala/src/main/scala/Settings.scala) and dies on
              # java.io.FileNotFoundException when it is not there. On a fresh data directory it is
              # not: setup_config() at the top of setup() only *reads* it, and every other
              # save_config() above sits inside an "I typed a custom URL" branch that a silent run
              # never takes. The port shift this replaces happened to write the file on its way
              # past, so nothing else had to; that was luck, and it is now said out loud. Same
              # pairing as the service start of option 6 further down.
              save_config()
              tryCatch({
                  register_fs_runner_task(
                      wait_until_response = TRUE,
                      bind_host = conf$fs_setup_host,
                      bind_port = conf$fs_setup_port,
                      # Bounded, unlike the default: nothing here is interactive on a server, and a
                      # database that cannot bind its address never answers at all.
                      timeout = 900
                  )
                  message("Episomer database is running, proceeding with training")
                  update_languages()
                  stop_fs_runner_task()
                }
                , finally = {
                  clear_fs_endpoint_override()
                  remove_running_task_pid("fs") 
                }
              )
              message("Episomer language models indexed and retrained, stopping the episomer database")
           }
        }
        
    }
    ####################################################################################
    ### Option 6. Social media authentication  #########################################
    ####################################################################################
    if (main_ans == "6" || ((main_ans %in% list("1", "2")) && launch)) {
        repeat {
            pause(silent)
            # Building the options, three per social media. The menu is rebuilt at each iteration
            # because adding an instance changes the labels.
            options <- list()
            actions <- list()
            targets <- list()
            default_sm_answer <- NULL
            add_option <- function(action, target, label, is_default = FALSE) {
                ie <- as.character(length(options) + 1)
                options[[ie]] <<- label
                actions[[ie]] <<- action
                targets[[ie]] <<- target
                if (is_default && is.null(default_sm_answer)) {
                    default_sm_answer <<- ie
                }
            }
            for (sm in social_media_families()) {
                instances <- sm_instances_of(sm)
                # dealing with activation of sm, which for a federated one gates all its instances
                if (is_sm_activated(sm)) {
                    add_option(
                        "deactivate",
                        sm,
                        sprintf("Deactivate %s (currently active)", sm)
                    )
                } else {
                    add_option(
                        "activate",
                        sm,
                        sprintf("Activate %s (currently inactive)", sm),
                        TRUE
                    )
                }
                # dealing with alerts of sm
                if (is_sm_with_alerts(sm)) {
                    add_option(
                        "exclude",
                        sm,
                        sprintf("Exclude %s in alerts (currently included)", sm)
                    )
                } else {
                    add_option(
                        "include",
                        sm,
                        sprintf(
                            "Include %s in alerts (currently excluded)",
                            sm
                        ),
                        TRUE
                    )
                }
                # dealing with authentication of sm: a federated social media is authenticated one
                # instance at a time, each with the access token it issued
                if (!sm_is_federated(sm)) {
                    authenticated <- is_sm_authenticated(sm)
                    add_option(
                        "credentials",
                        sm,
                        sprintf(
                            "Update %s credentials (%s)",
                            sm,
                            if (authenticated) {
                                "currently set"
                            } else {
                                "not properly set up"
                            }
                        ),
                        !authenticated
                    )
                } else {
                    add_option(
                        "add_instance",
                        sm,
                        sprintf(
                            "Add a %s instance or update its token (currently collecting from: %s)",
                            sm,
                            if (length(instances) > 0) {
                                paste(instances, collapse = ", ")
                            } else {
                                "none"
                            }
                        ),
                        is_sm_activated(sm) && length(instances) == 0
                    )
                    add_option(
                        "remove_instance",
                        sm,
                        sprintf(
                            "Remove a %s instance (currently collecting from: %s)",
                            sm,
                            if (length(instances) > 0) {
                                paste(instances, collapse = ", ")
                            } else {
                                "none"
                            }
                        ),
                        is_sm_activated(sm) && length(instances) == 0
                    )
                }
            }
            end_answer <- as.character(length(options) + 1)
            options[[end_answer]] <- "Do no changes in sm authentication"
            if (is.null(default_sm_answer)) {
                default_sm_answer <- end_answer
            }

            is_silent <- silent | (auto && default_sm_answer == end_answer)
            sm_ans <- choose_between(
                "Do you want to perform changes in social media activation / authentication",
                options,
                is_silent,
                default_sm_answer
            )
            # Performing requested action
            if (sm_ans == end_answer) {
                break
            }
            action <- actions[[sm_ans]]
            target <- targets[[sm_ans]]
            if (action == "activate") {
                sm_activate(target)
                # reloading so the networks now collected get their topics
                setup_config(save_properties_first = TRUE, fs_remote = NULL)
                save_config()
                message(sprintf("Social media %s has been activated", target))
            } else if (action == "deactivate") {
                sm_deactivate(target)
                if (length(active_social_media()) == 0) {
                    sm_activate(target)
                    message(alert(sprintf(
                        "At least one social media need to be active. Cannot deactivate %s",
                        target
                    )))
                } else {
                    save_config()
                    message(sprintf(
                        "Social media %s has been deactivated",
                        target
                    ))
                }
            } else if (action == "include") {
                sm_set_with_alerts(target)
                save_config()
                message(sprintf(
                    "Social media %s has been included in alert detection",
                    target
                ))
            } else if (action == "exclude") {
                sm_set_without_alerts(target)
                save_config()
                message(sprintf(
                    "Social media %s has been excluded of alert detection",
                    target
                ))
            } else if (action == "credentials") {
                sm_api_set_auth(target)
                message(sprintf(
                    "Credentials for social media %s has been updated",
                    target
                ))
            } else if (action == "add_instance") {
                instance <- trimws(readline(sprintf(
                    "Domain of the %s instance to collect from (e.g. mastodon.social):",
                    target
                )))
                if (nchar(instance) > 0) {
                    tryCatch(
                        {
                            # asks for the access token issued by that instance and stores it in
                            # the keyring, the instance itself goes to the configuration file
                            network <- sm_add_instance(target, instance)
                            sm_api_set_auth(network)
                            # reloading so the new network gets its own topics, then saving them
                            setup_config(save_properties_first = TRUE, fs_remote = NULL)
                            save_config()
                            message(sprintf(
                                "episomer will now collect posts from %s",
                                instance
                            ))
                        },
                        error = function(e) message(alert(conditionMessage(e)))
                    )
                }
            } else if (action == "remove_instance") {
                instance <- trimws(readline(sprintf(
                    "Domain of the %s instance to stop collecting from (e.g. mastodon.social):",
                    target
                )))
                if (nchar(instance) > 0) {
                    tryCatch(
                        {
                            sm_remove_instance(target, instance)
                            network <- paste(target, instance, sep = ".")
                            secret <- sm_secret_name(network, "token")
                            set_secret(secret = secret, value = "")
                            message(sprintf(
                                "Removing token for %s",
                                instance
                            ))
                            # reloading so the new network gets its own topics, then saving them
                            setup_config(save_properties_first = TRUE, fs_remote = NULL)
                            save_config()
                            message(sprintf(
                                "episomer stops collecting posts from %s",
                                instance
                            ))
                        },
                        error = function(e) message(alert(conditionMessage(e)))
                    )
                }
            }
        }
    }
    ####################################################################################
    ### Option 7. Runnin episomer services  ############################################
    ####################################################################################
    if (main_ans == "7" || ((main_ans %in% list("1", "2")) && launch)) {
        pause(silent)
        # Building options
        default_s_answer <- 1
        options = list("1"="Start all services")
        actions = list("1"="start")
        targets = list("1"="all")
        if(is_fs_running()) {
            options["2"] <- "Stop episomer database (currently running)"
            actions["2"] <- "stop"
            targets["2"] <- "fs"
        } else {
            options["2"] <- "Start episomer database (currently stopped)"
            actions["2"] <- "start"
            targets["2"] <- "fs"
        }
        # Deliberately the *local* status, not is_search_running() / is_detect_running(): the two
        # actions this menu offers - register_*_runner_task() and stop_*_runner_task() - start and
        # kill processes on this machine only. Labelling the entry "currently running" because
        # another machine of the cluster holds the lease would offer to stop something that is not
        # here. A local start refused by the lease still says so, in the log of the process it
        # spawns (register_runner() -> runner_lease_holder_message()).
        # fs keeps its /ping check below: it is not lease-based (plan section 6.2).
        #
        # search asks search_has_local_process() rather than is_runner_running_locally("search"),
        # because the parent loop and its per-network workers can outlive each other: search_loop()
        # blocks in parLapply() while the workers are R processes of their own (R/runners.R). A
        # parent killed by hand leaves them collecting, and reading the parent alone then offered
        # "Start search loop (currently stopped)" next to workers that were still running - which is
        # exactly the duplicate collection the lease exists to prevent. It is also the reader that
        # matches what the "stop" action does: stop_runner_task("search") kills the workers *and*
        # the parent.
        if(search_has_local_process()) {
            options["3"] <- "Stop search loop (currently running)"
            actions["3"] <- "stop"
            targets["3"] <- "search"
        } else {
            options["3"] <- "Start search loop (currently stopped)"
            actions["3"] <- "start"
            targets["3"] <- "search"
        }
        if(is_runner_running_locally("detect")) {
            options["4"] <- "Stop detect loop (currently running)"
            actions["4"] <- "stop"
            targets["4"] <- "detect"
        } else {
            options["4"] <- "Start detect loop (currently stopped)"
            actions["4"] <- "start"
            targets["4"] <- "detect"
        }
        options[["5"]] <- "Stop all services"
        actions[["5"]] <- "stop"
        targets[["5"]] <- "all"
        options[["6"]] <- "Perform no changes on services"
        actions[["6"]] <- ""
        targets[["6"]] <- ""
 
        is_silent <- silent | (auto && default_s_answer == "6")
        s_ans <- choose_between("Do you want to perform changes in social media activation / authentication", 
            options,
            is_silent,  
            default_s_answer
        )
        # Performing requested action
        if(s_ans != "6") {
            if(actions[[s_ans]] == "start") {
              existing_langs <- conf_langs[file.exists(unlist(lapply(conf$languages, `[[`, "vectors")))] 
              if(length(existing_langs)==0 ||  get_lang_traing_need()) {
                  message(alert("Cannot proceed starting services, languages needs to be successfully processed."))
              } else if(! tryCatch(check_java_deps(), warning=function(e) FALSE) || 
                        ! tryCatch(check_winutils(), warning=function(e) FALSE)) {
                  message(alert("Cannot proceed starting services, java dependencies has not been downloaded."))
              } else if(! tryCatch(check_geonames(), warning=function(e) FALSE)) {
                  message(alert("Cannot proceed starting services, geonames has not been successfully processed."))
              } else  {
                message("Starting services")
                save_config()
                if(targets[[s_ans]] %in% c("fs", "all")) {
                    register_fs_runner_task(wait_until_response=T)
                }
                if(targets[[s_ans]] %in% c("search", "all")) {
                    if(!tryCatch(check_sm_auth(), warning=function(e) FALSE)) {
                        message(alert("Cannot start the search service, The social media authentication is failing."))
                    }
                    else {
                        register_search_runner_task()
                    }
                }
                if(targets[[s_ans]] %in% c("detect", "all")) {
                     save_tasks(get_tasks())
                     register_detect_runner_task()
                }
                pause(silent)
                message("Service started")
              }
            }
            if(actions[[s_ans]] == "stop") {
                message("Stopping services")
                if(targets[[s_ans]] %in% c("detect", "all")) {
                    stop_detect_runner_task()
                }
                if(targets[[s_ans]] %in% c("search", "all")) {
                    stop_search_runner_task()
                }
                if(targets[[s_ans]] %in% c("fs", "all")) {
                    stop_fs_runner_task()
                }
                pause(silent)
                message("Service stopped")
            }
        }
    }
    ####################################################################################
    ### Option 8. Runnin the shiny app  ################################################
    ####################################################################################
    app <- NULL
    if (main_ans == "8" || ((main_ans %in% list("1", "2")) && launch)) {
        pause(silent)
        # Building options
        default_sh_answer <- "1"
        end_ans = "4"
        sh_ans <- choose_between("Do you want to open the shiny app", 
            list(
                "1"="Open the full shiny app",
                "2"="Open the dashboard only app",
                "3"="Open the admin only app",
                "4"="Do nothing"
            ),
            silent,  
            default_sh_answer
        )
        # Performing requested action
        if (sh_ans != end_ans && !is_fs_running()) {
            if (interactive()) {
                fs_for_shiny_ans <- choose_between(
                    "The episomer database must be started in order to run the shiny app - do you want to start it ?",
                    list(
                        "1" = "Yes",
                        "2" = "No"
                    ),
                    silent,
                    default = "1"
                )
                if (fs_for_shiny_ans == "1") {
                    message("Starting the episomer database...")
                    save_config()
                    register_fs_runner_task(wait_until_response = TRUE)
                } else {
                    stop(
                        "The episomer database must be started in order to run the shiny app"
                    )
                }
            } else {
                stop(
                    "The episomer database must be started in order to run the shiny app"
                )
            }
        }
        if (sh_ans != end_ans && !is_fs_running()) {
            # Naming the URL that did not answer is the whole point of this message: the database
            # may very well be running and simply not be reachable *from here* under the address
            # this machine resolved (see get_scala_api_endpoint()). Without the URL the wizard just
            # goes quiet and returns no app, which is indistinguishable from a Shiny failure.
            message(alert(sprintf(
                paste(
                    "Cannot launch the episomer app: the episomer database did not answer on %s.",
                    "If it runs somewhere else - a container, another machine - say where with the",
                    "EPI_HOST environment variable (e.g. EPI_HOST=http://localhost:%s) or by",
                    "calling episomer_app(fs_remote = ...) directly.",
                    sep = "\n"
                ),
                get_scala_ping_url(),
                conf$fs_port
            )))
        } else if (sh_ans == "1") {
            message("Opening the shiny app in full mode...")
            app <- episomer_app(data_dir = conf$data_dir)
        } else if (sh_ans == "2") {
            message("Opening the shiny app in dashboard...")
            app <- episomer_app(
                data_dir = conf$data_dir,
                profile = c("dashboard")
            )
        } else if (sh_ans == "3") {
            message("Opening the shiny app in admin...")
            app <- episomer_app(data_dir = conf$data_dir, profile = c("admin"))
        }
    }
    save_config()    
    message("config saved")
    message(alert("Thanks for using episomer!\nHappy signal detection!"))
    if(!is.null(app))
        app
    else
       invisible(NULL)
}

question <- function(txt) crayon::style(txt, crayon::combine_styles(crayon::bold, crayon::blue))
choice <- function(txt, bold = FALSE) {
    if(bold)
            crayon::style(txt, crayon::combine_styles(crayon::bold, crayon::green))
    else
            crayon::style(txt,  crayon::green)

}
alert <- function(txt) crayon::style(txt, crayon::combine_styles(crayon::bold, crayon::magenta, crayon::bgWhite))

pause <- function(silent=FALSE) { if(!silent) Sys.sleep(0.5)}

choose_between <- function(question, ops=list(), silent=FALSE, default=NULL) {
    if(length(options) > 0) {
        answered <- FALSE
        while (!answered) {
            pause(silent)
            message(question(sprintf("\n%s", question)))
            pause(silent)
            message <- paste(
                lapply(names(ops), function(k) {
                    choice(
                        sprintf(
                            ".... %s <- %s%s",
                            k,
                            ops[[k]],
                            ifelse(
                                !is.null(default) && k == default,
                                " (default \u21b5 )",
                                ""
                            )
                        ),
                        k == default
                    )
                }),
                collapse = "\n"
            )
            if (silent) {
                message(message)
                message(sprintf("Chosing default: %s", default))
                return(default)
            }
            message_end <- sprintf(
                "your choice%s",
                ifelse(!is.null(default), " (or \u21b5 ):", ":")
            )
            message(message)
            cat(message_end)
            ans <- readline()
            if (!is.null(default) && ans == "") {
                message(sprintf("%s %s", message_end, default))
                return(default)
            }
            answered <- exists(ans, where = ops)
            if (!answered) {
                pause()
                message(alert(sprintf(
                    "\n\nsorry, %s is not among the available options, \nplease try again",
                    ans
                )))
            }
        }
        return(ans)
    }
}
