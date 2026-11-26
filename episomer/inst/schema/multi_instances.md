J'ai détaillé dans episomer/inst/schema/services.excalidraw le fonctionnement des différents services principaux existant actuellement dans le projet episomer. 

Episomer works as a shiny app which is launched from an R session. From that shiny app the user can launch three necessary services to run the application: the search service for collecting messages, the database (fs) service for storing and aggregating messages and a scheduler service to manage dependencies and generating alerts.

All services and the Shiny app used to share a single data_folder containing all necessary data for episomer operation. Files can be accessed by many services but always there is only one service that can write on it.

Main architecture changes were made recently: 

Centralise all persistence in a single service for all file access (fs). 
- Can run any service on a separate host
- Each service is “stateless
- Each service registers and “talk” only with the persistence service.

## Context

Today there are R functions to know if a service is running and to allow an R process to be registered as such 
- to know if a service is running: is_xxxx_running (xxxx could be fs, search or detect). 
- to register as runner : register_runner(name) 

## Objective

- Allow remote services to be registered through as runner using the fs_loop,  (register_xxxx ...) preventing multiple instances of the same runner.

- Allow remote services to monitor status of other services. (is_xxxx_running ...)

## Current behaviour

each episomer service write a PID file in EPI_HOME to detect if the service is running. The shiny app uses these files to display the status of the services using the functions "is_search_running()" "is_detect_running()"... this only works if in a single machine setup.

runner processes depend on PID files read with get_running_task_pid and written with register_runner() (both in runners.R)

### Service communication, registration and monitoring through the persistence server

Currently services need to be launched using the ‘data folder’  as a parameter and setting PID files.

In the new version the workflow for launching a process would be:

- Launching fs loop: (passing data folder as a parameter)
    - The java subservice will deal with Lucene files
    - All the rest of the files will be served / updated by a R REST API

- Launching other services: (passing the fs loop API URL as a parameter defaulting to localhost:8080)
    - The fs loop will register the service and authorise it to continue if no other running service is already connected.
    - All file interactions will be performed via fs_loop R endpoint.

Shiny app will follow the same workflow as services

The persistence service will monitor the health status of all services


## Expected behavior

### already implemented

- Only the fs_loop is stateful and is launch in local mode (fs_remote = NULL) and access the data directly using R functions 

- Other services (search_loop, detect_loop and shiny app) are stateless and access data through conf$fs_remote endpoint

- This behaviour has been implemented as docker compose in the docker folder (compose.yml)

### to implement

Propose a solution to register and monitor services in the multi-instance architecture.

What I'm not sure is whether search_loop and detect_loop have write access to the machine in which fs_loop is running ?

Some important points are: 

**if a task is closed unexpectedly we should detect it, even if running in another machine**

**we throw an error if the service is already running elsewhere**

Help me designing the solution. Please be conscient that I'm not a specialist of data architecture and stuff. We dont implement anything so far