Don't read inst/schema/service_registry_design.md

Episomer works with 3 three different services : the search service for collecting social network messages, the database (fs) service for storing and aggregating messages and a scheduler service to manage dependencies and generating alerts.

All services and the Shiny app used to share a single data_folder containing all necessary data for episomer operation. Files can be accessed by many services but always there is only one service that can write on it.

Main architecture changes were made recently: 

Centralise all persistence in a single service for all file access (fs). 

- Can run any service on a separate host
- Each service is “stateless
- Each service registers and “talk” only with the persistence service.

## Context

Today there are R functions to know if a service is running and to allow an R process to be registered as such :

- to know if a service is running: is_xxxx_running (xxxx could be fs, search or detect). 
- to register as runner : register_runner(name) 
- a service can also be killed using kill_task()

## Objective

- Allow remote services to be registered through as runner using the fs_loop,  (register_xxxx ...) preventing multiple instances of the same runner.

- Allow remote services to monitor status of other services. (is_xxxx_running ...)

## Current behaviour

Each episomer service write a PID file in EPI_HOME to detect if the service is running. The shiny app uses these files to display the status of the services using the functions "is_search_running()" "is_detect_running()"... this only works if in a single machine setup.

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

### Already implemented

- Only the fs_loop is stateful and is launch in local mode (fs_remote = NULL) and access the data directly using R functions 

- Other services (search_loop, detect_loop and shiny app) are stateless and access data through conf$fs_remote endpoint

- This behaviour has been implemented as docker compose in the docker folder (compose.yml)

### What we can imagine

Let's say we are working with 3 differents machines:

- machine A : fs_loop and storage
- machine B: first instance of a search_loop
- machine C: potential second instance of a search_loop

A service running (e.g search_loop) generates at first a random token to identify it through an entire process. We could store in memory in "conf" object: conf$token_service = "search" and conf$token_value = "randomlygenerated_token".
When a service is running and allowed to work, it could write on a regular basis (to be defined), on the machine A using the scala API and for example write_json_atomic(). So machine A is aware of which machines are running what service.

The is_running() family of functions would have to be adapted by being able to read this file. We could decide if a service is running or not, by comparing the last modification of the file stored in machine A with a specific TTL (example 5 minutes).

Let's say machine C wants to start a search_loop or continue a search_loop. It must checks if it allowed to do so by reading the specific file on machine A and checking the last modification date:

- it was updated 30 seconds ago by machine C : ok it's me I can move on
- it was updated 10 minutes by machine C: ok it's me I can start again
- it was updated by machine B: OK I lost my spot, I must kill myself (see the kill_task functions)

so we need a new behaviour for `is_xxxx_running()`:

- Must check if a file telling that a service is alive is present on the fs_loop

New `register_runner()`:

- Must create a token in the machine in which the service is launched. I think the PID file must be kept in the machine in which a service is launched
- Must be shared to the fs_loop machine

Functions to be created (to be discussed): 

- is_it_me_currently_working : is my token equal to the token stored in the loop machine ?
- should_i_kill_myself: is someone has taken my place I should stop
- i_am_alive: update on the fs_loop machine the file with the service token value (if I am allowed to write because no other machine has been taken the spot)


When do we check if we are allowed to write on the file system ?
Just before writing to topics.json for the search_loop line 172 in R/search.R
and in tasks.json for the detect loop in R/scheduler.R see calls to save_tasks

Other stuff to have in mind, to be discussed later:

For the search_loop we could send the message that we are still alive at each API query
-> what about when we reach the max API rate and we must wait for x minutes ?

Help me designing the solution. Please be conscient that I'm not a specialist of data architecture and stuff. We dont implement anything so far.

Please discuss what I proposed. Help me thinking about the Github issues I must create and the checkbox of the different elements to be added. I would like to be able to write most of the code myself.

I want to write R code only.

Write in a file inst/schema/multi_services_plan.md