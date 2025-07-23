# Bluesky specific functions

Functions regarding the Bluesky data logic are prefixed with `bluesky-` in the `R/` and `tests/testthat/` folders.

All functions names are prefixed with `bluesky_`.

- `bluesky-api.R` contains the functions to interact with the Bluesky API.
- `bluesky-extract-elements.R` contains the functions to extract the relevant elements from the results from the Bluesky API and extract the elements from the posts.
- `bluesky-plans.R` contains the functions to the Bluesky specific logic to get, update and finish the plans.
  - `bluesky_get_plan_elements()` is used to get the elements from the plan.
  - `bluesky_update_plan()` is used to update the plan.
  - `bluesky_finish_plan()` is used to finish the plan.
  - `bluesky_new_plan_creation_based_on_previous_one_values()` is used to create a new plan if the previous one has passed with the required elements from the previous unfinished plan.
- `bluesky-search-topic.R` contains the functions to the Bluesky specific logic is the `search topic` function. We have specific function to assess "got_rows", what to do when no rows are found and how to get the first and last dates from the posts.
- `bluesky-session.R` contains the functions to create and check the validity of the Bluesky session.
- `bluesky-time-misc.R` contains the functions to format the dates and times.

**In the future, the same functionnalities implemented for other networks should be names the same way.**

# Plans logic

Plans functions are in the `R/plans.R` file.

Future plans must contain the name of the current network (i.e "bluesky" in our case), as it is used to identify the dedicated functions.

## get_plan()

The `get_plan()` function is used to create a new plan.

It uses elements that will be common to all plans independently of the network.

It also retrieved the network specific elements from the network specific function. For Bluesky it will call `bluesky_get_plan_elements()`.

## update_plans()

Specific operations were performed when a unfinished plan was found.

The specific logic is implemented in the `new_plan_creation_based_on_previous_one_values()` family of functions. For Bluesky it will call `bluesky_new_plan_creation_based_on_previous_one_values()`.

## finish_plans()

The `finish_plans()` function is used to finish the plans.

In an identific manner than the `get_plan()` function, it uses elements that will be common to all plans independently of the network. To get the specific logic to apply for the current network, it will call the `finish_plan()` family of functions. For Bluesky it will call `bluesky_finish_plan()`.

# Search messages

The functions to search messages are in the `R/search.R` file.

## search_loop()

The behaviour of `search_loop()` is the same as before, as now we have function able to deal with a specific network.

Nevertheless, it requires a "network" argument to be passed to the function.

Other parameters exists for the moment:

- `dev_in_progress`: a boolean to indicate if the function is running in development mode (it skips some parts that I am not sure about).
- `kill_after`: a numeric in seconds to kill the function after a certain time (used in my unit test to kill the function after x seconds).

## search_topic()

The `search_topic()` function is used to search for a topic on a specific network.

It will get the token using the `get_token()` family of functions. For Bluesky it will call `bluesky_get_token()`.

For Bluesky, I had to set `research_min_date` and `research_max_date` depending on the "boundaries" of the plan. It is done using the set_date_boundaries() family. For Bluesky it will call `bluesky_set_date_boundaries()`.

The function will then search for the topic using the `search_topic()` family of functions. For Bluesky it will call `bluesky_search_topic()`.
As it implemented at that moment, the search function will have 3 parameters:

- `plan`: the plan to search for.
- `query`: the query to search for.
- `token`: the token to use to search for the topic.

To evaluate if the search was successful, we have a function to assess "got_rows". For Bluesky it will call `bluesky_got_rows()`.

To get the first and last dates from the posts, we have a function to get the first and last dates from the posts. For Bluesky it will call `bluesky_get_first_and_last_dates_from_posts()`.

The plan is updated using the `update_plan()` family of functions. For Bluesky it will call `bluesky_update_plan()`.

If no rows were found, we have a function to deal with this case. For Bluesky it will call `bluesky_no_rows_logic()`.

# Unit test to run the loop

A specific unit test was created to run the loop.

It is located in the `tests/testthat/test-search_loop.R` file.

Please run `pkgload::load_all()` to load the package before running the unit test.

# To be modified

- boundaries_date_min: probably useless
