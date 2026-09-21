# episomer: Early Detection of Public Health Threats from Social Media Data

[episomer site](https://github.com/EU-ECDC/episomer)

[Report bug & issues](https://github.com/EU-ECDC/episomer/issues)

`{episomer}` has been created by building on `{epitweetr}` objectives and functionalities. The `{episomer}` package allows you to automatically monitor trends of social media posts by time, place and topic. This automated monitoring aims at early detecting public health threats through the detection of signals (e.g. an unusual increase in the number of posts for a specific time, place and topic). The `{episomer}` package was designed to focus on infectious diseases, and it can be extended to all hazards or other fields of study by modifying the topics and keywords.

The general principle behind `{episomer}` is that it collects posts and related metadata from social media APIs according to specified topics, and stores them in a database that can be used to calculate statistics or as a search engine. Bluesky and Mastodon are currently supported. `{episomer}` geolocalises the posts and extracts key words, URLs and hashtags from them; these are then aggregated by topic and geographical location. Next, a signal detection algorithm identifies the number of posts (by topic and geographical location) that exceeds what is expected for a given day. If the number of posts exceeds what is expected, `{episomer}` sends out email alerts to notify those who need to further investigate these signals following the epidemic intelligence processes (filtering, validation, analysis and preliminary assessment).

The package includes an interactive web application (Shiny app) with six pages: 

- dashboard page : visualise and explore posts

- alerts page : view the current alerts and train machine learning models for alert classification on user defined categories

- geotag page : evaluate the geolocation algorithm and provide annotations for improving its performance

- data protection page : search, anonymise and delete posts from the episomer database to support data deletion requests
 
- configuration page : change settings and check the status of the underlying processes
 
- troubleshoot page : automatic checks and hints for using `{episomer}` with all its functionalities 

On the dashboard, users can view the aggregated number of posts over time, the location of these posts on a map and different most frequent elements found in or extracted from these posts (words, hashtags, URLs, contexts and entities). These visualisations can be filtered by the topic, location and time period you are interested in. Other filters are available and include the possibility to adjust the time unit of the timeline, whether reposts/quotes should be included, what kind of geolocation types you are interested in, the sensitivity of the prediction interval for the signal detection, and the number of days used to calculate the threshold for signals. This information is also downloadable directly from this interface in the form of data, pictures, and/or reports.

## Documentation

The package ships with three vignettes, available in `episomer/vignettes/`:

- [**episomer: user documentation**](episomer/vignettes/episomer-vignette.Rmd) — the main user guide: installation, configuration, data collection, the Shiny app pages (dashboard, alerts, geotag, data protection, configuration, troubleshoot) and the signal detection algorithm.
- [**episomer: containers and multi-machine deployment**](episomer/vignettes/episomer-deployment.Rmd) — how to run `{episomer}` in Docker and how to split the collection, processing and app components across several machines.
- [**Add a new social media**](episomer/vignettes/add_new_social_media.Rmd) — how to extend `{episomer}` with an additional social media source.

Once the package is installed, the same vignettes are available from R:

```r
browseVignettes("episomer")
vignette("episomer-vignette", package = "episomer")
```

More information on the baseline methodology and assessment of the functionalities is available in the epitweetr peer-review publication ( https://doi.org/10.2807/1560-7917.ES.2022.27.39.2200177).
