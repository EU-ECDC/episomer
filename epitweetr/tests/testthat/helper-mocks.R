# Helper functions and mock data for testing

# Network status helpers
online <- function() {
  TRUE
}
offline <- function() {
  FALSE
}

# Mock post data
mock_post <- function(
  uri = "at://did:plc:test123/app.bsky.feed.post/abc123",
  cid = "cid123",
  handle = "test.bsky.app",
  did = "did:plc:test123",
  text = "Test post",
  created_at = "2023-01-01T00:00:00.000Z"
) {
  list(
    uri = uri,
    cid = cid,
    author = list(
      handle = handle,
      did = did,
      createdAt = created_at
    ),
    record = list(
      text = text,
      createdAt = created_at
    ),
    indexedAt = created_at
  )
}

# Mock search response
mock_search_response <- function(posts = list(mock_post()), cursor = NULL) {
  list(
    posts = posts,
    cursor = cursor
  )
}


# Helper to create multiple mock posts
create_mock_posts <- function(n = 5, base_text = "Test post") {
  lapply(1:n, function(i) {
    mock_post(
      uri = paste0("at://did:plc:test", i, "/app.bsky.feed.post/", i),
      cid = paste0("cid", i),
      handle = paste0("test", i, ".bsky.app"),
      did = paste0("did:plc:test", i),
      text = paste0(base_text, " ", i)
    )
  })
}

# Mock httr2 functions
mock_request <- function(url) {
  structure(list(url = url), class = "httr2_request")
}

mock_req_url_query <- function(req, ...) req

mock_req_headers <- function(req, ...) req

mock_req_body_json <- function(req, data) req

mock_req_perform <- function(req, mock_response = NULL) {
  if (is.null(mock_response)) {
    mock_response <- mock_search_response()
  }
  structure(
    list(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(mock_response))
    ),
    class = "httr2_response"
  )
}

mock_resp_check_status <- function(resp) resp

mock_resp_body_json <- function(resp, mock_data = NULL) {
  if (is.null(mock_data)) {
    mock_data <- mock_search_response()
  }
  mock_data
}

mock_last_response <- function(resp) resp
mock_resp_status <- function(resp) "200"

# Mock error response
mock_error_response <- function(
  status_code = 401,
  error_message = "HTTP 401 Unauthorized"
) {
  structure(
    list(
      status_code = status_code,
      body = charToRaw('{"error":"Unauthorized"}')
    ),
    class = "httr2_response"
  )
}

mock_error_resp_body_json <- function(resp) {
  stop("HTTP 401 Unauthorized")
}

# Mock error req_perform
mock_error_req_perform <- function(req) {
  mock_error_response()
}


## Mock data posts
example_post <- list(
  uri = "at://did:plc:k6vq7z2d5lyhhqnxs72tlchg/app.bsky.feed.post/3ltlzhcvjqc2g",
  cid = "bafyreia3ndujo2eoctbkhup3t2po2ucnidhgoctexu3hrghl4folrlkbqq",
  author = list(
    did = "did:plc:k6vq7z2d5lyhhqnxs72tlchg",
    handle = "egibertegoners.bsky.social",
    displayName = "Paying_attention_to_science",
    avatar = "https://cdn.bsky.app/img/avatar/plain/did:plc:k6vq7z2d5lyhhqnxs72tlchg/bafkreig6zutb4ursus7zc74dqwwpruzpcevosupggmfvctg3gxvor4jrim@jpeg",
    associated = list(
      chat = list(allowIncoming = "following"),
      activitySubscription = list(allowSubscriptions = "followers")
    ),
    viewer = list(muted = FALSE, blockedBy = FALSE),
    labels = list(
      list(
        src = "did:plc:k6vq7z2d5lyhhqnxs72tlchg",
        uri = "at://did:plc:k6vq7z2d5lyhhqnxs72tlchg/app.bsky.actor.profile/self",
        cid = "bafyreigamfrihqs462yaivhqh7fet4dra7gt2mt3gs23nu5qsqb4bdi2ve",
        val = "!no-unauthenticated",
        cts = "1970-01-01T00:00:00.000Z"
      )
    ),
    createdAt = "2023-09-06T21:52:30.540Z"
  ),
  record = list(
    `$type` = "app.bsky.feed.post",
    createdAt = "2025-07-10T09:31:10.092Z",
    embed = list(
      `$type` = "app.bsky.embed.external",
      external = list(
        description = "",
        title = " Übertragung » Influenza » Krankheiten » HNO-Ärzte-im-Netz » ",
        uri = "https://www.hno-aerzte-im-netz.de/krankheiten/influenza/uebertragung.html"
      )
    ),
    facets = list(
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Übertragung"
        )),
        index = list(byteEnd = 247L, byteStart = 234L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Influenza"
        )),
        index = list(byteEnd = 258L, byteStart = 248L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#link",
          uri = "https://www.hno-aerzte-im-netz.de/krankheiten/influenza/uebertragung.html"
        )),
        index = list(byteEnd = 301L, byteStart = 260L)
      )
    ),
    langs = list("de"),
    text = "Für alle, die derzeit akut respiratorisch erkrankt sind. Wenn Ärzt:innen besser informieren würden, wäre viel gewonnen. Maske tragen bei Arztbesuch sollte Standard sein. Bin seit einigen Tagen krank. Im Mai gegen Covid19 geimpft. #Übertragung #Influenza \nwww.hno-aerzte-im-netz.de/krankheiten/..."
  ),
  embed = list(
    `$type` = "app.bsky.embed.external#view",
    external = list(
      uri = "https://www.hno-aerzte-im-netz.de/krankheiten/influenza/uebertragung.html",
      title = " Übertragung » Influenza » Krankheiten » HNO-Ärzte-im-Netz » ",
      description = ""
    )
  ),
  replyCount = 0L,
  repostCount = 0L,
  likeCount = 0L,
  quoteCount = 0L,
  indexedAt = "2025-07-10T09:31:09.804Z",
  viewer = list(threadMuted = FALSE, embeddingDisabled = FALSE),
  labels = list()
)

post_with_2_langs <- list(
  uri = "at://did:plc:3lkp6xj3vtrwvtd3szj4x2hy/app.bsky.feed.post/3ltlufnhuuk2h",
  cid = "bafyreiefi4ltnpevbmnrrn2jvlte54ieohqkgij5f747azimpk4dcnlvdm",
  author = list(
    did = "did:plc:3lkp6xj3vtrwvtd3szj4x2hy",
    handle = "darlingultra.bsky.social",
    displayName = "Darling Ultra",
    avatar = "https://cdn.bsky.app/img/avatar/plain/did:plc:3lkp6xj3vtrwvtd3szj4x2hy/bafkreifoohwf4iogfj6jhdlmsanp4t54h4r5lucpwilnhkijyw4t54ij7q@jpeg",
    associated = list(
      chat = list(allowIncoming = "all"),
      activitySubscription = list(allowSubscriptions = "followers")
    ),
    viewer = list(muted = FALSE, blockedBy = FALSE),
    labels = list(
      list(
        src = "did:plc:3lkp6xj3vtrwvtd3szj4x2hy",
        uri = "at://did:plc:3lkp6xj3vtrwvtd3szj4x2hy/app.bsky.actor.profile/self",
        cid = "bafyreidsw2kkskgcu7yvrfcrgwg2rh6ednxluhekrf4oolsj3vfresk4ly",
        val = "!no-unauthenticated",
        cts = "1970-01-01T00:00:00.000Z"
      )
    ),
    createdAt = "2023-10-01T10:24:59.798Z"
  ),
  record = list(
    `$type` = "app.bsky.feed.post",
    createdAt = "2025-07-10T08:00:45.361Z",
    facets = list(
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Covidisnotover"
        )),
        index = list(byteEnd = 127L, byteStart = 112L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "maskup"
        )),
        index = list(byteEnd = 163L, byteStart = 156L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "cleanair"
        )),
        index = list(byteEnd = 177L, byteStart = 168L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Maskenpflicht"
        )),
        index = list(byteEnd = 212L, byteStart = 198L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Covid19"
        )),
        index = list(byteEnd = 261L, byteStart = 253L)
      )
    ),
    langs = list("aa", "de"),
    reply = list(
      parent = list(
        cid = "bafyreifrsnf4t7crbizk6oaiimaxw4afd72h4vo7pm7kjtcx7jt7lirzku",
        uri = "at://did:plc:vk2mooi24pafrjmhpg4ymrv3/app.bsky.feed.post/3ltlsgnaeem2m"
      ),
      root = list(
        cid = "bafyreifrsnf4t7crbizk6oaiimaxw4afd72h4vo7pm7kjtcx7jt7lirzku",
        uri = "at://did:plc:vk2mooi24pafrjmhpg4ymrv3/app.bsky.feed.post/3ltlsgnaeem2m"
      )
    ),
    text = "Man lernt daraus ,dass die Medien versagen. Sonst würde die Tagesschau ja mindestens 1x täglich berichten das #Covidisnotover und Prävention in Form von #maskup und #cleanair und am besten einer #Maskenpflicht in bestimmten Bereichen angesagt ist,da #Covid19 eine Multisystemerkrankung ist. \n!"
  ),
  replyCount = 0L,
  repostCount = 0L,
  likeCount = 5L,
  quoteCount = 0L,
  indexedAt = "2025-07-10T08:00:46.905Z",
  viewer = list(threadMuted = FALSE, embeddingDisabled = FALSE),
  labels = list()
)

post_without_hashtags <- list(
  uri = "at://did:plc:rb76ihywonygjlcgsvbtuv7r/app.bsky.feed.post/3ltjamhcygk2x",
  cid = "bafyreid566ziqizeoxkfbg7donazotglckdwnwqj2g2oqlvuoeuenc2bxe",
  author = list(
    did = "did:plc:rb76ihywonygjlcgsvbtuv7r",
    handle = "communityspeedw.bsky.social",
    displayName = "CSW Online",
    avatar = "https://cdn.bsky.app/img/avatar/plain/did:plc:rb76ihywonygjlcgsvbtuv7r/bafkreigglds67gvokutlplbjp4ob7i327ij4x7cioj5xt457ql6relf6ii@jpeg",
    associated = list(
      chat = list(allowIncoming = "following"),
      activitySubscription = list(allowSubscriptions = "followers")
    ),
    viewer = list(muted = FALSE, blockedBy = FALSE),
    labels = list(),
    createdAt = "2024-02-08T19:08:53.278Z"
  ),
  record = list(
    `$type` = "app.bsky.feed.post",
    createdAt = "2025-07-09T08:01:19+01:00",
    text = "The Covid19 lockdown flushed out a lot of excessive speeders. These antisocial drivers are a constant threat to other roadusers' safety. When traffic levels increase again, Speedwatch will continue to identify them."
  ),
  replyCount = 0L,
  repostCount = 0L,
  likeCount = 1L,
  quoteCount = 0L,
  indexedAt = "2025-07-09T07:01:19.603Z",
  viewer = list(threadMuted = FALSE, embeddingDisabled = FALSE),
  labels = list()
)


post_with_many_hashtags <- list(
  uri = "at://did:plc:csvpjaip4xymv6okp2heivh5/app.bsky.feed.post/3ltj5z2do3s26",
  cid = "bafyreiankiw3g2xdiiyypaa7gwxtskr7tpegh3wyketvyy5bzxbae7vr7u",
  author = list(
    did = "did:plc:csvpjaip4xymv6okp2heivh5",
    handle = "photofan9000.bsky.social",
    displayName = "AT",
    avatar = "https://cdn.bsky.app/img/avatar/plain/did:plc:csvpjaip4xymv6okp2heivh5/bafkreiempacplxkdrskvp36c5rluuy4qskq6nldtd4wuvkyq4iyalcydue@jpeg",
    associated = list(
      activitySubscription = list(allowSubscriptions = "followers")
    ),
    viewer = list(muted = FALSE, blockedBy = FALSE),
    labels = list(),
    createdAt = "2023-08-06T21:11:46.353Z"
  ),
  record = list(
    `$type` = "app.bsky.feed.post",
    createdAt = "2025-07-09T06:14:40.852Z",
    embed = list(
      `$type` = "app.bsky.embed.external",
      external = list(
        description = "Alt: a panda bear is sitting on a swing set in a park",
        thumb = list(
          `$type` = "blob",
          ref = list(
            `$link` = "bafkreig77j2lm5mbiynrdeitqohwoet3zrouaxbn7jlkgd3xqkzt6t6fse"
          ),
          mimeType = "image/jpeg",
          size = 181742L
        ),
        title = "a panda bear is sitting on a swing set in a park",
        uri = "https://media.tenor.com/4anmscU6_sIAAAAC/panda-panda-funny.gif?hh=498&ww=394"
      )
    ),
    facets = list(
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "WearAMask"
        )),
        index = list(byteEnd = 55L, byteStart = 45L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Covid19"
        )),
        index = list(byteEnd = 64L, byteStart = 56L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "CoronaVirus"
        )),
        index = list(byteEnd = 77L, byteStart = 65L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "PandemicPanda"
        )),
        index = list(byteEnd = 92L, byteStart = 78L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "WednesdayMorning"
        )),
        index = list(byteEnd = 110L, byteStart = 93L)
      )
    ),
    langs = list("en"),
    text = "Wednesday. Take care and stay safe.\n🐼😷\n#WearAMask #Covid19 #CoronaVirus #PandemicPanda #WednesdayMorning"
  ),
  embed = list(
    `$type` = "app.bsky.embed.external#view",
    external = list(
      uri = "https://media.tenor.com/4anmscU6_sIAAAAC/panda-panda-funny.gif?hh=498&ww=394",
      title = "a panda bear is sitting on a swing set in a park",
      description = "Alt: a panda bear is sitting on a swing set in a park",
      thumb = "https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:csvpjaip4xymv6okp2heivh5/bafkreig77j2lm5mbiynrdeitqohwoet3zrouaxbn7jlkgd3xqkzt6t6fse@jpeg"
    )
  ),
  replyCount = 0L,
  repostCount = 0L,
  likeCount = 8L,
  quoteCount = 0L,
  indexedAt = "2025-07-09T06:14:42.303Z",
  viewer = list(threadMuted = FALSE, embeddingDisabled = FALSE),
  labels = list()
)
