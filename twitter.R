library(twitteR)

consumer_key <- "8dTgwJdjOhANKltWgCbtBBXgE"
consumer_secret <-"h4KF1eOtnD7B0godb6dUgoIxihL0vw7Ci0vtI2XR4vykm3VIxp"
access_token <- "21667606-tiATMDuhptO444vpmJcsiVj1A0PQQQgDKG8PWHfRG"
access_secret <- "eCwWBHiCL71qsyNZk0z3Q58qyXs1TGkomUlvRa2SkrNtm" 


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

rt <- searchTwitter('#fcbayern',
  "lang:de", geocode = lookup_coords("de"), n = 100
)

tw <-  searchTwitter('#fcbayern', n = 25)
d <-  twListToDF(tw)
  
d

