
# FTW Player Ratings ----

ratings_raw <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1WFfAL1ixsncQ46V0yLfE8pLovzSvjVg3hJhmJHTaUEw/edit?usp=sharing"
  )

ratings <- 
  ratings_raw |> 
  mutate(matchday = dense_rank(Date)) |> 
  relocate(matchday, .after = Date)

readr::write_csv(ratings, here::here("bvb", "ratings", "data", "ratings.csv"))

# FTW MOTM Awards & Nominations ----

motm_raw <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1IWNAjZ8Qt3Z6VasoXB4qrz44fifRk96NoJ20yJg1IO8/edit?usp=sharing"
  )

motm <- 
  motm_raw |> 
  mutate(matchday = dense_rank(Date)) |> 
  relocate(matchday, .after = Date)

readr::write_csv(motm, here::here("bvb", "ratings", "data", "motm.csv"))
