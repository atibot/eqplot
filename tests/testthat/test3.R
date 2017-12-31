context("Map")

test_that("leaflet mapping", {
  data(raw_df)
  test_df <- raw_df %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
  map1 <- test_df %>% eq_map(annot_col = "DATE")
  map2 <- test_df %>% dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_that(map1, is_a("leaflet"))
  expect_that(map2, is_a("leaflet"))
})
