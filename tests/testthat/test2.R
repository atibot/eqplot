context("Plot")

test_that("geom", {
  data(raw_df)
  test_df <-
    raw_df %>% dplyr::mutate(LOCATION_NAME = eq_location_clean(LOCATION_NAME)) %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "TURKEY" | COUNTRY == "JAPAN") %>%
    dplyr::filter(DATE >= eq_date("2000-01-01") &
                    DATE <= eq_date("2017-01-01"))
  ggtimeline <- ggplot2::ggplot(test_df)
  ggtimeline2 <- ggtimeline + geom_timeline(
      ggplot2::aes(
        x = DATE,
        y = COUNTRY,
        size = EQ_PRIMARY,
        fill = TOTAL_DEATHS
      )) +
        eq_theme +
        ggplot2::scale_size_continuous(name = "Richer scale value", breaks = c(0, 2, 4, 6, 8)) +
        ggplot2::scale_fill_continuous(name = "# deaths") +
        ggplot2::labs(title = "Earthquakes")
  ggtimeline3 <- ggtimeline2 + geom_timeline_label(
    ggplot2::aes(
      x = DATE,
      y = COUNTRY,
      mag = EQ_PRIMARY,
      label = LOCATION_NAME
    ),
    n_max = 5
  )
  expect_that(ggtimeline2, is_a("ggplot"))
  expect_that(ggtimeline3, is_a("ggplot"))
})
