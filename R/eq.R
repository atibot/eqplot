globalVariables(
  c(
    "YEAR",
    "MONTH",
    "STATE",
    "DAY",
    "LATITUDE",
    "LONGITUDE",
    "EQ_PRIMARY",
    "TOTAL_DEATHS",
    "DATE",
    "year",
    "month",
    "day",
    "BC"
  )
)
#' Package: \code{eqplot}
#'
#' This package will provide tools to visualize the earthquake dataset obtained
#' from the U.S. National Oceanographic and Atmospheric Administration (NOAA) on
#' significant earthquakes around the world. This dataset contains information
#' about 5,933 earthquakes over an approximately 4,000 year time span.
#'
#' @source Dataset NOAA Significant Earthquake Database
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#' National Geophysical Data Center / World Data Service (NGDC / WDS):
#' Significant Earthquake Database.
#' National Geophysical Data Center, NOAA. doi:10.7289 / V5TD9V7K

##############################################################################

#' Get BC date from mirrored AD date: \code{bc_date()}
#'
#' This sub function converts a mirrored AD date to BC date and internally used.
#'
#' @param ad A Date class of mirrored AD date , a positive Date type (e.g. "2001-01-01").
#'
#' @return A Date class of BC date, a negative Date type (e.g. "-2001-01-01")
#'  corresponding to the mirrored AD date
#'
#' @note As this is just an internal function to be used for the other functions,
#'  it is not exported.
#'
#' @importFrom lubridate ymd year day
#' @import dplyr
bc_date <- function(ad) {
  c <- lubridate::ymd("0000-01-01") #dummy day
  ad1 <-
    lubridate::ymd(formatC(lubridate::year(ad), width = 4, flag = 0) , truncated = 2)
  diffdays <- ad - ad1
  bc1 <- c - (ad1 - c) + 1
  bc1 <- dplyr::if_else (lubridate::day(bc1) == 2 , bc1 - 1, bc1)
  bc <- bc1 + diffdays
}
#' Get Date class date from chracter type date reprezentation: \code{eq_date}
#'
#' This function converts chracter type date to Date type date.
#'
#' @param x  A chracter string reprezenting date in the format of "yyyy-mm-dd"
#' or "-yyyy-mm-dd".
#'
#' @return  A Date class date corresponding to "yyyy-mm-dd" or "-yyyy-mm-dd".
#'
#' @note This can be used to specify the date of earthquake occurence
#' for the other functions` arguments.
#'
#' @importFrom lubridate ymd
#'
#' @examples
#' eq_date("2001-01-01")
#' eq_date("-2001-01-01")
#'
#'
#' @export
eq_date <- function(x){
  if(!grepl("^-", x)) return_date <-lubridate::ymd(x)
  else return_date <-  bc_date(lubridate::ymd(sub("^-", "", x)))
  return_date
}


#'Clean data function: \code{eq_clean_data()}
#'
#' This function takes raw NOAA data frame and returns a clean data frame.
#' The clean data frame should have the following:
#'   A date column created by uniting the year, month, day and converting it
#'    to the Date class,
#'    LATITUDE, LONGITUDE, EQ_PRIMARY(magnitude) and TOTAL_DEATHS columns
#'    converted to numeric class.
#'    The rows whose LATITUDE, LONGITUDE or EQ_PRIMARY is missing (NA) are
#'    removed.
#'
#' @param raw_df A data frame of raw data.
#'
#' @return A data frame of clean data
#'
#' @import dplyr
#' @importFrom tidyr unite
#' @importFrom lubridate ymd
#'
#' @examples \dontrun{
#' data(raw_df)
#' clean_df <- eq_clean_data(raw_df)
#' }
#'
#' @export
eq_clean_data <- function(raw_df) {
  #remove the rows that has YEAR NA if any
  clean_df <- raw_df %>% dplyr::filter(!is.na(YEAR)) %>%

    #Clean the YEAR, MONTH, DAY
    dplyr::mutate(
      BC = (YEAR < 0),
      #BC flag for BC year
      year = formatC(abs(YEAR), width = 4, flag = 0),
      #zero fill
      month = dplyr::if_else(is.na(MONTH), 01L, MONTH),
      #Assume NA = 01 for Month
      day = dplyr::if_else(is.na(DAY), 01L, DAY) #Assume NA = 01 for Day
    ) %>%

    #unite year, month and day to make a new "date" column as the Date class
    tidyr::unite(DATE, year, month, day, sep = "-") %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    dplyr::mutate(DATE = dplyr::if_else(BC == TRUE, bc_date(DATE), DATE)) %>% #convert to BC date
    dplyr::select(-BC) %>%

    #convert to numeric class
    dplyr::mutate(
      LATITUDE = as.numeric(LATITUDE),
      LONGITUDE = as.numeric(LONGITUDE),
      EQ_PRIMARY = as.numeric(EQ_PRIMARY),
      #magnitude
      TOTAL_DEATHS = as.numeric(TOTAL_DEATHS)
      #total deaths
    ) %>%
    dplyr::filter(!(is.na(LATITUDE) | is.na(LONGITUDE))) %>%
    #ignore the earthquakes whose epicenters are unkown
    dplyr::filter(!is.na(EQ_PRIMARY))
  #ignore the earthquakes whose magnitudes are unkown
}

#'Clean the LOCATION_NAME : \code{eq_location_clean()}
#'
#' This function cleans the LOCATION_NAME column by stripping out the country
#'  name (including the colon) and converts names to title case (as opposed to
#'  all caps).
#'
#' @param  x A chracter vector
#'
#' @return A character vector cleaned
#'
#' @importFrom stringr str_to_title
#'
#' @examples \dontrun{
#'  library(readr)
#'  library(dplyr)
#'  # Note that the raw data set has to be downloaded beforehand
#'  raw_df <- readr::read_delim("data/rawdata.txt", "\t")
#'  raw_df <- raw_df %>% dplyr::mutate(LOCATION_NAME = eq_location_clean(LOCATION_NAME))
#' }
#'
#' @export
eq_location_clean <- function(x) {
  x <- sub(";.*", "", x)                        #delete ";" and beyond
  x <- substr(x, regexpr(":", x) + 1, nchar(x)) #delete the country name and ":"
  x <- gsub(":", "/", x)                #temp change ":" to "/" for str_to_title
  x <- stringr::str_to_title(x)
  x <- gsub("/", ":", x)                # back to ":"
}

#'GeomTimeline: \code{geom_timeline()}
#'
#' This geom and geom_timeline function will plot a time line of earthquakes
#' ranging from xmin to xmax dates with a point for each earthquake.
#' Optional aesthetics include color, size, and alpha (for transparency).
#' The x aesthetic is a date and an optional y aesthetic is a factor indicating
#' some stratification in which case multiple time lines will be plotted for
#' each level of the factor (e.g. country).
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param xmin A Date class date which specifies the minimum timeline range.
#'        This is optional. If not specified, The minimum timeline range of the
#'        \code{data} is assumed.
#'
#' @param xmax A Date class date which specifies the maximum timeline range.
#'        This is optional. If not specified, The maximum timeline range of the
#'         \code{data} is assumed.
#'
#' @return  plot of a time line of earthquakes ranging from xmin to xmax dates
#'          with a point for each earthquake.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom grid gpar pointsGrob
#'
#' @examples \dontrun{
#' library(dplyr)
#' library(lubridate)
#' data(raw_df)
#' test_df <- raw_df %>% eq_clean_data() %>%
#' filter(COUNTRY == "TURKEY" | COUNTRY == "JAPAN") %>%
#' dplyr::filter(DATE >= eq_date("2000-01-01") & DATE <= eq_date("2017-01-01"))
#' }
#'
#' @examples \dontrun{
#' ggtimeline <- ggplot2::ggplot(test_df) +
#' geom_timeline(ggplot2::aes(
#'                            x = DATE,
#'                            y = COUNTRY,
#'                            size = EQ_PRIMARY,
#'                            fill = TOTAL_DEATHS)
#'                            ) +
#' eq_theme +
#' ggplot2::scale_size_continuous(name = "Richer scale value", breaks = c(0, 2, 4, 6, 8)) +
#' ggplot2::scale_fill_continuous(name = "# deaths") +
#' ggplot2::ggtitle("Earthquakes")
#'
#' ggtimeline
#' }
#'
#' @export
geom_timeline <-  function(mapping = NULL,
                           data = NULL,
                           stat =  "timeline",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           xmin = NULL,
                           xmax = NULL,
                           ...) {
  #browser()
  layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xmin = xmin, xmax = xmax, na.rm = na.rm, ...)
  )
}

StatTimeline <- ggplot2::ggproto(
  "StatTimeline",
  ggplot2::Stat,
  setup_params = function(data, params) {
    #browser()
    if (is.null(params$xmin))
      params$xmin <- as.Date(min(data$x), origin)
    if (is.null(params$xmax))
      params$xmax <- as.Date(max(data$x), origin)
    params
  },
  compute_group = function(data, scales, xmin, xmax) {
    #browser()
    if (!(is.Date(xmin) &
          is.Date(xmax)))
      stop ("xmin and xmax must be Date type")
    if (xmin > xmax)
      stop ("xmax must be greater than or equal to xmin")
    out_df <- data %>% dplyr::filter(x >= xmin & x <= xmax)
  },
  required_aes = c("x", "y")
)

## geom_timeline
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = c("x", "y", "size"),
  default_aes = ggplot2::aes(
    color = "blue",
    fill = "grey50" ,
    alpha = 0.3,
    size = 1,
    stroke = 1,
    shape = 21
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    #browser()
    coords <- coord$transform(data, panel_scales)

    grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      default.units = "native",
      size = grid::unit(coords$size / 3, "char"),
      pch = coords$shape,
      gp = grid::gpar(
        #col = coords$colour,
        fill = coords$fill,
        alpha = coords$alpha
      )
    )
  }
)




#' New theme for the timeline plot: \code{eq_theme}
#'
#' This theme is for the earthquake timeline plot
#'
#' @import ggplot2
#'
#' @export
eq_theme <-  ggplot2::theme_minimal() + ggplot2::theme(
  legend.position =  "bottom",
  axis.line.x  = ggplot2::element_line(linetype = "solid"),
  axis.ticks.x = ggplot2::element_line(linetype = "solid"),
  panel.grid.major.x = ggplot2::element_blank(),
  panel.grid.minor.x = ggplot2::element_blank()
)


#'GeomTimeline_label:  \code{geom_timeline_label()}
#'
#' This geom and geom_timeline_label function will add annotations to the
#' earthquake data. This geom adds a vertical line to each data point with
#' a text annotation (e.g. the location of the earthquake) attached to each
#' line. There should be an option to subset to n_max number of earthquakes,
#' where we take the n_max largest (by magnitude) earthquakes. Aesthetics are
#' x, which is the date of the earthquake and label which takes the column name
#' from which annotations will be obtained.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param n_max  A integer to subset to n_max number of earthquakes.
#'               This is optional. If not specified, 3 is assumed (default).
#' @param xmin A Date class date which specifies the minimum timeline range.
#'        This is optional. If not specified, the minimum timeline range of the
#'        \code{data} is assumed.
#' @param xmax A Date class date which specifies the maximum timeline range.
#'        This is optional. If not specified, the maximum timeline range of the
#'         \code{data} is assumed.
#'
#' @return  Adds a vertical line to each data point with a text annotation
#'          for \code{n_max} largest (by magnitude) earthquakes.
#'
#' @note This geom_timeline_label function should be added with (+) to the previous
#'       geom_timeline()  or ggplot2 object generated by geom_timeline() with the
#'       same timeline range as the previous geom_timeline(). Unfortunately at this
#'       version it is required for users to ensure the coincident of the timeline
#'       range.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom grid gpar segmentsGrob textGrob gTree gList
#'
#' @examples \dontrun{
#' ggtimeline + geom_timeline_label(ggplot2::aes(
#'                                   x = DATE,
#'                                   y = COUNTRY,
#'                                   mag = EQ_PRIMARY,
#'                                   label = LOCATION_NAME),
#'                                   n_max = 5)
#' }
#'
#' @export
geom_timeline_label <-  function(mapping = NULL,
                                 data = NULL,
                                 stat =  "timelinelabel",
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 xmin = NULL,
                                 xmax = NULL,
                                 n_max = 3,
                                 ...) {
  #browser()
  layer(
    geom = GeomTimeline_label,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmin = xmin,
      xmax = xmax,
      n_max = n_max,
      na.rm = na.rm,
      ...
    )
  )
}

## stat_temp_label
StatTimelinelabel <- ggplot2::ggproto(
  "StatTimeline_label",
  ggplot2::Stat,
  setup_params = function(data, params) {
    #browser()
    if (is.null(params$xmin))
      params$xmin <- as.Date(min(data$x), origin)
    if (is.null(params$xmax))
      params$xmax <- as.Date(max(data$x), origin)
    params
  },
  compute_group = function(data, scales, xmin, xmax, n_max) {
    #browser()
    if (!(is.Date(xmin) &
          is.Date(xmax)))
      stop ("xmin and xmax must be Date type")
    if (xmin > xmax)
      stop ("xmax must be greater than or equal to xmin")

    out_df <- data %>%
      dplyr::filter(x >= xmin & x <= xmax) %>%
      dplyr::top_n(n_max, mag)
    #browser()
    out_df
  },

  required_aes = c("x", "y",  "mag")
)

GeomTimeline_label <- ggplot2::ggproto(
  "GeomTimeline_label",
  ggplot2::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(size = 1),
  draw_key = ggplot2::draw_key_abline,
  draw_panel = function(data, panel_scales, coord) {
    #browser()
    coords <- coord$transform(data, panel_scales)
    vline <- grid::segmentsGrob(
      x0 = coords$x,
      y0 = coords$y,
      x1 = coords$x,
      y1 = coords$y + 0.2,
      default.units = "native",
      arrow = NULL,
      name = NULL,
      gp = grid::gpar(lwd = 0.001, col = "grey"),
      vp = NULL
    )
    annotation <- grid::textGrob(
      label = coords$label,
      x = coords$x,
      y = coords$y + 0.2,
      just = "left",
      hjust = NULL,
      vjust = NULL,
      rot = 30,
      check.overlap = FALSE,
      default.units = "native",
      name = NULL,
      gp = grid::gpar(fontsize = 8),
      vp = NULL
    )
    #browser()
    grid::gTree(children = grid::gList(vline, annotation))
  }
)




#' Visualize earthquakes in space: \code{eq_map()}
#'
#' This function takes an argument data containing the filtered data frame
#' with earthquakes to visualize. The function maps the epicenters (LATITUDE
#' /LONGITUDE) and annotates each point with in pop up window containing
#' annotation data stored in a column of the data frame. The user should be
#' able to choose which column is used for the annotation in the pop-up with
#' a function argument named \code{annot_col}. Each earthquake should be shown with
#' a circle, and the radius of the circle should be proportional to the
#' earthquake's magnitude (\code{EQ_PRIMARY}).
#'
#' @param data A data frame of the clean data
#'
#' @param annot_col A character string of a column name of the \code{data} for
#' annotation
#'
#' @param cluster A logical flag (deafult FALSE). If TRUE, ClusterOPtion is used.
#'
#' @return Visualization of earthquakes in space annotating each point with
#' in pop up window
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples \dontrun{
#' library(dplyr)
#' data(raw_df)
#' raw_df %>% eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")
#' }
#'
#' @export
eq_map <- function(data,
                   annot_col = "DATE",
                   cluster = FALSE) {
  #check and find the column required for annotation
  n <- 1
  coln <- colnames(data)
  for (n in 1:length(coln)) {
    if (coln[n] == annot_col)
      break
    n <- n + 1
  }
  if (n > length(coln)) {
    stop("The specified column not found")
  }
  colnames(data)[n] <- "tmp"

  #Cluster option
  ClusterOptions <- NULL
  if (cluster)
    ClusterOptions <- leaflet::markerClusterOptions()

  #map the epicenters (LATITUDE/LONGITUDE) and annotates each point
  #with in pop up window.
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      data = data,
      radius = ~ EQ_PRIMARY,
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      weight = 2,
      popup = ~ paste(tmp),
      clusterOptions = ClusterOptions
    )
}

#' Create annotation label: \code{eq_create_label()}
#'
#'  This function takes the dataset as an argument and creates an HTML label
#'  that can be used as the annotation text in the leaflet map. This function
#'  should put together a character string for each earthquake that will show
#'  the cleaned location (as cleaned by the \code{eq_location_clean()} function
#'  created above, the magnitude (EQ_PRIMARY), and the total number of deaths
#'  (\code{TOTAL_DEATHS}), with boldface labels for each ("Location", "Total
#'  deaths", and "Magnitude"). If an earthquake is missing values for any of
#'  these, both the label and the value should be skipped for that element of
#'  the tag.
#'
#' @param data A data frame of the clean data
#'
#' @return An HTML label annotating each point with the date(\code{DATE}), the
#' cleaned location name(\code{LOCATION_NAME}), the magnitude (\code{EQ_PRIMARY})
#' and the total number of deaths(\code{TOTAL_DEATHS}) in pop up window.
#'
#' @import dplyr
#'
#' @examples \dontrun{
#' library(dplyr)
#' data(raw_df)
#' raw_df %>% eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data) {
  date <- dplyr::if_else(
    !is.na(data$DATE),
    paste("<b>Date:</b>", data$DATE, "<br />"),
    ""
  )
  location <- dplyr::if_else(
    !is.na(data$LOCATION_NAME),
    paste("<b>Location:</b>", data$LOCATION_NAME, "<br />"),
    ""
  )
  magnitude <- dplyr::if_else(
    !is.na(data$EQ_PRIMARY),
    paste("<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />"),
    ""
  )
  deaths <- dplyr::if_else(
    !is.na(data$TOTAL_DEATHS),
    paste("<b>Total deaths:</b>", data$TOTAL_DEATHS, "<br />"),
    ""
  )
  paste(date, location, magnitude, deaths)
}
