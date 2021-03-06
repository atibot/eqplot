% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq.R
\name{geom_timeline_label}
\alias{geom_timeline_label}
\title{GeomTimeline_label:  \code{geom_timeline_label()}}
\usage{
geom_timeline_label(mapping = NULL, data = NULL, stat = "timelinelabel",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, xmin = NULL, xmax = NULL, n_max = 3, ...)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link{aes}} or
\code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
   options:

   If \code{NULL}, the default, the data is inherited from the plot
   data as specified in the call to \code{\link{ggplot}}.

   A \code{data.frame}, or other object, will override the plot
   data. All objects will be fortified to produce a data frame. See
   \code{\link{fortify}} for which variables will be created.

   A \code{function} will be called with a single argument,
   the plot data. The return value must be a \code{data.frame.}, and
   will be used as the layer data.}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link{borders}}.}

\item{xmin}{A Date class date which specifies the minimum timeline range.
This is optional. If not specified, the minimum timeline range of the
\code{data} is assumed.}

\item{xmax}{A Date class date which specifies the maximum timeline range.
This is optional. If not specified, the maximum timeline range of the
 \code{data} is assumed.}

\item{n_max}{A integer to subset to n_max number of earthquakes.
This is optional. If not specified, 3 is assumed (default).}

\item{...}{other arguments passed on to \code{\link{layer}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{color = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}
}
\value{
Adds a vertical line to each data point with a text annotation
         for \code{n_max} largest (by magnitude) earthquakes.
}
\description{
This geom and geom_timeline_label function will add annotations to the
earthquake data. This geom adds a vertical line to each data point with
a text annotation (e.g. the location of the earthquake) attached to each
line. There should be an option to subset to n_max number of earthquakes,
where we take the n_max largest (by magnitude) earthquakes. Aesthetics are
x, which is the date of the earthquake and label which takes the column name
from which annotations will be obtained.
}
\note{
This geom_timeline_label function should be added with (+) to the previous
      geom_timeline()  or ggplot2 object generated by geom_timeline() with the
      same timeline range as the previous geom_timeline(). Unfortunately at this
      version it is required for users to ensure the coincident of the timeline
      range.
}
\examples{
\dontrun{
ggtimeline + geom_timeline_label(ggplot2::aes(
                                  x = DATE,
                                  y = COUNTRY,
                                  mag = EQ_PRIMARY,
                                  label = LOCATION_NAME),
                                  n_max = 5)
}

}
