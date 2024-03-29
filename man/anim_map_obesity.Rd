% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{anim_map_obesity}
\alias{anim_map_obesity}
\title{Creates animation object. Can be saved by using \code{gganimate::anim_save() }.}
\usage{
anim_map_obesity(map = "world", time = 20)
}
\arguments{
\item{map}{str | World or DK}

\item{time}{int | Time in second the gif should run for}
}
\value{
gif item
}
\description{
Creates animation object. Can be saved by using \code{gganimate::anim_save() }.
}
\examples{
gif <- anim_map_obesity(map = "world")

# Save gif
gganimate::anim_save(filename = "world.gif", animation = gif, path = "~/")
}
