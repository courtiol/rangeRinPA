% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{plot_map_sampling}
\alias{plot_map_sampling}
\title{Plot the map of the sampling design}
\usage{
plot_map_sampling(data, proj = "+proj=moll", basesize = 7)
}
\arguments{
\item{data}{the complete dataset}

\item{proj}{the projection to be used (e.g. "+proj=moll" - the default)}

\item{basesize}{the value for the argument \code{base_size} used in \href{ggplot2::theme}{\code{ggplot2::theme()}}}
}
\description{
Plot the map of the sampling design
}
\examples{
plot_map_sampling(data_rangers_with_geo)

if(require(patchwork)) {
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=moll") +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=robin" ) +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=natearth2") +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=mbt_fps") +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=hammer") +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=wag1") +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=eqearth") +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=eck4") +
  plot_map_sampling(data_rangers_with_geo, proj = "+proj=boggs")
}

}
