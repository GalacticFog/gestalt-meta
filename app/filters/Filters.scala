package filters

import javax.inject.Inject

import play.api.http.HttpFilters

import play.filters.cors.CORSFilter


class Filters @Inject() (corsFilter: CORSFilter, log: LoggingFilter) extends HttpFilters {
  println("/////////////////////////// LOADED FILTERS ////////////////////////////////////")
  def filters = Seq(corsFilter, log)
}
