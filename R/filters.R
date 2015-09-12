#' filter 1: filter the Date > today or some date
#' @export
filter.date = function(envir, dt = Sys.Date(),...) {

  # envir$data is data.table, if not, convert it.
  if (!is.data.table(envir$data)) {
    if (is.data.frame(envir$data)) {
      setDT(envir$data)
    } else {
      stop("envir$data must be data.frame / data.table")
    }
  }

  # get the last trade date or delist_date, set dt to be minimum of the last day,today or some day.
  code = envir$data[1,CODE]
  lasttrade_date = wind.fetchData(wss,list(codes=code,fields="lasttrade_date,delist_date"))
  if ((lt <- lasttrade_date[1,LASTTRADE_DATE] + lasttrade_date[1,DELIST_DATE]) > 0) {
    dt = min(dt,as.Date(lt,origin="1900-01-01")-2)
  }

  # adjust the data
  assign("data",envir$data[DATETIME<=dt],envir=envir)
  invisible()
}

#' filter 2: filter the Date > today or some date
#' @export
filter.amt_nan = function(envir,...) {
  if ("AMT" %in% names(envir$data)) {
    assign("data",envir$data[!is.nan(AMT)],envir=envir)
  } else {
    stop("No AMT fields")
  }
  invisible()
}
