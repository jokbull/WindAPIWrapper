#' @import WindR
#' @importFrom caTools base64encode
#' @import data.table
#' @importFrom magrittr %>%
#' @export %>%
NULL

## TODO :
# #1. filters -- nan/date/?
# 2. subscriber: R mode or KDB-TP mode?
# 3. utils: dateconvert, rename the colnames etc.
# *4. Improve the performance of cache function -- done
# *5. Add help for wsd/wsi/wss param lists. -- done
# 6. MongoDB? convert R object to JSON








.onAttach = function(libname, pkgname) {
  packageStartupMessage("Welcome to my WindAPI Wrapper.\nThe cache folder is c:/myCache")
}


.onLoad = function(libname, pkgname) {
  require(data.table)
  require(WindR)

  options("myCache"="c:/myCache")
  if (!dir.exists(getOption("myCache")))  dir.create(getOption("myCache"))
  invisible()
}

.onUnload = function(libname,pkgname) {
  if(w.isconnected()) w.stop()
  #detach(package:WindR)
  invisible()
}

# isConnection <- function()  { w.isconnected() }



#' connection
#'
#' @export
wind.start = function(){
  if (!w.isconnected()) w.start(showmenu = FALSE)
}



#' fetch data
#' @param command is a character with quote or without quote
#' @param param is a list or a string.
#' @return `$Data, in data.table format
#' @export
wind.fetchData = function(command, param, limit = 9,
                          readFlag=TRUE, writeFlag=TRUE,
                          filters=NULL, ...) {

  library(data.table)
    # deparse the command
  commandstr = match.arg(deparse(substitute(command)),c("wsd","wss","wst","wsi","wsq","wset","tdays"))
  commandstr = paste0("w.",commandstr)

  dots = list(...)
  #if (any(c("envir","dt") %in% names(dots))) stop("envir,dt is defined!!!")

  # param is a list or string
  if (is.list(param)) {
    for (x in names(dots)){
      #eval(parse(text=paste0(x,"<-dots$",x)))
      assign(x,get(x,dots),param)
    }

    # apistr = "do.call(commandstr,param)"
    apistr = paste0("cache(commandstr,param,",readFlag,",",writeFlag,")")

  } else if (is.character(param)) {
    # param = paste0(param,",' '")
    # apistr = paste0(commandstr,"(",param,")");
    stop("param should be a list now....")

  } else {
    stop("param should be a list or string")
  }

  # check the options

  switch(commandstr,
    w.wsd = {
      if (any(!c("codes","fields","beginTime","endTime") %in% names(param))) {
        stop("wsd param consist of codes, fields, beginTime, endTime and options")
      }
    },
    w.wset = {
      if (!"tablename" %in% names(param)) {
        stop("wset consists of tablename")
      }
    },
    w.wsi = {
      stopifnot(c("codes","fields","beginTime","endTime") %in% names(param))
      #       {
      #         stop("wsi param consist of codes, fields, beginTime, endTime and options")
      #       }
    },
    w.wsq = {
      stopifnot(c("codes","fields") %in% names(param))
    },
    w.wss = {
      if (any(!c("codes","fields") %in% names(param))) {
        stop("wss param consist of codes, fields, and options")
      }
    },
    w.wst = stopifnot(c("codes","fields","beginTime","endTime") %in% names(param))
  )


  wind_i = 1
  while(wind_i < limit) {
    out.Cache <- eval(parse(text=paste0("wind_out = ",apistr)))
    if(wind_out$ErrorCode==0) {
      break
    } else {
      message(sprintf('try %d times: error when getting %s, errorcode = %s, msg = %s',wind_i, apistr, wind_out$ErrorCode,wind_out$Data[1,2]))
    }
    rm(out.Cache)
    wind_i = wind_i + 1
    Sys.sleep(1)
  }
  setDT(wind_out$Data)

  dots$envir = new.env()
  wind_out$Data$CODE=param$codes
  dots$envir$data = wind_out$Data

  if(is.null(filters) || all(is.character(filters))) {
    if(length(filters))
      lapply(filters,function(f){
        do.call(f,dots)
      })
  }
  return(dots$envir$data)
}

#' @export
wind.subscribe = function(){}
