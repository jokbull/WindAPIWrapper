#' Cache the functino results.
#'
#' @export
cache = function(func,param,readFlag=TRUE,writeFlag=TRUE){

  # func must be a character
  if (!is.character(func)) stop("the 1st argument func should be a character or function!" )

  # content file
  contentfile = file.path("c:/myCache",paste0(func,".RData"))
  if (file.exists(contentfile)) load(contentfile) else cacheContent = list()

  # text file
  key  = deparse(param,width.cutoff=500,nlines = 1)
  file = file.path("c:/myCache",paste0(encode(c(func,key)) ,".RData"))

  # read mode
  if (readFlag) {
      if ( key %in% names(cacheContent)) {
        load(file)
        if (key %in% names(cacheText)) {
          return(cacheText[[key]])
        } else {
          stop("cacheText doesn't have the key ",key )
        }
      }
  }


  # calculate
  result = do.call(func,param)

  # write mode
  if (writeFlag) {
    cacheText = list()
    cacheContent[[key]] = Sys.Date()
    cacheText[[key]] = result
    save(cacheContent,file=contentfile)
    save(cacheText,file=file)
  }
  return(result)
}

# convert func,key to the file name string
encode <- function(strings,collapse="|"){
  substr( base64encode(paste(strings,collapse = collapse)), 1L, 64L)
}

decode <- function(string,collapse="|") {
  unlist(strsplit(base64decode(string,"character"),split=collapse,fixed=TRUE))
}
