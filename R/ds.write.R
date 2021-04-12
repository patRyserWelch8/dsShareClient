#'@name ds.write
#'@title write outcome of computations to some DataSHIELD server
#'@description. This client-side function writes some data to one or more DataSHIELD servers. The
#'function aims to sent some outcome of computations used in learning algorithms or situations where
#'some servers needs storing outcomes of computations.
#'
#'It  is inspired from the federated learning model in this publication [Federated Machine Learning: Concept
#' and applications](https://arxiv.org/pdf/1902.04885.pdf).
#' @param data.to.server - a character argument representing the name of new server variable
#' @param no.rows        - a numerical (integer) argument indicatind the maximum of rows used
#' in one transfer of data
#' @param client.side.variable - a character argument representing the name of client.side.variable
#' storing the outcome of the computations
#' @param column.server - a character argument representing the name of column used to indicate
#' the server origin
#' @param datasources  a list of connections to dataSHIELD servers
#' @export
ds.write <- function(data.to.server        = NULL,
                     no.rows               = 1000,
                     client.side.variable  = NULL,
                     column.server         = NULL,
                     datasources           = NULL)
{
  success <- FALSE
  tryCatch(
    {success <- dswr.transfer(data.to.server        = data.to.server,
                              no.rows               = no.rows,
                              client.side.variable  = client.side.variable,
                              column                = column.server,
                              client.side.split     = paste0(client.side.variable,"_split"),
                              datasources           = datasources)},
    #warning = function(warning) {ds.warning(ds.share.param, warning)},
    error = function(error) {dsConnectClient::ds.error(error)},
    finally = {return(success)})
}

#' @param data.to.server - a character argument representing the name of new server variable
#' @param no.rows        - a numerical (integer) argument indicatind the maximum of rows used
#' in one transfer of data
#' @param client.side.variable - a character argument representing the name of client.side.variable
#' storing the outcome of the computations
#' @param client.side.split - a character argument representing the name of client.side.variable
#' storing the outcome of the computations during transfer
#' @param column  - a character argument representing the name of column used to indicate
#' the server origin
#' @param datasources  a list of connections to dataSHIELD servers
dswr.transfer <- function(data.to.server       = NULL,
                          no.rows              = 1000,
                          client.side.variable = NULL,
                          column               = NULL,
                          client.side.split    = NULL,
                          datasources          = NULL)
{

  success <- dswr.check.param(data.to.server =  data.to.server,
                              no.rows = no.rows,
                              client.side.variable = client.side.variable,
                              column = column,
                              client.side.split = client.side.split,
                              datasources = datasources)

  if(success)
  {
    success <- dswr.split(client.side.variable, column, client.side.split, datasources)

    dswr.write(data.to.server, no.rows, client.side.split, datasources)


  }
  return(success)
}

dswr.write <- function(data.to.server, no.rows, client.side.split,datasources)
{

  env <- globalenv()
  if(exists(client.side.split, envir = env))
  {
      # get the data from the global env
      data          <- get(client.side.split, envir = env)

      # update stopping criterion
      stop          <- all(lapply(data, nrow) == 0)
      while(!stop)
      {
          # extract the data to be written to server  - format into a matrix
          data.to.write <- lapply(data, function(x){data.matrix(na.omit(x[1:no.rows,1:ncol(x)-1]))})

          #set the seeds

          # format data ready for process
          data.to.write <- lapply(data.to.write, function(x){dswr.encode.data(x,
                                                                              ncol(x),
                                                                              runif(1,min=1e11, max =9e15))})


          #server call to write data ....[to do ....] if return false stop ....


          # remove data written data from data frame
          data          <- lapply(data,function(x){na.omit(x[no.rows+1:nrow(x)+1,])})

          # update stopping criterion
          stop      <- all(lapply(data, nrow) == 0)
      }

  }
}


dswr.encode.data <- function(data.to.write, no.columns, index)
{

  header        <- ""
  data          <- as.character(paste(as.numeric(data.to.write),sep="",collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  timestamp     <- as.numeric(Sys.time()) / size

  return.value  <- list(header = "FM1" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)

  return(return.value)
}

#'@name dswr.split
#'@title split a data frame into a subsets
#'@description This function split a data frame into subsets; one for each server.
#'@param client.side.variable - a character argument representing the name of client.side.variable
#' storing the outcome of the computations
dswr.split <- function(client.side.variable = "", column = "", client.side.split = "", datasources = NULL)
{
  success    <- FALSE
  # get data to be split
  env        <- globalenv()
  data       <- get(client.side.variable, envir = env)

  # split the data
  split.data <- split(data, data[[column]])

  # assign split data
  assign(client.side.split, split.data, envir = env)

  # check split data has been assigned and it is a list
  success <- exists(client.side.split, envir = env) & is.list(split.data)

  return(success)
}


#' @param data.to.server - a character argument representing the name of new server variable
#' @param no.rows        - a numerical (integer) argument indicatind the maximum of rows used
#' in one transfer of data
#' @param client.side.variable - a character argument representing the name of client.side.variable
#' storing the outcome of the computations
#' @param client.side.split - a character argument representing the name of client.side.variable
#' storing the outcome of the computations during transfer
#' @param datasources  a list of connections to dataSHIELD servers
dswr.check.param <- function(data.to.server        = NULL, # done
                             no.rows               = 1000, # done
                             client.side.variable  = NULL, # done
                             column                = NULL, # done
                             client.side.split     = NULL, # done
                             datasources           = datasources) # done
{
  success <- FALSE
  # check arguments have expected classes and type.
  if(!is.character(client.side.variable))
  {
    stop("CLIENT::SHARING::ERR::200")
  }

  if(!is.character(client.side.split))
  {
    stop("CLIENT::SHARING::ERR::201")
  }

  if(!is.character(column))
  {
    stop("CLIENT::SHARING::ERR::202")
  }


  if(!is.character(data.to.server))
  {
    stop("CLIENT::SHARING::ERR::203")
  }



  if(!is.numeric(no.rows))
  {
    stop("CLIENT::SHARING::ERR::204")
  }

  if(is.null(datasources) || !is.list(datasources))
  {
    stop("CLIENT:SHARE:ERR:103")
  }

  env  <- globalenv()

  # check arguments R object exists
  if (!exists(client.side.variable, envir = env))
  {
    stop("CLIENT::SHARING::ERR::205")
  }

  data <- get(client.side.variable, envir = env)
  if(!is.data.frame(data))
  {
    stop("CLIENT::SHARING::ERR::206")
  }


  # check column exists
  column.exists <- column %in% names(data)

  if(!column.exists)
  {
    stop("CLIENT::SHARING::ERR::205")
  }

  #verify the factors are the same length as the servers connection
  length.factors <- length(levels(factor(data[[column]])))
  no.servers     <- length(datasources)

  if(length.factors != no.servers)
  {
    stop("CLIENT::SHARING::ERR::207")
  }

  success <- is.character(client.side.variable)        &
             is.character(data.to.server)              &
             is.character(client.side.split)           &
             is.character(column)                      &
             is.list(datasources)                      &
             is.numeric(no.rows)                       &
             exists(client.side.variable, envir = env) &
             is.data.frame(data)                       &
             column.exists                             &
             length.factors == no.servers

  return(success)
}
