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
                     class.type.server     = "data.frame",
                     no.rows               = 1000,
                     client.side.variable  = NULL,
                     column.server         = NULL,
                     datasources           = NULL)
{
  success <- FALSE
  tryCatch(
    {success <- dswr.transfer(data.to.server        = data.to.server,
                              class.type.server     = class.type.server,
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
                          class.type.server    = "data.frame",
                          no.rows              = 1000,
                          client.side.variable = NULL,
                          column               = NULL,
                          client.side.split    = NULL,
                          datasources          = NULL)
{

  success <- dswr.check.param(data.to.server =  data.to.server,
                              class.type.server  = class.type.server,
                              no.rows = no.rows,
                              client.side.variable = client.side.variable,
                              column = column,
                              client.side.split = client.side.split,
                              datasources = datasources)

  if(success)
  {
    success <- dswr.split(client.side.variable, column, client.side.split, datasources)

    if(success)
    {
      success <- dswr.write(data.to.server,class.type.server, no.rows, client.side.split, datasources)
    }
  }
  return(success)
}

dswr.write <- function(data.to.server,class.type.server,  no.rows, client.side.split, datasources)
{
  outcome    <- FALSE
  success    <- FALSE
  # suggest first writing to the server
  is.new.var <- TRUE

  env <- globalenv()
  if(exists(client.side.split, envir = env))
  {
      # get the data from the global env
      data          <- get(client.side.split, envir = env)

      # update stopping criterion
      stop          <- all(lapply(data, nrow) == 0)
      print("The transfer of data between the client and the server has started. It may take a while")
      while(!stop)
      {
          print("...")
          # extract the data to be written to server  - format into a matrix
          data.to.write <- lapply(data, function(x){data.matrix(stats::na.omit(x[c(1:no.rows),c(1:ncol(x)-1)]))})


          #set the seeds
          set.seed(as.numeric(Sys.time()))




          # format data ready for process
          data.to.write <- lapply(data.to.write, function(x)
                                                 {dswr.encode.data(x, ncol(x),stats::runif(1,min=1e9,max =9e11))})



          #server call to write data ....[to do ....] if return false stop ....
          success <- dswr.send.data.to.server(data.to.server = data.to.server,
                                              class.type.server = class.type.server,
                                              data.to.write = data.to.write,
                                              is.new.var.server = is.new.var,
                                              datasources = datasources)

          if(success)
          {
            # remove data written data from data frame
            data  <- lapply(data,function(x){stats::na.omit(x[no.rows:nrow(x)+1,])})


            # update stopping criterion
            stop   <- all(lapply(data, nrow) == 0)

            # indicates it is not the first writing ...
            is.new.var  <- is.new.var & FALSE
          }
          else
          {
            stop   <- TRUE
          }
      }
      print("The transfer of data has ended.")
  }
  #TRUE if all data has been written to server. Otherwise FALSE.
  return(all(lapply(data, nrow) == 0))
}

# this function has been implemented using functionality of DSI prior this new feature
# https://datashield.discourse.group/t/how-to-send-10-messages-using-datashield-aggregate-to-10-servers-simultaneously/367/10
# calls concatDataToVariableDS <- function(data.written.to.server = "",
#                                          class.type             = "data.frame",
#                                          is.new.var = TRUE,
#                                          header = "",
#                                          payload = "",
#                                          property.a = 0,
#                                          property.b = 0,
#                                          property.c = 0.0,
#                                          property.d = 0.0)

dswr.send.data.to.server <- function(data.to.server,class.type.server, data.to.write = list(), is.new.var.server = TRUE, datasources)
{
  outcome <- FALSE

  # check the number of data sources and the list is not
  # empty
  if(length(datasources) <= length(data.to.write) &
     length(data.to.write) > 0)
  {
    # compute indices
    indices <- 1:length(datasources)


    # prepare each server call
    calls <- lapply(data.to.write, function(x){if(!is.null(x)){return(call("concatDataToVariableDS",
                                                     data.to.server,
                                                     class.type.server,
                                                     is.new.var.server,
                                                     x$header,
                                                     x$payload,
                                                     x$property.a,
                                                     x$property.b,
                                                     x$property.c,
                                                     x$property.d))}
                                                else
                                                {return(NULL)}})


    # server calls
    outcome <- lapply(indices,
                      function(x)
                      {
                        if(!is.null(calls[[x]])){return(dsConnectClient::ds.aggregate(calls[[x]], TRUE, TRUE, datasources[[x]]))}
                        else{return(TRUE)}
                      })
    # transform to one logical value

    outcome <- dssp.transform.outcome.to.logical(outcome)
  }

  return(outcome)
}

# encode the data to be written to the server.
# this is a standard apply between dsShareServer and dsShareClient
dswr.encode.data <- function(data.to.write, no.columns, index)
{

  if(nrow(data.to.write) > 0)
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
  }
  else
  {
    return.value <- NULL
  }

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
                             class.type.server     = "data.frame",
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

  if(no.rows <= 0 || no.rows > 10000)
  {
    stop("CLIENT::SHARING::ERR::214")
  }

  if(is.null(datasources) || !is.list(datasources))
  {
    stop("CLIENT:SHARE:ERR:103")
  }

  if(!is.character(class.type.server))
  {
    stop("CLIENT::SHARING::ERR::207")
  }

  if(!class.type.server %in% c("matrix", "data.frame"))
  {
    stop("CLIENT::SHARING::ERR::208")
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
    stop("CLIENT::SHARING::ERR::211")
  }

  #verify the factors are the same length as the servers connection
  length.factors <- length(levels(factor(data[[column]])))
  no.servers     <- length(datasources)

  if(length.factors != no.servers)
  {
    stop("CLIENT::SHARING::ERR::212")
  }

  success <- is.character(client.side.variable)               &
             is.character(data.to.server)                     &
             is.character(client.side.split)                  &
             is.character(column)                             &
             is.character(class.type.server)                  &
             class.type.server %in% c("matrix", "data.frame") &
             is.list(datasources)                             &
             is.numeric(no.rows)                              &
             exists(client.side.variable, envir = env)        &
             is.data.frame(data)                              &
             column.exists                                    &

             length.factors == no.servers

  return(success)
}
