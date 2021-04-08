
dswr.transfer <- function(client.side.variable = "", column = "", client.side.split = "")
{
  success <- dswr.check.param(client.side.variable, column, client.side.split)
  if(success)
  {
    dswr.split(client.side.variable, column, client.side.split)
  }
  return(success)
}


#'@name dswr.split
#'@title split a data frame into a subsets
#'@description This function split a data frame into subsets; one for each server.
dswr.split <- function(client.side.variable = "", column = "", client.side.split = "")
{

       # get data to be split
       env        <- globalenv()
       data       <- get(client.side.variable, envir = env)

       # split the data
       split.data <- split(data, data[[column]])

       # assign split data
       assign(client.side.split, split.data, envir = env)

       # check split data has been assigned and it is a list
       success <- exists(client.side.split, envir = env) & is.list(split.data)
   }
   return(success)
}

dswr.check.param <- function(client.side.variable = "", column, client.side.split = "")
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


  env  <- globalenv()

  # check arguments R object exists
  if (!exists(client.side.variable, envir = env))
  {
    stop("CLIENT::SHARING::ERR::203")
  }

  data <- get(client.side.variable, envir = env)
  if(!is.data.frame(data))
  {
    stop("CLIENT::SHARING::ERR::204")
  }

  # check column exists
  column.exixts <- column %in% names(data)

  if(!column.exists)
  {
    stop("CLIENT::SHARING::ERR::205")
  }

  success <- is.character(client.side.variable) &
             is.character(client.side.split) &
             is.character(column) &
             exists(client.side.variable, envir = env) &
             is.data.frame(data) &
             column.exixts
  return(success)
}
