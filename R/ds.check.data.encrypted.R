

ds.check.data.encrypted <- function(data.server = NULL, data.encrypted = NULL, data.held.in.server = "D", datasources = NULL)
{
  success <- FALSE
  tryCatch(
    {success <- dsce.check.data.encrypted(data.server, data.encrypted, data.held.in.server, datasources)},
    error = function(error) {print(error);dsConnectClient::ds.error(error)},
    finally = {return(success)})
}

dsce.check.data.encrypted <- function(data.server = NULL, data.encrypted = NULL, data.held.in.server = "D",datasources = NULL)
{
   if(!is.character(data.server))
   {
     stop("CLIENT:SHARE:ERR:100")
   }

   if(!is.character(data.encrypted))
   {
     stop("CLIENT:SHARE:ERR:101")
   }

   if(!is.character(data.held.in.server))
   {
     stop("CLIENT:SHARE:ERR:102")
   }

   if(is.null(datasources) || !is.list(datasources))
   {
      stop("CLIENT:SHARE:ERR:103")
   }

   outcome <- dsce.server.call(data.server, data.encrypted, data.held.in.server, datasources)
   return(outcome)

}

dsce.server.call <- function(data.server, data.encrypted, data.held.in.server, datasources)
{
   outcome <- FALSE
   #isDataEncodedDS <- function(data.server = NULL, data.encoded = NULL, data.held.in.server = NULL)
   expression <- call("isDataEncodedDS", data.server, data.encrypted,data.held.in.server)
   outcome    <- dsConnectClient::ds.aggregate(expression = expression, error.stop = TRUE, datasources = datasources)
   return(dssp.transform.outcome.to.logical(outcome))
}
