



#'@title check arguments are correct
#'@description throw some errors or return a logical value
#'@return TRUE - all correct data type. FALSE - if checks have passed, but a class type is not correct.
#'@note throws errors CLIENT:SHARE:ERR:100 to CLIENT:SHARE:ERR:103
dstr.check.param <- function(data.server = NULL, data.encrypted = NULL, data.held.in.server = "D",datasources = NULL)
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

   return(is.character(data.server) &
          is.character(data.encrypted) &
          is.character(data.held.in.server) &
          is.list(datasources))
}


#'@title check the data has been suitably encrypted
#'@description call the server-side function \code{isDataEncodedDS} from the server package dsShareServer
#'@return TRUE data are apprpriately encrypted on every DataSHIELD server. Otherwise, FALSE
#'@notes Server errors thrown SERVER::ERR:SHARE::005 to SERVER::ERR:SHARE::007.
#'Server errors thrown SERVER::ERR::SHARING::001 to SERVER::ERR::SHARING::002, SERVER:ERR:021
dsce.check.data.encrypted <- function(data.server = NULL, data.encrypted = NULL, data.held.in.server = "D",datasources = NULL)
{
   expression <- call("isDataEncodedDS", data.server, data.encrypted,data.held.in.server)
   outcome    <- dsConnectClient::ds.aggregate(expression = expression, error.stop = TRUE, datasources = datasources)
   return(dssp.transform.outcome.to.logical(outcome))
}


#'@title indicates some encrypted data remains to be transferred.
#'@return TRUE if all the data have been transferred in every server
#'@note Server errors thrown SERVER::ERR::SHARING::001, SERVER:ERR:021, SERVER:ERR:009
#'It is assumed the parameter is correct and have been checked with
#'\code{dstr.check.param}
dstr.is.eof <- function(data.encrypted = "", datasources = NULL)
{
   expression <- call("isEndOfDataDS",data.encrypted)
   outcome    <- dsConnectClient::ds.aggregate(expression = expression, error.stop = TRUE, datasources = datasources)
   return(dssp.transform.outcome.to.logical(outcome))
}

#'@title transfers encrypted data from the servers for computations
#'@return TRUE if all the data have been transferred without any issues.
#'@note Server errors thrown SERVER::ERR::SHARING::001, SERVER:ERR:002, SERVER:ERR:004, SERVER:ERR:021
#'It is assumed the parameter is correct and have been checked with \code{dstr.check.param}. It is also assumed
#'the data have been checked for encryption first with the function \code{dsce.check.data.encrypted}
dstr.next <- function(data.encrypted = "", no.rows = 10, datasources = NULL)
{
   expression <- call("nextDS", data.encrypted, no.rows)
   outcome    <- dsConnectClient::ds.aggregate(expression = expression, error.stop = TRUE, datasources = datasources)
   return(outcome)
}
