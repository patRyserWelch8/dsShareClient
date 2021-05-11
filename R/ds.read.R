#'@name  ds.read
#'@title retrieve encrypted data from datashield servers
#'@param data.from.server a list of encrypted data obtained from some dataSHIELD server
#'@param data.encrypted a character variable representing the name of the R object of encrypted data
#'@param data.held.in.server a character variable representing the name of the R object of the data on
#'@param no.rows a numerical variable representing to be transferred at each iteration.
#'@param client.side.variable a character variable representing the name of an R object
#'@param datasources  a list of connections to dataSHIELD servers
#'@export ds.read
ds.read <- function(data.from.server     = NULL,
                    data.encrypted       = NULL,
                    data.held.in.server  = "D",
                    no.rows              = 1000,
                    client.side.variable = NULL,
                    datasources          = NULL)
{
   success <- FALSE
   tryCatch(
      {success <- dstr.transfer(data.from.server,
                                data.encrypted,
                                data.held.in.server,
                                no.rows,
                                client.side.variable,
                                datasources)},
      #warning = function(warning) {ds.warning(ds.share.param, warning)},
      error = function(error) {dsConnectClient::ds.error(error)},
      finally = {return(success)})
}

#'@name dstr.transfer
#'@title obtain encrypted data from some dataSHIELD servers
#'@param data.from.server a list of encrypted data obtained from some dataSHIELD server
#'@param data.encrypted a character variable representing the name of the R object of encrypted data
#'@param data.held.in.server a character variable representing the name of the R object of the data on
#'@param no.rows a numerical variable representing to be transferred at each iteration.
#'@param client.side.variable a character variable representing the name of an R object
#'@param datasources  a list of connections to dataSHIELD servers
dstr.transfer <- function(data.from.server     = NULL,
                          data.encrypted       = NULL,
                          data.held.in.server  = "D",
                          no.rows              = 1000,
                          client.side.variable = NULL,
                          datasources          = NULL)
{
   #init variables
   success <- FALSE
   env     <- globalenv()

   #check the arguments are correct.....
   success <- dstr.check.param(data.from.server,
                               data.encrypted,
                               data.held.in.server,
                               no.rows,
                               client.side.variable,
                               datasources)
   # if arguments correct continue...
   if(success)
   {

      # assign on the server the sharing settings for the transfer....
      success <- ds.assign.sharing.settings(datasources = datasources)

      # if assignment successful continue
      if (success)
      {
         # check the data on the server are suitably encrypted - NEEDS REVIEWINNG
         success <- dstr.check.data.encrypted(data.from.server, data.encrypted, data.held.in.server, datasources)


         # if data is suitably encrypted continue
         if (success)
         {

            # create client-side R object for containing encrypted data
            assign(client.side.variable, data.frame(), envir = env)
            success <- exists(client.side.variable, envir = env)

            # if successfully create continue
            if(success)
            {
               # get data from the server
               success <- dstr.get.data.from.server(data.encrypted, no.rows, client.side.variable, datasources)
            }
         }
      }
   }

   return(success)
}

#'@name dstr.concatenate
#'@title concatenate encrypted data to a client-side dataframe
#'@param data.from.server a list of encrypted data obtained from some dataSHIELD server
#'@param client.side.variable a character variable representing the name of an R object
#'in the global environment.
dstr.concatenate <- function(data.from.server = list(), client.side.variable = NULL)
{

   #extract data from the structure sent from the server
   extracted.data <- lapply(data.from.server, dstr.extract.encrypted.data)



   # attach the sources to each matrix as last column
   sources        <- 1:length(extracted.data)
   extracted.data  <- lapply(sources,function(x,data){return(cbind(data[[x]],x))},data = extracted.data)


    # bind the matrices together
   extracted.data <- do.call(rbind, extracted.data)

   # convert matrix into a data frame
   extracted.data <- as.data.frame(extracted.data)



   # save data
   env        <- globalenv()
   data.saved <- get(client.side.variable, envir = env)


   data.saved <- rbind.data.frame(data.saved, extracted.data)

   assign(client.side.variable, data.saved, envir = env)
}


#'@name dstr.extract.encrypted.data
#'@title extract data from encrypted and encoded data.
#'@param data.from.server a list of encrypted data obtained from some dataSHIELD server
#'@return extracted data in a dataframe
#'in the global environment.
dstr.extract.encrypted.data <- function(data.from.server = list())
{
   # check the structure is as expected.
   field.names          <- names(data.from.server)
   expected.field.names <- c("header","payload","property.a","property.b","property.c","property.d")
   has.correct.field    <- all(expected.field.names %in% field.names)

   # continue if fields are correct
   if(has.correct.field)
   {
      data       <- data.from.server$payload
      no.columns <- data.from.server$property.b

      if(is.character(data) & is.numeric(no.columns))
      {
         # checks it can be converted to numerical values
         can.be.converted <- grepl('^-?[0-9.;e]+$', data)
         if(can.be.converted)
         {
            # split character string into a list of elements
            data.list       <- strsplit(data,";")
            if (length(data.list[[1]]) > 1)
            {
               # transform into a vector and remove potential blank caracters
               data.vector <- unlist(data.list)
               data.vector <- gsub(" ", "",data.vector)
               # compute no rows
               no.rows     <- length(data.vector)/no.columns

               # check it is not a scalar!
               if (no.rows > 1 & no.columns > 1)
               {
                  # transform vector as numeric values and then as a matrix
                  data.numeric    <- as.numeric(x = data.vector)
                  received.matrix <- matrix(data=data.numeric,nrow=no.rows, ncol= no.columns)
               }
            }
         }
      }
   }
   return(received.matrix)
}

#'@name dstr.get.data.from.server
#'@title read iteratively the data from the server
#'@param data.encrypted a character variable representing the name of the R object of encrypted data
#'@param no.rows a numerical variable representing to be transferred at each iteration.
#'@param client.side.variable a character variable representing the name of an R object
#'@param datasources  a list of connections to dataSHIELD servers
dstr.get.data.from.server <- function(data.encrypted = NULL, no.rows = 1000, client.side.variable = NULL, datasources = NULL)
{
   # init variable
   stop <- dstr.is.eof(data.encrypted, datasources)
   print("...")
   while(!stop)
   {
      print("...")
      data.from.server <- dstr.next(data.encrypted,no.rows, datasources)
      dstr.concatenate(data.from.server, client.side.variable)
      stop          <- dstr.is.eof(data.encrypted, datasources)
   }
   return(stop)
}

#'@title check arguments are correct
#'@description throw some errors or return a logical value
#'@param data.server a list of encrypted data obtained from some dataSHIELD server
#'@param data.encrypted a character variable representing the name of the R object of encrypted data
#'@param data.held.in.server a character variable representing the name of the R object of the data on
#'@param no.rows a numerical variable representing to be transferred at each iteration.
#'@param client.side.variable a character variable representing the name of an R object
#'@param datasources  a list of connections to dataSHIELD servers
#'@return TRUE - all correct data type. FALSE - if checks have passed, but a class type is not correct.
#'@note throws errors CLIENT:SHARE:ERR:100 to CLIENT:SHARE:ERR:103
dstr.check.param <- function(data.server = NULL,
                             data.encrypted = NULL,
                             data.held.in.server = "D",
                             no.rows = 1000,
                             client.side.variable = NULL,
                             datasources = NULL)
{
   env <- globalenv()

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

   if(!is.character(client.side.variable))
   {
      stop("CLIENT:SHARE:ERR:104")
   }

   if(!is.numeric(no.rows))
   {
      stop("CLIENT:SHARE:ERR:105")
   }

   if(no.rows <= 0)
   {
      stop("CLIENT:SHARE:ERR:106")
   }


   return(is.character(data.server) &
          is.character(data.encrypted) &
          is.character(data.held.in.server) &
          is.character(client.side.variable) &
          no.rows > 0 &
          is.list(datasources))
}


#'@title check the data has been suitably encrypted
#'@description call the server-side function \code{isDataEncodedDS} from the server package dsShareServer
#'@param data.server a list of encrypted data obtained from some dataSHIELD server
#'@param data.encrypted a character variable representing the name of the R object of encrypted data
#'@param data.held.in.server a character variable representing the name of the R object of the data on
#'@param datasources  a list of connections to dataSHIELD servers
#'@return TRUE data are apprpriately encrypted on every DataSHIELD server. Otherwise, FALSE
#'@notes Server errors thrown SERVER::ERR:SHARE::005 to SERVER::ERR:SHARE::007.
#'Server errors thrown SERVER::ERR::SHARING::001 to SERVER::ERR::SHARING::002, SERVER:ERR:021
dstr.check.data.encrypted <- function(data.server = NULL, data.encrypted = NULL, data.held.in.server = "D",datasources = NULL)
{
   expression <- call("isDataEncodedDS", data.server, data.encrypted)
   outcome    <- dsConnectClient::ds.aggregate(expression = expression, error.stop = TRUE, datasources = datasources)
   return(dssp.transform.outcome.to.logical(outcome))
}


#'@title indicates some encrypted data remains to be transferred.
#'@param data.encrypted a character variable representing the name of the R object of encrypted data
#'@param datasources  a list of connections to dataSHIELD servers
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
#'@param data.encrypted a character variable representing the name of the R object of encrypted data
#'@param no.rows a numerical variable representing to be transferred at each iteration.
#'@param datasources  a list of connections to dataSHIELD servers
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
