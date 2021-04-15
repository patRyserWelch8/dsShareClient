#'@name ds.assign.sharing.settings
#'@title assigns settings on each data servers.
#'@description assigns the initial settings required for sharing parametres or
#'encoded information.
#'@param datasources - a list of \code{DSConnection-class} objects obtained after login.
#'@export
ds.assign.sharing.settings <- function(datasources = NULL)
{
  successful <- FALSE
  if (!is.null(datasources))
  {
    outcome    <- dsConnectClient::ds.aggregate(expression = call("assignSharingSettingsDS"), error.stop = TRUE , datasources = datasources)
    outcome    <- dsConnectClient::ds.aggregate(expression = call("danger_options"), error.stop = TRUE , datasources = datasources)
    successful <- dssp.transform.outcome.to.logical(outcome)

    if (!successful)
    {
      stop("::ds.share.param::ERR:018")
    }
  }
  return(successful)
}
