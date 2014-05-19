#' Return number of parameters in set.
#'
#' Either number of parameters or sum over parameter lengths.
#'
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param devectorize [\code{logical(1)}]\cr
#'   Sum over length of vector parameters?
#'   Default is code{FALSE}.
#' @return [\code{integer}].
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeDiscreteVectorParam("x", len = 2, values = c("a", "b"))
#' )
#' getParamNr(ps)
#' getParamNr(ps, devectorize = TRUE)
#' @export
getParamNr = function(par.set, devectorize = FALSE) {
  checkArg(par.set, "ParamSet")
  checkArg(devectorize, "logical", len = 1L, na.ok = FALSE)
  if (devectorize)
    sum(getParamLengths(par.set))
  else
    length(par.set$pars)
}