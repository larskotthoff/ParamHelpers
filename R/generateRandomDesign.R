#' @title Generates a random design for a parameter set.
#'
#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  numeric(vector)   \tab  \code{numeric}  \cr
#'  integer(vector)   \tab  \code{integer}  \cr
#'  discrete(vector)  \tab  \code{factor} (names of values = levels) \cr
#'  logical(vector)   \tab  \code{logical}
#' }
#' If you want to convert these, look at \code{\link[BBmisc]{convertDataFrameCols}}.
#' Dependent parameters whose constraints are unsatisfied generate \code{NA} entries in their
#' respective columns.
#'
#' The algorithm simply calls \code{\link{sampleValues}} and arranges the result in a data.frame.
#'
#' @template arg_gendes_n
#' @template arg_parset
#' @template arg_trafo
#' @template ret_gendes_df
#' @export
generateRandomDesign = function(n = 10L, par.set, trafo = FALSE) {

  z = doBasicGenDesignChecks(par.set)
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  des = sampleValues(par.set, n, discrete.names = TRUE, trafo = trafo)

  # FIXME: all next lines are sloooow in R I guess. C?
  elementsToDf = function(x)  {
    Map(function(p, v) {
      # blow up scalar NAs
      if (isScalarNA(v))
        v = as.data.frame(t(rep(v, p$len)))
      else
        as.data.frame(t(v))
    }, par.set$pars, x)
  }
  des = lapply(des, elementsToDf)
  des = lapply(des, do.call, what = cbind)
  des = do.call(rbind, des)
  setColNames(des, pids)
}





