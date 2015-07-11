#' Plot method for optimization paths.
#'
#' Plot method for every type of optimization path, containing any numbers and
#' types of variables. For every iteration up to 4 types of plots can be generated:
#' One plot for the distribution of points in X and Y space respectively and plots
#' for the trend of specified X variables, Y variables and extra measures over the time. 
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param iters [\code{integer} | NULL]\cr
#'   Vector of iterations which should be plotted one after another. If \code{NULL},
#'   which is the default, only the last iteration is plotted. Iteration 0 plots
#'   all elements with dob = 0. Note that the plots for iteration i contains
#'   all observations alive in iteration i.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @template arg_opplotter_lims
#' @param title [\code{character(1)}]\cr
#'   Main title for the arranged plots, default is Optimization Path Plots. 
#' @param ... 
#'   Additional parameters for \code{\link{renderOptPathPlot}}.
#' @return NULL
#' @export
#' 
plotOptPath = function(op, iters, pause = TRUE, xlim = list(), ylim = list(), 
  title = "Optimization Path Plots", ...) {
  
  requirePackages("gridExtra", why = "plotOptPath")
  
  if (missing(iters))
    iters = max(getOptPathDOB(op))
  
  assertClass(op, "OptPath")
  assertIntegerish(iters, lower = 0L, upper = max(getOptPathDOB(op)), any.missing = FALSE)
  assertFlag(pause)
  assertCharacter(title, len = 1L)
  
  # Set and check x and y lims, if needed
  # Consider only points alive during at least 1 plotted iteration
  # Set and check x and y lims, if needed
  data = getAndSubsetPlotData(op, iters, ...)
  lims = getOptPathLims(xlim, ylim, data$op.x, data$op.y, iters, 0.05)
  xlim = lims$xlim
  ylim = lims$ylim
  

  # Helper to arrange plot via gridExtra and pause process
  arrangePlots = function(plots, iter, iters) {
    
    if (!is.null(plots$plot.x.over.time))
      plots$plot.x.over.time = do.call(gridExtra::arrangeGrob,
        c(plots$plot.x.over.time, ncol = 1L))
    
    if (!is.null(plots$plot.y.over.time))
      plots$plot.y.over.time = do.call(gridExtra::arrangeGrob,
        c(plots$plot.y.over.time, ncol = 1L))
    

    plot.top = Filter(Negate(is.null), list(plots$plot.x, plots$plot.y))
    plot.top =  do.call(gridExtra::arrangeGrob, c(plot.top, nrow = 1L))
    
    plot.bottom = Filter(Negate(is.null), list(plots$plot.x.over.time, plots$plot.y.over.time))
    
    if (length(plot.bottom) > 0) {
      plot.bottom =  do.call(gridExtra::arrangeGrob, c(plot.bottom, nrow = 1L))
      plots = list(plot.top, plot.bottom)
    } else {
      plots = list(plot.top)
    }
    
    

    
    do.call(gridExtra::grid.arrange, c(plots, ncol = 1L, main = title))
    if (pause && iter != getLast(iters)) {
      pause()
    }
  }
  
  # Get rendered data and plot it for every iteration
  for (iter in iters) {
    plots = renderOptPathPlot(op, iter = iter, xlim = xlim, ylim = ylim, ...)
    arrangePlots(plots, iter, iters)
  }
  
  return(invisible(NULL))
}

#' Plot an optimization path vs. the improvement in the performance measure.
#'
#' Plot the performance improvement each step in the optimization path achieves.
#' If no improvement was achieved for a particular step, the previous
#' performance will be assumed and a flat line plotted.
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param min.improvement \code{numeric(1)}\cr
#'   Minimum relative improvement over the previous step required to show a text
#'   label in the plot specifying the configuration change and improvement.
#' @param ...
#'   Additional parameters for \code{\link{ggplot2::annotate}}.
#' @return The ggplot2 object.
#' @export
#'
plotOptPathImprovements = function(opt.path, min.improvement = 0.01, ...) {
  assertClass(opt.path, "OptPath")
  assertNumeric(min.improvement, lower = 0L, upper = 1L)

  optfun = if (opt.path$minimize) { min } else { max }
  which.optfun = if (opt.path$minimize) { which.min } else { which.max }
  len = getOptPathLength(opt.path)

  df = data.frame(step = 1:len,
    target = sapply(1:len, function(i) {
      optfun(head(opt.path$env$path[[opt.path$y.names]], i))
    }))
  opt.path.pars = opt.path$env$path[,names(opt.path$par.set$pars),drop = FALSE]
  p = ggplot(df, aes(x = step, y = target)) +
    geom_step() +
    xlab("Step") +
    ylab(opt.path$y.names)
  best = opt.path.pars[1,,drop = FALSE]
  for(i in 2:len) {
    cur = opt.path.pars[i,,drop = FALSE]
    diff = df$target[i-1] - df$target[i]
    diff.rel = abs(diff/df$target[i-1])
    better = optfun(c(0, diff)) == 0
    if (better) {
      if (diff.rel > min.improvement) {
        perc = signif(diff.rel * 100, 2)
        ypos = df$target[i-1] - diff/2
        old = best[sapply(best != cur, isTRUE)]
        new = cur[sapply(best != cur, isTRUE)]
        label = paste(names(old),
          paste(paste(old, new, sep = " → "), " (", perc, "%)", sep = ""),
          sep = " = ", collapse = "\n")
        p = p + annotate("text", x = i, y = ypos, label = label, ...)
      }
      best = cur
    }
  }
  return(p)
}
