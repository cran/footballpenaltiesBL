penaltyPlot <-
function(player, goalkeeper = TRUE, exact = TRUE, pendat, includeMisses = TRUE, quarterlyAxis = TRUE,
  colours = NULL, main = NULL, sub = NULL, jitter = TRUE, yvalconst = FALSE)
  {
    pensel = pendat[findPlayerSet(player, goalkeeper, exact, pendat), ]
    if (!includeMisses) pensel = pensel[pensel$result == 'Tor' | pensel$result == 'gehalten', ]
    starthelp = as.Date(format(min(pensel$date), "%Y-%m-01"))
    dateseq = seq.Date(starthelp, max(pensel$date)+30, by = 'quarter')
    if (is.null(colours))
      colours = if(goalkeeper) c('green4', 'darkorange', 'red2') else c('red2', 'mediumblue', 'green4')
    ylim = if(yvalconst) c(-0.25, 0.25) else c(-0.25, 1.25)
    plot.new()
    plot.window(xlim = range(dateseq), ylim = ylim)
    if (quarterlyAxis) axis.Date(1, at = dateseq, las = 2, format = '%m %Y')
      else axis.Date(1, x = dateseq, las = 2, format = '%m %Y')
    if (!yvalconst) axis(2, at = c(0, 0.5, 1), labels = c('Save', 'Miss', 'Goal'), las = 1)
    if (yvalconst) legend('top', c('Save', 'Miss', 'Goal'), col = colours, pch = 19, ncol = 3)
    yfact = ifelse(pensel$result == 'Tor', 3, ifelse(pensel$result == 'gehalten', 1, 2))
    if (yvalconst) yval = rep(0, length(yfact)) else yval = (yfact-1) / 2
    if (!includeMisses) yval[yfact == 2] = NA
    if (jitter) yval = yval + runif(length(yval), min = -0.1, max = 0.1)
    points(pensel$date, yval, pch = 19, col = colours[yfact])
    titlestring = if (!is.null(main)) main else 
      ifelse(goalkeeper, paste0('Penaltes against ', pensel$goalkeeper[1]),
        paste0('Penalties by ', pensel$penaltytaker[1]))
    substring = if (!is.null(sub)) sub else ifelse(includeMisses, "including missed shots", "excluding missed shots")
    title(titlestring, sub = substring)
    box()
    invisible()
  }
