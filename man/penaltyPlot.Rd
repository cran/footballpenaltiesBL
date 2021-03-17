\name{penaltyPlot}
\alias{penaltyPlot}
\title{
Plot an overview of all penalties taken by or against a men's Bundesliga player.
}
\description{
Search for a player -- either exact match or using fixed regular expressions -- and
create a plot of all penalties taken by or against this player as table with the date
on the x-axis and the result on the y-axis.
}
\usage{
penaltyPlot(player, goalkeeper = TRUE, exact = TRUE, pendat, includeMisses = TRUE,
  quarterlyAxis = TRUE, colours = NULL, main = NULL, sub = NULL, jitter = TRUE,
  yvalconst = FALSE)
}
\arguments{
  \item{player}{
Character vector of length 1 which contains the full name of a player in \code{pendat}
or a part thereof.
}
  \item{goalkeeper}{
Boolean, specifies whether \code{player} is searched for among goalkeepers (TRUE)
or penalty takers (FALSE).
}
  \item{exact}{
Boolean, specifies whether to look for an exact match. If set to FALSE,
\code{grep(player, fixed = TRUE)} is used to search for the player.
}
  \item{pendat}{
Name of a data set containing the penalties. The data set needs to follow the
format of \code{\link{penalties}}.
}
  \item{includeMisses}{
Boolean, indicates whether to include penalties that missed the goal without the
goalkeeper actively saving the ball.
}
  \item{quarterlyAxis}{
Boolean, to customise the x-axis. If TRUE, the beginning of every quarter of each year
will be included on the axis, if FALSE, only the beginning of each year will be marked.
}
  \item{colours}{
Vector of length 3 that indicate the colours for saves, misses, and goals. The default
value tries to reflect the relative success of each outcome for goalkeepers and penalty takers.
}
  \item{main}{
Main title of the plot. The default indicates who the penalties were taken by or against.
}
  \item{sub}{
Subtitle of the plot. The defaults indicates whether missed shots were included or not.
}
  \item{jitter}{
Boolean, should observations be jittered along the y-axis to avoid overlap?
}
  \item{yvalconst}{
Boolean, if TRUE, all penalties are plotted with a constant y-value (jitter may be added
afterwards if \code{jitter = TRUE}). If FALSE, saves, misses, and goals are plotted with
three different y-values (plus possible jitter). A legend is added to the top of the plot
in that case.
}
}
\value{
No value is returned, function is called for its side-effect of producing a plot.
}
\author{
Leo N. Geppert, with thanks to Marius Ötting for the inspiration.
}

\seealso{
penaltyPrint
}
\examples{
data(penalties)
# Basic goalkeeper plot with quarterly and yearly marks. Depending on the duration of the career,
# quarterly marks may be too cluttered.
penaltyPlot('Illgner', pendat = penalties, exact = FALSE)
penaltyPlot('Illgner', pendat = penalties, exact = FALSE, quarterly = FALSE)

# similar plot for a penalty taker
penaltyPlot('Polster', pendat = penalties, exact = FALSE, quarterly = FALSE, goalkeeper = FALSE)

# demonstrate the effect of keeping the value on the y-axis constant as well as of excluding misses
penaltyPlot('Neuer', pendat = penalties, exact = FALSE, quarterly = FALSE)
penaltyPlot('Neuer', pendat = penalties, exact = FALSE, quarterly = FALSE, yvalconst = TRUE)
penaltyPlot('Neuer', pendat = penalties, exact = FALSE, quarterly = FALSE, includeMisses = FALSE)
}
\keyword{hplot}
