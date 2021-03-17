\name{penaltyPrint}
\alias{penaltyPrint}
\title{
Show a table of all penalties taken by or against a men's Bundesliga player.
}
\description{
Search for a player -- either exact match or using fixed regular expressions -- and
return all penalties taken by or against this player as table.
}
\usage{
penaltyPrint(player, goalkeeper = TRUE, exact = TRUE, pendat, includeMisses = TRUE)
}
\arguments{
  \item{player}{
Character vector of length 1 which contains the full name of a player in \code{pendat}
or a part thereof.
}
  \item{goalkeeper}{
Boolean, specifies whether \code{player} is searched for among goalkeepers (TRUE) or
penalty takers (FALSE).
}
  \item{exact}{
Boolean, specifies whether to look for an exact match. If set to FALSE,
\code{grep(player, fixed = TRUE)} is used to search for the player.
}
  \item{pendat}{
Name of a data set containing the penalties. The data set needs to follow the format
of \code{\link{penalties}}.
}
  \item{includeMisses}{
Boolean, indicates whether to include penalties that missed the goal without the
goalkeeper actively saving the ball.
}
}
\value{
Returns a \code{data.frame} that is a subset of \code{pendat}.
}
\author{
Leo N. Geppert
}

\seealso{
penaltyPlot
}
\examples{
data(penalties)
penaltyPrint('Illgner, Bodo', pendat = penalties)
penaltyPrint('Illgner', exact = FALSE, pendat = penalties)
}
\keyword{print}
