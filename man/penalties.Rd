\name{penalties}
\alias{penalties}
\docType{data}
\title{
Penalty Shots in the German Men's Bundesliga (Association Football)
}
\description{
This data set contains information on all penalty shot's taken in the German Men's Bundesliga from its inaugural season
1963/64 until the end of season 2016/17. 
}
\usage{data("penalties")}
\format{
  A data frame with 4599 observations on the following 15 variables.
  \describe{
    \item{\code{date}}{Date vector containing the date the penalty was taken.}
    \item{\code{season}}{Character vector containing the season the penalty was taken in.}
    \item{\code{matchday}}{Numeric vector that specifies the match day the penalty was taken on. Usually, seasons in
    	the men\'s bundesliga consist of 34 match days, but there have been some seasons with a varying number, 30 and 38.}
    \item{\code{goalkeeper}}{Character vector containing the name of the goalkeeper the penalty was taken against.}
    \item{\code{penaltytaker}}{Character vector containing the name of the player who took the penalty.}
    \item{\code{homegame}}{Numeric vector that specifies where the match was played, 1 indicates an away match for the
    	goalkeeper, 2 a home match. In many cases it is advisable to convert this variable to a factor.}
    \item{\code{result}}{Character vector that gives the result of the penalty in German. Possible values are
    	\itemize{
    	  \item \code{Tor}: Goal
    	  \item \code{gehalten}: Save (abbreviation for gehalten)
    	  \item \code{vorbei}: Miss (abbreviation for vorbei)
    	  \item \code{drüber}: Miss, too high (abbreviation for drüber)
      	  \item \code{Latte}: Miss, hit the crossbar (abbreviation for Latte)
    	  \item \code{Pfosten}: Miss, hit the post (abbreviation for Pfosten)}
        The difference between a save and a miss is that the goalkeeper actively stopped the ball for save, whereas
        the penalty taker missed the goal without the goalkeeper touching it in the latter case.
        
        The distinction between different types of misses is included for historical reasons only. Current policy is to
        distinguish between goals, saves, and misses only. Analyses that rely on \code{drüb}, \code{Latt} or \code{Pfos}
        are likely misleading.}
    \item{\code{minute}}{Numeric vector specifying the minute of the match the penalty was taken in.}
    \item{\code{goaldiff}}{Numeric vector that gives the goal difference before the penalty was taken. A positive number
    	indicates that the goalkeeper's club is in the lead.}
    \item{\code{gkclub}}{Character vector that specifies the goalkeeper's club}
    \item{\code{ptclub}}{Character vector that specifies the penalty taker's club}
    \item{\code{gkage}}{Numeric vector giving the goalkeeper's age in years at the time the penalty was given}
    \item{\code{ptage}}{Numeric vector giving the penalty taker's age in years at the time the penalty was given}
    \item{\code{gkexp}}{Numeric vector giving the goalkeeper's experience, measured in number of seasons.
    	0 stands for the debut season, from then on, 1 is added for every following season,
    	regardless of whether the player played in the Bundesliga or not.}
    \item{\code{ptexp}}{Numeric vector giving the penalty taker's experience, measured in number of seasons.
    	0 stands for the debut season, from then on, 1 is added for every following season,
    	regardless of whether the player played in the Bundesliga or not.}
  }
}

\source{
Updated regularly 
}
\examples{
data(penalties)
str(penalties)

penaltyPrint('Illgner', exact = FALSE, pendat = penalties)
penaltyPlot('Illgner', exact = FALSE, pendat = penalties)
}
\keyword{datasets}
