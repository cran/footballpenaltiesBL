penaltyPrint <-
function(player, goalkeeper = TRUE, exact = TRUE, pendat, includeMisses = TRUE)
  {
    pensel = pendat[findPlayerSet(player, goalkeeper, exact, pendat), ]
    if (!includeMisses) pensel = pensel[pensel$result == 'Tor' | pensel$result == 'gehalten', ]
    pensel
  }
