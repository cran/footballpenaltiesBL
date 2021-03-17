findPlayerSet <-
function(player, goalkeeper = TRUE, exact = TRUE, pendat)
  {
    select = 
      if (goalkeeper & exact) which(pendat$goalkeeper == player)
      else if (goalkeeper & !exact) grep(player, pendat$goalkeeper, fixed = TRUE)
      else if (!goalkeeper & exact) which(pendat$penaltytaker == player)
      else grep(player, pendat$penaltytaker, fixed = TRUE)
    select
  }
