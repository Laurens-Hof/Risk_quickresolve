#clean environment
rm(list = ls())

#define parameters
sims <- 10000
attackers <- 6
defenders <- 3

#Define the function
resolveround <- function(attack, defense){
  attackdice <- min(3, attack)
  defensedice <- min(2, defense)
  
  attackrolls <- floor(runif(attackdice, min =1, max = 7))
  defenserolls <- floor(runif(defensedice, min = 1, max = 7))
  
  sort(defenserolls, decreasing = TRUE)
  sort(attackrolls, decreasing = TRUE)
  
  if(attackdice >= defensedice){
    for (i in 1:defensedice){
      if(attackrolls[i] > defenserolls[i]){defense <- defense -1}
      else {attack <- attack - 1}
    }
  }else{
    if(attackrolls > defenserolls[1]){defense <- defense - 1}
    else{attack <- attack -1}
  }
    
  if(attack == 0){return(-defense)}
  else if(defense == 0){return(attack)}
  else if(attack > 0 & defense > 0){resolveround(attack, defense)}
}

#run the simulation
results <- replicate(sims, resolveround(attackers, defenders))
hist(results,
     breaks = seq(-defenders - 0.5, attackers + 0.5, by = 1),
     freq = FALSE,
     main = "Units left",
     xlab = "Attackers minus defenders",
     ylab = "Probability")

