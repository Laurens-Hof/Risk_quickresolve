rm(list= ls())

#take in the number of attacking and defending units
defense <- readline(prompt = "Number of units defending:")
attack <- readline(prompt = "Number of units attacking:")
defenseairfields <- readline(prompt = 'Does the defender have an airfield? Type "yes" or "no"')
attackairfields <- readline(prompt = 'Does the attacker have airfields? Type "yes" or "no"')
#convert into integers
defense <- as.integer(defense)
attack <- as.integer(attack)
defenseairfields <- ifelse(defenseairfields == "yes", 1, 0)
attackairfields <- ifelse(attackairfields == "yes", 1, 0)


resolveround <- function(attack, defense){
  attackdice <- min(3 + attackairfields, attack)
  defensedice <- min(2 + defenseairfields, defense)
  
  attackrolls <- floor(runif(attackdice, min =1, max = 7))
  defenserolls <- floor(runif(defensedice, min = 1, max = 7))
  
  sort(defenserolls, decreasing = TRUE)
  sort(attackrolls, decreasing = TRUE)
  
  for (i in 1:defensedice){
    if(attackrolls[i] > defenserolls[i]){
      defense <- defense -1
    } else {
      attack <- attack - 1
    }
  }
  
  if(attack == 0){
    print(c("Defender wins. Units remaining:", defense))
  }else if (defense == 0){
    print(c("Attacker wins. Units remaining:", attack))
  }else if(attack > 0 & defense > 0){
    print(c("Attacking units remaining:", attack))
    print(c("Defending units remaining:", defense))
    continuation <- readline(prompt = 'Does the attacker want to continue? Type "yes" or "no"')
    if (continuation == "yes"){
      resolveround(attack, defense)
    }
  }
}

resolveround(attack, defense)