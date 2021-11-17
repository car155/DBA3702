iteration = function(iter) {
  doors = c("A", "B", "C")
  
  # Host select prize door
  prize_door = sample(doors, 1)
  
  # Player select door
  player_door = sample(doors, 1)
  
  # Host opens one door
  open_door = sample(doors[!(doors %in% c(prize_door, player_door))], 1)
  
  # Player win if swap
  swap_door = doors[!(doors %in% c(open_door, player_door))]
  return(swap_door == prize_door)
}

n = 1000
simulation.outcome = data.frame(iter=c(1:n), result=sapply(1:n, iteration))
prob = mean(simulation.outcome$result)
print(prob)

