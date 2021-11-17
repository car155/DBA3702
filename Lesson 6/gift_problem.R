library(dplyr)

gift_iteration = function(iter, guests=10) {
  touch = rep(F, guests+1)
  
  # host touches box
  touch[1] = T
  box_psn = 1

  while (sum(touch) < guests) {
    # seats are numbered clockwise
    # -1 moves box anticlockwise, 1 moves box clockwise
    
    # coin flip
    coin = sample(c(-1, 1), 1)
    # box moves
    box_psn = (box_psn - 1 + coin) %% (guests + 1) + 1
    # update touch
    touch[box_psn] = T
  }
  return(which(!touch))
}

n = 10000
simulation.outcome = data.frame(iter=1:n, result=sapply(1:n, gift_iteration))
distribution = simulation.outcome %>% group_by(result) %>% summarise(count = n())
best_seat = distribution[which.max(distribution$count),]$result
print(distribution)
print(best_seat)