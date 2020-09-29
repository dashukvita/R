# При помощи функции simulate_walk из предыдущего видео можно моделировать случайное блуждание на отрезке [lower, upper]. Это достаточно простой случай. Больший интерес представляют задачи с блужданием по плоскости, то есть в размерности 2.
# Random walk with absorption
simulate_walk <- function(lower = -10, upper = 10, n_max = 100, p = 0.01) {
  current_positionX <- 0
  current_positionY <- 0
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(list(status = "Absorbed", 
                                 position = 1, 
                                 steps = i));
    current_positionX <- current_positionX + rnorm(1)
    current_positionY <- current_positionY + rnorm(1)
    radius <- sqrt(current_positionX*current_positionX + current_positionY*current_positionY)
    if (radius > 6) return(list(status = "Out of cycle", 
                                position = 2, 
                                steps = i));
  }
  return(list(status = "Max steps reached", 
              position = 3, 
              steps = n_max));
}

# Simulate results
result <- replicate(100000, simulate_walk(), simplify = FALSE)
result <- data.frame(
  status = sapply(result, function(x) x$status),
  position = sapply(result, function(x) x$position),
  steps = sapply(result, function(x) x$steps)
)

# Inspect results
tapply(result$position, result$status, length)
