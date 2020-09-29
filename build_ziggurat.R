# Построить зиккурат
build_ziggurat <- function(n) {
  result <- matrix(1:1,nrow = n*2-1, ncol =  n*2-1)
  y <- 2
  z <- n*2-3
  while(y < n*2-1 && z > 0){
    transfer <- matrix(y:y,nrow = z, ncol =  z)
    result[1:nrow(transfer)+y-1, 1:ncol(transfer)+y-1] <-transfer
    y <- y+1
    z <- z-2
  }
  result
}
