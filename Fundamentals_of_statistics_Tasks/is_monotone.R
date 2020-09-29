# Написать функцию проверяющую вектор на монотонность
is_monotone <- function(x) {
  all(c(x[-1]-x[-length(x)] >= 0)) || all(c(x[-1]-x[-length(x)] <= 0))
}
