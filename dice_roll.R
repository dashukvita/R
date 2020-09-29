# Функция dice_roll(n) должна выдавать n независимых бросков игрального кубика. Допустимые значения в диапазоне от 1 до 6.
dice_roll <- function(n) {
  print(sample(1:6,n, replace=T))
}
z <- dice_roll(5)
