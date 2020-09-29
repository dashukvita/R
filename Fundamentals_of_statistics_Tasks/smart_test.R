# 1. Напишите функцию smart_test, которая получает на вход dataframe с двумя номинативными переменными с произвольным числом градаций. Функция должна проверять гипотезу о независимости этих двух переменных при помощи критерия хи - квадрат или точного критерия Фишера.

smart_test <-  function(x){
  x <- table(x)
  if (min(x) < 5) {
    result_list <- fisher.test(x)
    result_vec <- c(result_list$p.value)
  }
  else {
    result_list <- chisq.test(x)
    result_vec <- c(result_list$statistic[[1]], result_list$parameter[[1]], result_list$p.value[[1]])
  }
  result_vec
}
