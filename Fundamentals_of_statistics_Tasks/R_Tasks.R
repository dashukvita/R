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

#2 Напишите функцию most_significant, которая получает на вход dataframe с произвольным количеством переменных, где каждая переменная это нуклеотидная последовательность. 
#Функция должна возвращать вектор с �названием переменной (или переменных), в которой был получен минимальный p - уровень значимости при проверке гипотезы о равномерном распределении нуклеотидов при помощи критерия хи - квадрат. 

most_significant <-  function(x){
  n <- ncol(x)
  mat_r <- matrix(,nrow = 1, ncol=ncol(x))
  colnames(mat_r) <- colnames(x)
  for(i in 1:n){
    mat_r[1, i] <- chisq.test(table(test_data[i]))$p.value
  }
  names(which(mat_r[1,] == min(mat_r)))
}
