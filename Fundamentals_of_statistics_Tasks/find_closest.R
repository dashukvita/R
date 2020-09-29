# Есть целочисленный вектор v и число n. Задача — найти позицию элемента в векторе, который ближе всего к числу n. При этом если таких элементов несколько, необходимо указать все позиции.
find_closest <- function(v, n) {
  which(abs(v-n) == min(abs(v-n)), arr.ind = TRUE)
}

# Написать функцию расчета сочетаний с повторениями и без
if(with_repretitions) factorial(n+k-1)/(factorial(k)*factorial(n-1)) else factorial(n)/(factorial(k)*factorial(n-k))
