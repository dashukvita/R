# 1. Напишите функцию get_fractions, которая принимает на вход два числа, m и n, и возвращает аналогичный вектор, содержащий все дроби вида {i/m, i = 0, 1, ..., m} и  {j/n, j = 0, 1, ..., n}. Вектор не должен содержать повторов. И -- сюжетный поворот -- должен быть упорядочен в порядке убывания.
get_fractions <- function(m, n) {
  u <- seq (0, 1, 1/m)
  v <- seq (0, 1, 1/n)
  w <- unique((sort(c(u, v), decreasing = TRUE)))
}

answer = get_fractions(3, 7)
answer

# 2. Выполните в своей сессии следующие команды:
  set.seed(1337)
x <- runif(1e6, min = -1, max = 1)
# Cколько среди них чисел в диапазоне (-0.2, 0.3)
set.seed(1337)
x <- runif(1e6, min = -1, max = 1)
sum(ifelse ((x > -0.2 & x < 0.3), 1, 0))

# 3. Функция dice_roll(n) должна выдавать n независимых бросков игрального кубика. Допустимые значения в диапазоне от 1 до 6.
dice_roll <- function(n) {
  print(sample(1:6,n, replace=T))
}
z <- dice_roll(5)

# 4. Написать функцию проверяющую вектор на монотонность
is_monotone <- function(x) {
  all(c(x[-1]-x[-length(x)] >= 0)) || all(c(x[-1]-x[-length(x)] <= 0))
}

# 5. Написать функцию расчета сочетаний с повторениями и без
if(with_repretitions) factorial(n+k-1)/(factorial(k)*factorial(n-1)) else factorial(n)/(factorial(k)*factorial(n-k))

# 6. Есть целочисленный вектор v и число n. Задача — найти позицию элемента в векторе, который ближе всего к числу n. При этом если таких элементов несколько, необходимо указать все позиции.
find_closest <- function(v, n) {
  which(abs(v-n) == min(abs(v-n)), arr.ind = TRUE)
}

# 7. Построить зиккурат
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

# 8.Пусть x -- целочисленный вектор. Напишите функцию, которая вернёт матрицу из двух строк. В первой строке перечислите все различные элементы вектора, упорядоченные по возрастанию. Во второй строке укажите частоты (количество повторов) этих элементов.
count_elements <- function(x) {
  answer <- t(as.matrix(data.frame(table(x))))
  answer
}
count_elements(x)

# 9. Подсчёт общего покрытия
avian1 <- read.csv("/Users/user/Downloads/avianHabitat.csv")
avian2 <- read.csv("/Users/user/Downloads/avianHabitat2.csv", sep = ";",
                   quote = "\"",header = T, comment.char = "%",na.strings = "Don't remember", skip = 5)
avian2 <-cbind(avian2, Observer)
avian2 <-avian2[,c(1,17,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
avian <-rbind(avian1, avian2)
str(avian)
head(avian)
summary(avian)
any(!complete.cases(avian))
any(avian$PDB > 100)
check_percent_range <- function(x){
  any(x < 0 | x > 100)
}
names(avian)
coverage_var <- names(avian)[-(1:4)][c(T,F)]
avian$total_cov <- rowSums(avian[, coverage_var])
summary(avian$total_cov)

# 10. Пусть функция decorate_string действует поверх функции paste, дополнительно приклеивая к результату аргумент pattern. При этом этот аргумент должен быть присоединён как в начале строки (строк), так и в конце, но перевёрнутый задом наперёд.
decorate_string <- function(pattern,  ...) {
  pattern_rev <-  sapply(lapply(strsplit(pattern, NULL), rev), paste, collapse="")
  paste0(pattern,paste(...), pattern_rev) 
} 

# 11.  Получить две функции, содержащие честную и нечестную рулетку. Честная -- это когда все имеющиеся значения (всего их 37) выпадают с равной вероятностью. А нечестная пусть выдаёт все значения, кроме зеро, с равной вероятностью. Что же касается зеро (первый элемент определённого мной вектора roulette_values), то вероятность его выпадения пусть будет в два раза больше, чем любого другого значения.
generator <- function(set, prob = rep(1/length(set), length(set))) {
  function(n) 
    sample(set, n , replace=T, prob)
}
fair_roulette <- generator(roulette_values, c(rep(2/(length(roulette_values)+1),1),rep(1/(length(roulette_values)+1),(length(roulette_values)-1))))
fair_roulette(1000)
rigged_roulette <- generator(roulette_values)
rigged_roulette(5)

# 12. Напишем бинарный оператор! Пусть %+% действует на два числовых вектора, складывая их поэлементно, но без учёта правил переписывания: если длина векторов различна, то возвращаем вектор большей длины, но с пропущенными значениями в конце.
"%+%" <- function(x, y) {
  if (length(x) > length(y)) n <- length(x) else n <- length(y)
  x[1:n]+y[1:n]
}

# 13. Составьте вектор cat_catalogue, содержащий всевозможные комбинации имеющихся характеристик, и отсортируйте его.
cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")

vect <-sort(c(outer(outer(cat_temper, cat_color, paste), outer(cat_age, cat_trait, paste), paste)))
vect

# 14. Определить количество тех замеров, где обнаруживается хотя бы один экземпляр вида.
av <-
  avian %>%
  subset(EHt>0, c(Site,Observer,DBHt,WHt,EHt,AHt,HHt,LHt)) %>%
  transform(Site = factor(str_replace(.$Site, "[:digit:]+",""))) %>%
  group_by(Site, Observer) %>%
  tally()
av

# 15. При помощи функции simulate_walk из предыдущего видео можно моделировать случайное блуждание на отрезке [lower, upper]. Это достаточно простой случай. Больший интерес представляют задачи с блужданием по плоскости, то есть в размерности 2.
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