# Получить две функции, содержащие честную и нечестную рулетку. Честная -- это когда все имеющиеся значения (всего их 37) выпадают с равной вероятностью. А нечестная пусть выдаёт все значения, кроме зеро, с равной вероятностью. Что же касается зеро (первый элемент определённого мной вектора roulette_values), то вероятность его выпадения пусть будет в два раза больше, чем любого другого значения.
generator <- function(set, prob = rep(1/length(set), length(set))) {
  function(n) 
    sample(set, n , replace=T, prob)
}
fair_roulette <- generator(roulette_values, c(rep(2/(length(roulette_values)+1),1),rep(1/(length(roulette_values)+1),(length(roulette_values)-1))))
fair_roulette(1000)
rigged_roulette <- generator(roulette_values)
rigged_roulette(5)
