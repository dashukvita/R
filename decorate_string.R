# Пусть функция decorate_string действует поверх функции paste, дополнительно приклеивая к результату аргумент pattern. При этом этот аргумент должен быть присоединён как в начале строки (строк), так и в конце, но перевёрнутый задом наперёд.
decorate_string <- function(pattern,  ...) {
  pattern_rev <-  sapply(lapply(strsplit(pattern, NULL), rev), paste, collapse="")
  paste0(pattern,paste(...), pattern_rev) 
}
