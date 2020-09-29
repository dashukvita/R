# Составьте вектор cat_catalogue, содержащий всевозможные комбинации имеющихся характеристик, и отсортируйте его.
cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")

vect <-sort(c(outer(outer(cat_temper, cat_color, paste), outer(cat_age, cat_trait, paste), paste)))
vect
