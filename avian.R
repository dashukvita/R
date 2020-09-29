# Подсчёт общего покрытия
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
