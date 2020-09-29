# Определить количество тех замеров, где обнаруживается хотя бы один экземпляр вида.
av <-
  avian %>%
  subset(EHt>0, c(Site,Observer,DBHt,WHt,EHt,AHt,HHt,LHt)) %>%
  transform(Site = factor(str_replace(.$Site, "[:digit:]+",""))) %>%
  group_by(Site, Observer) %>%
  tally()
av
