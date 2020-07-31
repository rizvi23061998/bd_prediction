library(dplyr)

global <- read.csv("data/time_series_covid19_confirmed_global.csv")
bd <- global[which(global$Country.Region == "Bangladesh"),-c(1:4)]
# bd <- as.vector(bd)
bd <- bd[((which(bd!=0)[1]) : ncol(bd))]
dates <- names(bd) %>% as.Date("X%m.%d.%y")
colnames(bd) <- NULL
rownames(bd) <- c("cases")
bd <- t(bd)
bd <- bd - c(0,head(bd,-1))
bd <- cbind("cases" = (bd),"dt" = as.character(dates))

write.csv(bd,"bd_20_July.csv",row.names = F)