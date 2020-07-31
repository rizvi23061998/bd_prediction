library(dplyr)
library(zoo)
library(DataCombine)
library(e1071)
library(ggplot2)

set.seed(123)

all_country_test_daily <- read.csv("data/owid-covid-data.csv")

top_19 <- read.csv('data/full-list-covid-19-tests.csv')#,check.names = F)
top_19 <- top_19[-1,]
top_19$Country.Other <- factor(top_19$Country.Other)

# top_19_daily_test <- all_country_test_daily[all_country_test_daily$Entity %in% top_19$Country.Other,]
# top_19_daily_test$Entity <- factor(top_19_daily_test$Entity)
# top_19_daily_test$days <- rep(0,nrow(top_19_daily_test))

# for(x in levels(top_19_daily_test$Entity)){
#   print(x)
#   len <- length(top_19_daily_test[which(top_19_daily_test$Entity == x),"days"])
#   print(len)
#   top_19_daily_test[top_19_daily_test$Entity == x,"days"] <- seq(1,len,by=1)
# }
# 
# g <- ggplot()  + 
#   geom_line(data = top_19_daily_test,aes(x = days,y=New.tests,group=Entity,col=Entity))+
#   facet_wrap(~Entity)
# print(g)
# print(all_country_test_daily)


ref_countries <- c("Australia","Belgium","Canada","Germany","France","Italy",
                   "Netherlands","Turkey","United Kingdom",
                   "United States","India","Spain","Iran","Qatar","Russia","Bangladesh")
ref_countries_test_daily <- NULL
  # select(location,new_cases,new_tests,total_tests)

test_features <- c("tDay-7","tDay-6","tDay-5","tDay-4","tDay-3","tDay-2","tDay-1")
pos_features <- c("pDay-7","pDay-6","pDay-5","pDay-4","pDay-3","pDay-2","pDay-1")
for(cc in ref_countries){
  tmp_group <- all_country_test_daily %>% filter(location==cc)
  idx <- which(!is.na(tmp_group$total_tests))
  tmp_group$new_cases <- abs(tmp_group$new_cases)
  # if(tmp_group$iso_code=="BGD"){
  #   print(tmp_group$new_cases)
  # }
  tmp_group$new_cases[is.na(tmp_group$new_cases)] <- 0
  if(length(idx) > 0){
    tmp_group <- tmp_group[idx[1]:idx[length(idx)],]
    tmp_group$total_tests <- na.approx(tmp_group$total_tests)
    tmp_group$new_tests <- tmp_group$total_tests- c(0,head(tmp_group$total_tests,-1))
    tmp_group$pos_rate <- tmp_group$new_cases/tmp_group$new_tests
    
    # print(tmp_group$location[1])
    # print(tmp_group[,c("new_tests","pos_rate")])
    for(ii in 1:7){
      tmp_group <- slide(tmp_group,Var = "new_cases",NewVar = test_features[ii],slideBy = (ii-8))  
      tmp_group <- slide(tmp_group,Var = "pos_rate",NewVar = pos_features[ii],slideBy = (ii-8))  
    }
    
    if(is.null(ref_countries_test_daily) ){
      ref_countries_test_daily <- tmp_group[8:nrow(tmp_group),]
    }else{
      ref_countries_test_daily <- rbind(ref_countries_test_daily,tmp_group[8:nrow(tmp_group),])  
    }    
    if(tmp_group$iso_code[1] =="BGD"){
      bd_7 <- tmp_group[1:7,"new_tests"]
      bd_all <- tmp_group[,"new_tests"]
    }
    
  }else{
    # print(tmp_group$location)
  }
}

write.csv(ref_countries_test_daily,"ref_countries_features.csv")

ref_countries_test_daily <- ref_countries_test_daily %>% select(c(location,"new_tests",all_of(pos_features),all_of(test_features)))


train_data <- ref_countries_test_daily[ref_countries_test_daily$location != "Bangladesh",-1]
rownames(train_data) <- NULL
test_data <- ref_countries_test_daily[ref_countries_test_daily$location == "Bangladesh",-1]
rownames(test_data) <- NULL
model <- svm(new_tests~.,train_data)

pred <- predict(model,test_data[,-1])

pred <- c(bd_7,as.numeric(pred))

print(quantile(pred/bd_all))

z<- as.data.frame(cbind("day" = 1:length(pred),pred,"real" = bd_all))
col_val = c("Real" = "red","Predicted" = "blue")
tiff("test_with_cases.tiff",res = 300,height = 2000,width = 2000)
g <- ggplot()+
    geom_point(data = z,aes(x=day,y=real,col="Real"))+
  geom_line(data = z,aes(x=day,y=pred,col = "Predicted"))+
  labs(y="# of tests")+
  scale_color_manual(values = col_val)
print(g)
dev.off()

low_pos <- 0.006535948
high_pos <- 0.365267246 
med_pos <- 0.159835436

med_ratio <- 3.66

pred <- pred[1:135]

bd_reported <- read.csv("bd_20_July.csv")
bd_pred_low <- bd_reported
bd_pred_low$cases <- round(pred*low_pos)
bd_pred_high <- bd_reported
bd_pred_high$cases <- round(pred*high_pos)
bd_pred_med <- bd_reported
bd_pred_med$cases <- round(pred*med_pos)
bd_pred_med_ratio <- bd_reported
bd_pred_med_ratio$cases <- round(bd_pred_med_ratio$cases* med_ratio)



# write.csv(bd_pred_low,"bd_20_July_low.csv")
# write.csv(bd_pred_high,"bd_20_July_high.csv")
# write.csv(bd_pred_med,"bd_20_July_med.csv")
# write.csv(bd_pred_med_ratio,"bd_20_July_med_ratio.csv")

