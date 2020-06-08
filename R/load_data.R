
population_data <- read.xlsx("data/BD_SIR_MODEL_UH04142020.xls",1,startRow = 2 )

bd_time1 <- as.data.frame(read_excel("data/BD_COVID_SIR_Samiha.xlsx",sheet  = "INFECTION"))
bd_time1 <- bd_time1[,1:35]
colNames <- colnames(bd_time1)
dates <- as.vector(sapply(colNames[-1],as.numeric)) %>% lapply( as.Date,origin = "1899-12-30")
dates <- do.call("c",dates)
dates <- sapply(dates, format,"%Y-%m-%d")
colnames(bd_time1) <- c(colNames[1],dates)
bd_time2 <- read_excel("data/IEDCR REPORTING_04192020.xlsx" )
colNames <- colnames(bd_time2)
dates <- as.vector(sapply(colNames[-1],as.numeric)) %>% lapply( as.Date,origin = "1899-12-30")
dates <- do.call("c",dates)
dates <- sapply(dates, format,"%Y-%m-%d")
colnames(bd_time2) <- c(colNames[1],dates)

bd_time <- join(bd_time1,bd_time2,by="District")



bd_time2 <- read_excel("data/IEDCR REPORTING of DAILY cases per District06022020.xlsx")
bd_time2 <- as.data.frame(bd_time2)
bd_time2 <- bd_time2[,-1]
colNames <- colnames(bd_time2)
dates <- as.vector(sapply(colNames[-1],as.numeric)) %>% lapply( as.Date,origin = "1899-12-30")
dates <- do.call("c",dates)
dates <- sapply(dates, format,"%Y-%m-%d")
colnames(bd_time2) <- c(colNames[1],dates)
bd_time2[(bd_time2=="Missing")] <- 0
bd_time2[,-1] <- apply(bd_time2[,-1], 2, as.numeric)

dhaka_time <- apply(bd_time2[which(bd_time2$District %in% c("Dhaka City","Dhaka district","keraniganj")),-1],2,sum)
dhaka_time <- c("District"="Dhaka",dhaka_time)
bd_time2 <- rbind(dhaka_time,bd_time2)
bd_time2 <- bd_time2[-which(bd_time2$District %in% c("Dhaka City","Dhaka district","keraniganj")),]

l <- ncol(bd_time)
bd_time <- bd_time[,-c( (l-5):l )]
bd_time <- merge(bd_time,bd_time2,by='District',all = T)

bd_time[is.na(bd_time)] <- 0
# bd_time[(bd_time=="Missing")] <- 0
bd_time[,-1] <- apply(bd_time[,-1], 2, as.numeric)


bd_time[which(bd_time$District == "TOTAL"),-(1:35)] <- apply(bd_time[which(bd_time$District != "TOTAL"),-(1:35)],2,sum)


data <- data.matrix(bd_time[which(bd_time$District == "TOTAL"),-1])
data <- as.vector(data)
last_data <- which(data != 0)
last_data_idx <- last_data[length(last_data)] 



cases <- data[1 : last_data_idx]

# --------------------- for total ---------------
bd_latest <- as.data.frame(read_excel('total.xlsx'))
cases <- as.numeric(bd_latest$cases)
days <- seq(1,length(cases))

data <- cbind(cases,days)
data <- as.data.frame(data)

population_data <- population_data[,1:5] 
population_data <- population_data[rowSums(is.na(population_data)) != ncol(population_data),]

total_population <- sum(population_data$Total.population)
N <- total_population

start_date <- colnames(bd_time)[2]
start_date <- as.Date(start_date,format="%Y-%m-%d")
end_date <- colnames(bd_time)[last_data_idx+1]
end_date <- as.Date(end_date,format="%Y-%m-%d")


end_date <- bd_latest$dt[nrow(bd_latest)]
end_date <- as.Date(end_date,format="%Y-%m-%d")


t_period <- as.numeric(end_date - start_date+1)
date_range <- seq(start_date,end_date,by = 1)


