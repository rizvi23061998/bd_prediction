dfs <- vector( "list",4)

l <- 1
for (ii in 1:length(scaled_betas)){
  # for (jj in 1:length(scaled_gammas)){
    id <- paste(beta_id[ii],type,sep = "_")
    dfs[[l]] <- readRDS(paste0("outputs/rds/df/reports/df_",id,".rds"))
    dfs[[l]] <- readRDS(paste0("outputs/rds/df/df_",id,".rds"))
    
    dfs[[l]]$Inc <- cumsum(dfs[[l]]$Inc)
    dfs[[l]]$Inc_pred <- cumsum(dfs[[l]]$Inc_pred)
    dfs[[l]]$Reports <- cumsum(dfs[[l]]$Reports)
    
    l <- l+1
  # }
}



# color_val <- palette(rainbow(16))

c25 <- c(
  "No Change"="dodgerblue2", 
  "10% Reduction" = "#E31A1C", # red
  "20% Reduction" = "green4",
  "30% Reduction" = "#6A3D9A", # purple
  "10% Increase" = "#FF7F00", # orange
  "60% Increase" ="black", 
  "30% Increase" ="gold1",
  "skyblue2", "#FB9A99", # lt pink
  "20% Increase" ="palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown",
  "Real"="red"
)

# names(c25) <- c(LETTERS[1:25],"Real")

beta_names <- c("No Change","10% Reduction","20% Reduction","30% Reduction")
# beta_names <- c("No Change","10% Increase","20% Increase","30% Increase")

g_case <- ggplot()+
  geom_point(data = (dfs[[1]]),aes(x=Dates,y=Inc,color="Real")) 


for (ii in 1:length(dfs)){
  # print(LETTERS[ii])
  g_case <- g_case +
    geom_line(data=dfs[[ii]],aes(x=Dates,y=Inc_pred,col = !!(beta_names[ii])),size = 1) 
  
  # g_case <- g_case + geom_line(data=dfs[[ii]],aes(x=Dates,y=Reports,col = !!(beta_names[ii])),size = 1,linetype="dotted")
    
}  

# labs(x = "Dates", y= "Daily Incidence",color="") +
g_title <- "Cumulative No. of Cases with Change in Transmission Rate"
# g_title <- "Cumulative No. of Cases with Change in Removal Rate"
g_case <- g_case+
  labs(title = g_title,x = "Dates", y= "Cumulative No. of Cases",color="") +
  theme(title = element_text(size=15,color = "steelblue4",hjust = .5))+
  scale_color_manual(values = c25)



# tiff(paste0("outputs/img/reports/case_",type,"_c.tiff"),res = 300,height = 3000,width = 3000,unit = "px")
tiff(paste0("outputs/img/case_",type,"_c.tiff"),res = 300,height = 3000,width = 3000,unit = "px")
print(g_case)
dev.off()
