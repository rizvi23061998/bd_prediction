dfs <- vector( "list",4)

l <- 1
for (ii in 2:3){
  for (jj in 2:3){
    id <- paste(ids[ii],"_",ids2[jj],"_",rec_type,sep = "")
    dfs[[l]] <- readRDS(paste0("outputs/rds/df/df_",id,".rds"))

    # dfs[[l]]$Inc <- cumsum(dfs[[l]]$Inc)
    # dfs[[l]]$Inc_pred <- cumsum(dfs[[l]]$Inc_pred)

    l <- l+1
  }
}
# 
# 
# 
# # color_val <- palette(rainbow(16))
# 
# c25 <- c(
#   "dodgerblue2", "#E31A1C", # red
#   "green4",
#   "#6A3D9A", # purple
#   "#FF7F00", # orange
#   "black", "gold1",
#   "skyblue2", "#FB9A99", # lt pink
#   "palegreen2",
#   "#CAB2D6", # lt purple
#   "#FDBF6F", # lt orange
#   "gray70", "khaki2",
#   "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
#   "darkturquoise", "green1", "yellow4", "yellow3",
#   "darkorange4", "brown","red"
# )
# 
# names(c25) <- c(LETTERS[1:25],"Real")
# 
# 
# g_case <- ggplot()+
#   geom_point(data = (dfs[[1]]),aes(x=Dates,y=Inc,color="Real"))
# 
# 
# for (ii in 1:length(dfs)){
#   print(LETTERS[ii])
#   g_case <- g_case +
#     geom_line(data=dfs[[ii]],aes(x=Dates,y=Inc_pred,col = !!(LETTERS[ii])),size = 1) 
# }  
#   
#   # labs(x = "Dates", y= "Daily Incidence",color="") +
# 
# g_case <- g_case+
#   labs(x = "Dates", y= "Daily Incidence",color="") +
#   scale_color_manual(values = c25)
# 
# type="both_90"
# 
# tiff(paste0("outputs/img/case_",type,".tiff"),res = 300,height = 3000,width = 3000,unit = "px")
# print(g_case)
# dev.off()

type = "beta_365"
plot_df_no <- readRDS(paste0("outputs/rds/df/df_no_red_",type,".rds"))


color_val <- c("Real" = "blue","No Change" = "red",
               "10% Reduction(Beta),\n10% Increase(Gamma)" = "skyblue2", "10% Reduction(Beta),\n20% Increase(Gamma)" = "orange",
               "20% Reduction(Beta),\n10% Increase(Gamma)" = "#b3ff00", "20% Reduction(Beta),\n20% Increase(Gamma)" = "#096e02")

g_case <- ggplot()+
  # geom_point(data = plot_df_no,aes(x=Dates,y=Inc,color="Real"))+
  geom_line(data=plot_df_no,aes(x=Dates,y=Inc_pred,col = "No Change"),size = 1)
            
for(ii in 1:length(dfs)){
  g_case <- g_case +
        geom_line(data=dfs[[ii]],aes(x=Dates,y=Inc_pred,col = !!(names(color_val[(ii+2)]))),size = 1)
}

g_case <- g_case+
  labs(x = "Dates", y= "Number of Cases(Daily)") +
  theme(legend.position = c(.83,.8),legend.key.size = unit(1.8,"line"),
        legend.text=element_text(size = 10),
        legend.title = element_text(colour = 'red',size=13),
        legend.title.align =  .5,
        axis.text  = element_text(size=12),
        axis.title = element_text(colour ="navyblue",size=14))+
  scale_color_manual(name="Intervention\nType",values = color_val)
              
# type = rec_type
type = "both"

tiff(paste0("outputs/img/case_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_case)
dev.off()

