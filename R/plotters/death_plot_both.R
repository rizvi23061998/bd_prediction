dfs <- vector( "list",4)

death_rate <- 0.031 * 0.196 * 0.593
rec_type <- "both"

death_df <- c()

l <- 1
for (ii in 2:3){
  for (jj in 2:3){
    id <- paste(ids[ii],"_",ids2[jj],"_",rec_type,sep = "")
    dfs[[l]] <- readRDS(paste0("outputs/rds/df/df_",id,".rds"))
    
    # dfs[[l]]$Inc <- cumsum(dfs[[l]]$Inc)
    dfs[[l]]$Inc_pred <- (dfs[[l]]$Inc_pred) * death_rate
    
    death_df <- c(death_df,round(sum(dfs[[l]]$Inc_pred)))
    
    l <- l+1
  }
}

type = "beta_365"
plot_df_no <- readRDS(paste0("outputs/rds/df/df_no_red_",type,".rds"))
plot_df_no$Inc_pred <- plot_df_no$Inc_pred * death_rate

death_df <- c(round(sum(plot_df_no$Inc_pred)), death_df)
death_df <- as.data.frame(death_df)
colnames(death_df) <- c("y")
death_df$x <- c("No Change",
                paste0("10% Decrease(",expression(Beta),"),\n10% increase(",expression(Gamma),")"),
                paste0("10% Decrease(",expression(Beta),"),\n20% increase(",expression(Gamma),")"),
                paste0("20% Decrease(",expression(Beta),"),\n10% increase(",expression(Gamma),")"),
                paste0("20% Decrease(",expression(Beta),"),\n20% increase(",expression(Gamma),")")) 


color_val <- c( "red",
               "skyblue2",  "orange",
               "#b3ff00", "#096e02")
names(color_val) <- death_df


g_case <- ggplot(data = death_df,aes(x=x,y=y,fill=x))+
  # geom_polygon(aes(x=Dates,y= Cases),fill = "#c9e5ff")+
  # geom_point(data = plot_df_no,aes(x=Dates,y=Inc,color="Real")) +
  geom_bar(stat = "identity",width = .7)+
  geom_text(aes(label=y), vjust=-0.3, size=3.5)+
  scale_fill_manual(values = color_val) + 
  labs(x = "Intervention Type",y = "Total Death upto 365 day")+
  theme(legend.position = "none",
        axis.text = element_text(colour = "black",size = 10,angle = 60,hjust=1),
        axis.title = element_text(color = "navyblue",size = 12))

type = "both"

tiff(paste0("outputs/img/death_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_case)
dev.off()