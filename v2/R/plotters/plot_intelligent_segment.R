library(ggplot2)

# plot_df_no <- readRDS("outputs/df_no_red_ls.rds")
# plot_df_25 <- readRDS("outputs/df_25_red_ls.rds")
# plot_df_50 <- readRDS("outputs/df_50_red_ls.rds")
# plot_df_75 <- readRDS("outputs/df_75_red_ls.rds")


# plot_df_no <- readRDS("outputs/rds/df/df_no_red_smc.rds")
# plot_df_30 <- readRDS("outputs/rds/df/df_30_red_smc.rds")
# plot_df_10 <- readRDS("outputs/rds/df/df_10_red_smc.rds")
# plot_df_20 <- readRDS("outputs/rds/df/df_20_red_smc.rds")

# type =  "beta_90"
# type = "beta_seg_60_2"
type = "beta_seg_intel_2"
# lockdown_period <- 60

plot_df_no <- readRDS(paste0("outputs/rds/df/df_no_red_",type,".rds"))
plot_df_30 <- readRDS(paste0("outputs/rds/df/df_30_red_",type,".rds"))
plot_df_10 <- readRDS(paste0("outputs/rds/df/df_10_red_",type,".rds"))
plot_df_20 <- readRDS(paste0("outputs/rds/df/df_20_red_",type,".rds"))

max_vals <- plot_df_no[which.max(plot_df_no$Inc_pred),c("Dates","Inc_pred")]
max_vals <- rbind(max_vals,plot_df_10[which.max(plot_df_10$Inc_pred),c("Dates","Inc_pred")])
max_vals <- rbind(max_vals,plot_df_20[which.max(plot_df_20$Inc_pred),c("Dates","Inc_pred")])
max_vals <- rbind(max_vals,plot_df_30[which.max(plot_df_30$Inc_pred),c("Dates","Inc_pred")])


color_val <- c("Real" = "blue","No Change" = "red",
               "10% Reduction\nof Beta" = "orange", "20% Reduction\nof Beta" = "#b3ff00",
               "30% Reduction\nof Beta" = "#096e02")

color_val <- c(color_val,
               "10% Increase" = "orange", "20% Increase" = "#b3ff00",
               "30% Increase" = "#096e02")






g_case <- ggplot()
  # geom_polygon(aes(x=Dates,y= Cases),fill = "#c9e5ff")+
  # geom_point(data = plot_df_no,aes(x=Dates,y=Inc,color="Real")) 

end_date_idx  <- which(plot_df_no[,"Dates"] == as.Date("2020-05-24"))

# ll <- lockdown_period + end_date_idx

for(ii in 1:length(relax_dates)){
  ll <- relax_dates[ii] + end_date_idx + 15
  g_case <- g_case + 
    geom_rect(aes(xmin = !!(plot_df_no[(ll+1),"Dates"]),xmax = !!(plot_df_no[(ll+30),"Dates"]),
                  ymin = 0, ymax = Inf),fill = "#d1d8e0") ##dedfe0"#d1d8e0"
  # 
}


g_case <- g_case +   
  geom_line(data=plot_df_no,aes(x=Dates,y=Inc_pred,color="No Change"),size = 1) +
  geom_line(data=plot_df_10,aes(x=Dates,y=Inc_pred,color="10% Reduction\nof Beta"),size = 1) +
  geom_line(data=plot_df_20,aes(x=Dates,y=Inc_pred,color="20% Reduction\nof Beta"),size = 1) +
  geom_line(data=plot_df_30,aes(x=Dates,y=Inc_pred,color="30% Reduction\nof Beta"),size = 1) +
  # geom_line(data=plot_df_10_gamma,aes(x=Dates,y=Inc_pred,color="10% Increase"),size = 1,linetype="dotted") +
  # geom_line(data=plot_df_20_gamma,aes(x=Dates,y=Inc_pred,color="20% Increase"),size = 1,linetype="dotted") +
  # geom_line(data=plot_df_30_gamma,aes(x=Dates,y=Inc_pred,color="30% Increase"),size = 1,linetype="dotted") +
  labs(x = "Dates", y= "Number of Cases(Daily)",color="") +
  # labs(x = "Dates", y= "Cumulative Incidence",color="") +
  scale_color_manual(name='Intervention Type',values = color_val)+
  theme(
        legend.key.size =   unit(2.5,"line"),
        legend.position = c(0.2,.75),
        legend.text=element_text(size = 12),
        legend.title = element_text(colour = 'red',size=15),
        legend.title.align =  .5,
        axis.text  = element_text(size=12),
        axis.title = element_text(colour ="navyblue",size=14),
        legend.box.spacing = unit(.1,"line"),
        legend.spacing = unit(100,"pt")
  )
# 

for(ii in 1:nrow(max_vals)){
  g_case <- g_case+
    geom_text(aes(x=!!(max_vals[ii,1]), y= !!(max_vals[ii,2]), label=!!(round(max_vals[ii,2]))) ,
              vjust=-.8,size=4,col="black")
}

# type = "seg_90_2"
# tiff("outputs/case_ls.tiff",res = 300,height = 3000,width = 3000,unit = "px")
# tiff("outputs/img/case_smc_beta.tiff",res = 300,height = 3000,width = 3000,unit = "px")
tiff(paste0("outputs/img/segmented/case_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_case)
dev.off()