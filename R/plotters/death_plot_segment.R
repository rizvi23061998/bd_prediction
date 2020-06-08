library(ggplot2)

death_rate <- 0.031 * 0.196 * 0.593

# type =  "beta_90"
type = "beta_seg_60_2"
lockdown_period <- 60

plot_df_no <- readRDS(paste0("outputs/rds/df/df_no_red_",type,".rds"))
plot_df_30 <- readRDS(paste0("outputs/rds/df/df_30_red_",type,".rds"))
plot_df_10 <- readRDS(paste0("outputs/rds/df/df_10_red_",type,".rds"))
plot_df_20 <- readRDS(paste0("outputs/rds/df/df_20_red_",type,".rds"))

# plot_df_no$Inc <- cumsum(plot_df_no$Inc)
# plot_df_no$Inc_pred <- cumsum(plot_df_no$Inc_pred)
# plot_df_10$Inc_pred <- cumsum(plot_df_10$Inc_pred)
# plot_df_20$Inc_pred <- cumsum(plot_df_20$Inc_pred)
# plot_df_30$Inc_pred <- cumsum(plot_df_30$Inc_pred)

# plot_df_no$Inc <- (plot_df_no$Inc)*death_rate
plot_df_no$Inc_pred <- (plot_df_no$Inc_pred*death_rate)
plot_df_10$Inc_pred <- (plot_df_10$Inc_pred*death_rate)
plot_df_20$Inc_pred <- (plot_df_20$Inc_pred*death_rate)
plot_df_30$Inc_pred <- (plot_df_30$Inc_pred*death_rate)



death_df <- c(
  round(sum(plot_df_no$Inc_pred)),
  round(sum(plot_df_10$Inc_pred)),
  round(sum(plot_df_20$Inc_pred)),
  round(sum(plot_df_30$Inc_pred))
)

death_df <- as.data.frame(death_df)
colnames(death_df) <- c("y")
death_df$x <- c(
  "No Change",
  "10% Reduction\n of Beta",
  "20% Reduction\n of Beta",
  "30% Reduction\n of Beta"
) 

color_val <- c("Real" = "blue","No Change" = "red",
               "10% Reduction\n of Beta" = "orange", "20% Reduction\n of Beta" = "#b3ff00",
               "30% Reduction\n of Beta" = "#096e02")

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

tiff(paste0("outputs/img/segmented/death_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_case)
dev.off()