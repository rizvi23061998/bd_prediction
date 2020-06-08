library(ggplot2)


death_rate <- 0.031 * 0.196 * 0.593

# type =  "beta_90"
type = "beta_365"

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

death_no_change <- sum(plot_df_no$Inc_pred)
death_10_beta <- sum(plot_df_10$Inc_pred)
death_20_beta <- sum(plot_df_20$Inc_pred)
death_30_beta <- sum(plot_df_30$Inc_pred)

type = "gamma_365"

plot_df_30_gamma <- readRDS(paste0("outputs/rds/df/df_30_inc_",type,".rds"))
plot_df_10_gamma <- readRDS(paste0("outputs/rds/df/df_10_inc_",type,".rds"))
plot_df_20_gamma <- readRDS(paste0("outputs/rds/df/df_20_inc_",type,".rds"))


plot_df_10_gamma$Inc_pred <- (plot_df_10_gamma$Inc_pred*death_rate)
plot_df_20_gamma$Inc_pred <- (plot_df_20_gamma$Inc_pred*death_rate)
plot_df_30_gamma$Inc_pred <- (plot_df_30_gamma$Inc_pred*death_rate)


death_10_gamma <- sum(plot_df_10_gamma$Inc_pred)
death_20_gamma <- sum(plot_df_20_gamma$Inc_pred)
death_30_gamma <- sum(plot_df_30_gamma$Inc_pred)

death_df <- c("No Change" = death_no_change, "10% Reduction\n of Beta" = death_10_beta,
              "20% Reduction\n of Beta" = death_20_beta,"30% Reduction\n of Beta" = death_30_beta,
              "10% Increase\n of Gamma" = death_10_gamma,"20% Increase\n of Gamma" = death_20_gamma,
              "30% Increase\n of Gamma" = death_30_gamma)

death_df<- as.data.frame(death_df)
death_df <- apply(death_df, 2, round)
death_df<- as.data.frame(death_df)
death_df$Intervention_Type <- rownames(death_df)
rownames(death_df) <- NULL
colnames(death_df) <- c("y",'x')

color_val <- c("Real" = "blue","No Change" = "red",
               "10% Reduction\n of Beta" = "orange", "20% Reduction\n of Beta" = "#b3ff00",
               "30% Reduction\n of Beta" = "#096e02")

color_val <- c(color_val,
               "10% Increase\n of Gamma" = "orange", "20% Increase\n of Gamma" = "#b3ff00",
               "30% Increase\n of Gamma" = "#096e02")


color_val <- c("red","orange","#b3ff00","#096e02","orange","#b3ff00","#096e02")
names(color_val) <- death_df$x

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
  
  
# 

type = "comb"
# tiff("outputs/case_ls.tiff",res = 300,height = 3000,width = 3000,unit = "px")
# tiff("outputs/img/case_smc_beta.tiff",res = 300,height = 3000,width = 3000,unit = "px")
tiff(paste0("outputs/img/death_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_case)
dev.off()