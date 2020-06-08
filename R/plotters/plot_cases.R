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

# plot_df_no$Inc <- (plot_df_no$Inc)/N
# plot_df_no$Inc_pred <- (plot_df_no$Inc_pred/N)
# plot_df_10$Inc_pred <- (plot_df_10$Inc_pred/N)
# plot_df_20$Inc_pred <- (plot_df_20$Inc_pred/N)
# plot_df_30$Inc_pred <- (plot_df_30$Inc_pred/N)

type = "gamma_365"

plot_df_30_gamma <- readRDS(paste0("outputs/rds/df/df_30_inc_",type,".rds"))
plot_df_10_gamma <- readRDS(paste0("outputs/rds/df/df_10_inc_",type,".rds"))
plot_df_20_gamma <- readRDS(paste0("outputs/rds/df/df_20_inc_",type,".rds"))


# plot_df_10_gamma$Inc_pred <- (plot_df_10_gamma$Inc_pred/N)
# plot_df_20_gamma$Inc_pred <- (plot_df_20_gamma$Inc_pred/N)
# plot_df_30_gamma$Inc_pred <- (plot_df_30_gamma$Inc_pred/N)


color_val <- c("Real" = "blue","No Reduction" = "red",
               "10% Reduction" = "orange", "20% Reduction" = "#b3ff00",
               "30% Reduction" = "#096e02")

color_val <- c(color_val,
               "10% Increase" = "orange", "20% Increase" = "#b3ff00",
               "30% Increase" = "#096e02")


# g_case <- ggplot()+
#   # geom_polygon(aes(x=Dates,y= Cases),fill = "#c9e5ff")+
#   geom_point(data = plot_df_no,aes(x=Dates,y=Inc,color="Real")) +
# 
#   geom_line(data=plot_df_no,aes(x=Dates,y=Inc_pred,color="No Increase"),size = 1) +
  # geom_line(data=plot_df_10,aes(x=Dates,y=Inc_pred,color="10% Increase"),size = 1) +
  # geom_line(data=plot_df_20,aes(x=Dates,y=Inc_pred,color="20% Increase"),size = 1) +
  # geom_line(data=plot_df_30,aes(x=Dates,y=Inc_pred,color="30% Increase"),size = 1) +
#   # labs(x = "Dates", y= "Daily Incidence",color="") +
#   labs(x = "Dates", y= "Cumulative Incidence",color="") +
#   scale_color_manual(values = color_val)


g_case <- ggplot()+
  # geom_polygon(aes(x=Dates,y= Cases),fill = "#c9e5ff")+
  geom_point(data = plot_df_no,aes(x=Dates,y=Inc,color="Real")) +

  geom_line(data=plot_df_no,aes(x=Dates,y=Inc_pred,color="No Reduction"),size = 1) +
  geom_line(data=plot_df_10,aes(x=Dates,y=Inc_pred,color="10% Reduction"),size = 1) +
  geom_line(data=plot_df_20,aes(x=Dates,y=Inc_pred,color="20% Reduction"),size = 1) +
  geom_line(data=plot_df_30,aes(x=Dates,y=Inc_pred,color="30% Reduction"),size = 1) +
  geom_line(data=plot_df_10_gamma,aes(x=Dates,y=Inc_pred,color="10% Increase"),size = 1,linetype="dotted") +
  geom_line(data=plot_df_20_gamma,aes(x=Dates,y=Inc_pred,color="20% Increase"),size = 1,linetype="dotted") +
  geom_line(data=plot_df_30_gamma,aes(x=Dates,y=Inc_pred,color="30% Increase"),size = 1,linetype="dotted") +
    labs(x = "Dates", y= "Daily Incidence",color="") +
  # labs(x = "Dates", y= "Cumulative Incidence",color="") +
  scale_color_manual(values = color_val)
# 

type = "comb"
# tiff("outputs/case_ls.tiff",res = 300,height = 3000,width = 3000,unit = "px")
# tiff("outputs/img/case_smc_beta.tiff",res = 300,height = 3000,width = 3000,unit = "px")
tiff(paste0("outputs/img/case_",type,".tiff"),res = 300,height = 3000,width = 3000,unit = "px")
print(g_case)
dev.off()