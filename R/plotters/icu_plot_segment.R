library(ggplot2)

icu_rate <- 0.031 * 0.196

# type =  "beta_90"
type = "beta_seg_90_2"
# lockdown_period <- 60

plot_df_no <- readRDS(paste0("outputs/rds/df/df_no_red_",type,".rds"))
plot_df_30 <- readRDS(paste0("outputs/rds/df/df_30_red_",type,".rds"))
plot_df_10 <- readRDS(paste0("outputs/rds/df/df_10_red_",type,".rds"))
plot_df_20 <- readRDS(paste0("outputs/rds/df/df_20_red_",type,".rds"))

# plot_df_no$Inc <- (plot_df_no$Inc)*icu_rate
plot_df_no$Inc_pred <- (plot_df_no$Inc_pred*icu_rate)
plot_df_10$Inc_pred <- (plot_df_10$Inc_pred*icu_rate)
plot_df_20$Inc_pred <- (plot_df_20$Inc_pred*icu_rate)
plot_df_30$Inc_pred <- (plot_df_30$Inc_pred*icu_rate)

icu_no_change <- icu_calc(plot_df_no)
icu_10_beta <- icu_calc(plot_df_10)
icu_20_beta <- icu_calc(plot_df_20)
icu_30_beta <- icu_calc(plot_df_30)

icu_df <- cbind("Dates" = plot_df_no$Dates,"No Change" = icu_no_change, "10% Reduction\nof Beta" = icu_10_beta,
                "20% Reduction\nof Beta" = icu_20_beta,"30% Reduction\nof Beta" = icu_30_beta
               )

icu_df <- as.data.frame(icu_df)
icu_df<- as.data.frame(icu_df)
icu_df <- icu_df%>% gather('int_type','icu',2:5)
# icu_df$Dates <- sapply(icu_df$Dates,as.Date, origin='1970-01-01')
icu_df$Dates <- as.Date(icu_df$Dates,origin = "1970-01-01")
color_val <- c("No Change" = "red",
               "10% Reduction\nof Beta" = "orange", "20% Reduction\nof Beta" = "#b3ff00",
               "30% Reduction\nof Beta" = "#096e02")



g_icu <- ggplot(data = icu_df,aes(x = Dates,y=icu,group=int_type,col=int_type)) +
  geom_rect(xmin=0,xmax=Inf,ymin=0,ymax=total_icu,fill="#9eff99")+
  geom_line()+
  labs(x = "Dates",y = "Number of patients in ICU(Daily)")+
  scale_color_manual(name="Intervention Type",values = color_val)+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  
  theme(legend.key = element_rect(fill="white",
                                  linetype = "solid",size = .2),
        legend.key.size =   unit(1.8,"line"),
        legend.position = c(.85,.75),
        legend.text=element_text(size = 10),
        legend.title = element_text(colour = 'red',size=13),
        legend.title.align =  .5,
        axis.text  = element_text(size=12),
        axis.title = element_text(colour ="navyblue",size=14),
        legend.box.spacing = unit(.1,"line"),
        
  )


type = paste(type,icu_period,sep="_")


tiff(paste0("outputs/img/icu/icu_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_icu)
dev.off()