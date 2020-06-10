library(ggplot2)
library(tidyr)

icu_rate <- 0.031 * 0.196
icu_period <- 9
total_icu <- 1174


icu_calc <- function(df){
  s <- 0
  icu_patients <- c()
  for(ii in 1:nrow(df)){
    s <- s+df$Inc_pred[ii]
    if(ii> icu_period){
      s <- s-df$Inc_pred[ii-icu_period]
    }
    icu_patients <- c(icu_patients,s)
  }
  df$Inc_period <- icu_patients
  df$Inc_period
}

# type =  "beta_90"
type = "beta_365"

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

type = "gamma_365"

plot_df_30_gamma <- readRDS(paste0("outputs/rds/df/df_30_inc_",type,".rds"))
plot_df_10_gamma <- readRDS(paste0("outputs/rds/df/df_10_inc_",type,".rds"))
plot_df_20_gamma <- readRDS(paste0("outputs/rds/df/df_20_inc_",type,".rds"))


plot_df_10_gamma$Inc_pred <- (plot_df_10_gamma$Inc_pred*icu_rate)
plot_df_20_gamma$Inc_pred <- (plot_df_20_gamma$Inc_pred*icu_rate)
plot_df_30_gamma$Inc_pred <- (plot_df_30_gamma$Inc_pred*icu_rate)


icu_10_gamma <- icu_calc(plot_df_10_gamma)
icu_20_gamma <- icu_calc(plot_df_20_gamma)
icu_30_gamma <- icu_calc(plot_df_30_gamma)

icu_df <- cbind("Dates" = plot_df_no$Dates,"No Change" = icu_no_change, "10% Reduction\nof Beta" = icu_10_beta,
              "20% Reduction\nof Beta" = icu_20_beta,"30% Reduction\nof Beta" = icu_30_beta,
              "10% Increase\nof Gamma" = icu_10_gamma,"20% Increase\nof Gamma" = icu_20_gamma,
              "30% Increase\nof Gamma" = icu_30_gamma)


icu_df<- as.data.frame(icu_df)
max_vals <- (apply(icu_df[,-1],2,max))
max_dates <- apply(icu_df[,-1],2,function(x){which(x == max(x))})
max_dates <- icu_df[max_dates,1]

max_vals <- cbind("dates" = max_dates,'Max_val'=max_vals)

max_vals <- as.data.frame(max_vals)
# rownames(max_vals) <- NULL
max_vals$dates <- as.Date(max_vals$dates,origin='1970-01-01')

icu_df <- icu_df%>% gather('int_type','icu',2:8)
icu_df$Dates <- as.Date(icu_df$Dates,origin='1970-01-01')

color_val <- c("No Change" = "red",
               "10% Reduction\nof Beta" = "orange", "20% Reduction\nof Beta" = "#b3ff00",
               "30% Reduction\nof Beta" = "#096e02")

color_val <- c(color_val,
               "10% Increase\nof Gamma" = "orange", "20% Increase\nof Gamma" = "#b3ff00",
               "30% Increase\nof Gamma" = "#096e02")


color_val <- c("orange","orange","#b3ff00","#b3ff00","#096e02","#096e02","red")
# names(color_val) <- icu_df$x

line_val <- c(2,1,2,1,2,1,1)
# names(line_val) <- names(color_val)

g_icu <- ggplot(data = icu_df,aes(x = Dates,y=icu,group=int_type,col=int_type,lty=int_type),se=F) +
  geom_rect(xmin=0,xmax=Inf,ymin=0,ymax=total_icu,fill="#9eff99")+
  geom_line()+
  # geom_hline(yintercept = max_vals)+
  labs(x = "Dates",y = "Number of patients in ICU(Daily)")+
  scale_color_manual(name="Intervention Type",values = color_val)+
  scale_linetype_manual(name="Intervention Type",values = line_val)+
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

for(ii in 1:nrow(max_vals)){
  # print(geom_text(aes(x=!!(max_vals[ii,1]), y= !!(max_vals[ii,2]), label=!!(round(max_vals[ii,2])) )))
  if(ii == 3){
    g_icu <- g_icu+
      geom_text(aes(x=!!(max_vals[ii,1]), y= !!(max_vals[ii,2]), label=!!(round(max_vals[ii,2]))) ,
                vjust=.2,size=4,col="black")
  }
  else{
    g_icu <- g_icu+
      geom_text(aes(x=!!(max_vals[ii,1]), y= !!(max_vals[ii,2]), label=!!(round(max_vals[ii,2]))) ,
                vjust=-.8,size=4,col="black")
  }
  
}
  
  
# apply(array, margin, ...)

type = paste0("comb",icu_period)

tiff(paste0("outputs/img/icu/icu_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_icu)
dev.off()