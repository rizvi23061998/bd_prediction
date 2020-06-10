dfs <- vector( "list",4)

icu_rate <- 0.031 * 0.196 
rec_type <- "both"

icu_df_both <- c()
int_names <- c("No Change",
               paste0("10% Decrease(","Beta","),\n10% increase(","Gamma",")"),
               paste0("10% Decrease(","Beta","),\n20% increase(","Gamma",")"),
               paste0("20% Decrease(","Beta","),\n10% increase(","Gamma",")"),
               paste0("20% Decrease(","Beta","),\n20% increase(","Gamma",")")
               ) 


l <- 1
for (ii in 2:3){
  for (jj in 2:3){
    id <- paste(ids[ii],"_",ids2[jj],"_",rec_type,sep = "")
    dfs[[l]] <- readRDS(paste0("outputs/rds/df/df_",id,".rds"))
    
    # dfs[[l]]$Inc <- cumsum(dfs[[l]]$Inc)
    dfs[[l]]$Inc_pred <- (dfs[[l]]$Inc_pred) * icu_rate
    
    icu_df_both <- cbind(icu_df_both, icu_calc(dfs[[l]]))
    colnames(icu_df_both)[l] <- int_names[l+1]
    l <- l+1
  }
}

type = "beta_365"
plot_df_no <- readRDS(paste0("outputs/rds/df/df_no_red_",type,".rds"))
plot_df_no$Inc_pred <- plot_df_no$Inc_pred * icu_rate

icu_df_both <- cbind("Dates"=plot_df_no$Dates,icu_calc(plot_df_no), icu_df_both)
colnames(icu_df_both)[2] <- int_names[1]

icu_df <- as.data.frame(icu_df_both)

max_vals <- (apply(icu_df[,-1],2,max))
max_dates <- apply(icu_df[,-1],2,function(x){which(x == max(x))})
max_dates <- icu_df[max_dates,1]

max_vals <- cbind("dates" = max_dates,'Max_val'=max_vals)

max_vals <- as.data.frame(max_vals)
# rownames(max_vals) <- NULL
max_vals$dates <- as.Date(max_vals$dates,origin='1970-01-01')


icu_df <- icu_df%>% gather('int_type','icu',2:6)
icu_df$Dates <- as.Date(icu_df$Dates, origin='1970-01-01')

color_val_both <- c( "red",
  "skyblue2",  "orange",
  "#b3ff00", "#096e02")
names(color_val_both) <- int_names

g_icu <- ggplot(data = icu_df,aes(x = Dates,y=icu,group=int_type,col=int_type),se=F) +
  geom_rect(xmin=0,xmax=Inf,ymin=0,ymax=total_icu,fill="#9eff99")+
  geom_line()+
  labs(x = "Dates",y = "Number of patients in ICU(Daily)")+
  scale_color_manual(name="Intervention\nType",values = color_val_both)+
  # scale_linetype_manual(name="Intervention Type",values = line_val)+
  
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # theme_bw()+
  # guides(col=guide_legend(nrow = 2,byrow = F))+
  # theme(
  #   legend.key.size =   unit(.8,"cm"),legend.position = "right",
  #       legend.title = element_text(colour = "red"),
  #       legend.spacing = unit(.5,"cm") 
  #       )+
  theme(legend.key = element_rect(fill="white",
                                  linetype = "solid",size = .2),
        legend.key.size =   unit(1.8,"line"),
        legend.position = c(.82,.80),
        legend.text=element_text(size = 10),
        legend.title = element_text(colour = 'red',size=13),
        legend.title.align =  .5,
        axis.text  = element_text(size=12),
        axis.title = element_text(colour ="navyblue",size=14),
        legend.box.spacing = unit(.1,"line"),
        
  )


for(ii in 1:nrow(max_vals)){
  # print(geom_text(aes(x=!!(max_vals[ii,1]), y= !!(max_vals[ii,2]), label=!!(round(max_vals[ii,2])) )))
  if(ii == 9){
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


type = paste0("both",icu_period)

tiff(paste0("outputs/img/icu/icu_",type,".tiff"),res = 300,height = 2000,width = 2000,unit = "px")
print(g_icu)
dev.off()