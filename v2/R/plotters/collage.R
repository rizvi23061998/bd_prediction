library(ggpubr)

ga4 <- readRDS(paste0("outputs/rds/img/icu","both4",".rds"))
gb4 <- readRDS(paste0("outputs/rds/img/icu","comb4",".rds"))
gc4 <- readRDS(paste0("outputs/rds/img/icu","beta_seg_60_2_4",".rds"))
gd4 <- readRDS(paste0("outputs/rds/img/icu","beta_seg_90_2_4",".rds"))

ga9 <- readRDS(paste0("outputs/rds/img/icu","both9",".rds"))
gb9 <- readRDS(paste0("outputs/rds/img/icu","comb9",".rds"))
gc9 <- readRDS(paste0("outputs/rds/img/icu","beta_seg_60_2_9",".rds"))
gd9 <- readRDS(paste0("outputs/rds/img/icu","beta_seg_90_2_9",".rds"))


ga <- ggarrange(ga4,ga9,labels = c("(i)","(v)"),label.x = .4,label.y = 1,nrow = 2,ncol = 1,common.legend = T,legend = "bottom")

gb <- ggarrange(gb4,gb9,labels = c("(ii)","(vi)"),label.x = .4,label.y = 1,nrow = 2,ncol = 1)

gc <- ggarrange(gc4,gc9,labels = c("(iii)","(viii)"),label.x = .4,label.y = 1,nrow = 2,ncol = 1)

gd <- ggarrange(gd4,gd9,labels = c("(iv)","(viii)"),label.x = .4,label.y = 1,nrow = 2,ncol = 1)
g <- ggarrange(ga,gb,nrow=1,ncol=2)
tiff("test.tiff",height = 3000,width=3000,res = 300,units = "px")
print(g)
dev.off()