lapply(str_split(nucmerr$time, pattern = "-"), function(x){x[2]})

month <- 9

nucmer_list <- list()

for (i in 1:8) {
  nucmer_list[[i]] <- subset(nucmerr, str_split(nucmerr$time, pattern = "-", simplify = T)[,2] == paste("0", as.character(i+1), sep = ""))
  }

length(unique(nucmerr$ID))
mut_list <- data.frame(month = paste("2020-0", 2:9, sep = ""), 
                       mutation_rate = sapply(nucmer_list, function(x){round(nrow(x)/length(unique(x$ID)), 4)}), 
                       sample = sapply(nucmer_list, function(x){length(unique(x$ID))})
                       )

  library(ggplotify)
  library(eoffice)
  p<- as.ggplot(~boxplot(cars$speed)) #把图变成ggplot对象(),library(ggplotify)
  
  PPT_file='mut.pptx'  #接受图形的ppt文件
  
  topptx(p,PPT_file)
  
  library(rvcheck) # 直接打开文件，查看
  o(PPT_file)
 
# graph2ppt("plot")

  #小提琴图
  library(ggplot2)
  library(ggsci)
  library(ggpubr)
  nucmerr <- nucmerr[!duplicated(nucmerr), ]
  nucmerr<-nucmerr[!nucmerr$qvar%in%c("B","D","H","K","M","N","R","S","V","W","Y", "U","UU"),]
  
  #nucmerr统一月份
  nucmerr$month <- ""
  nucmerr[str_split(nucmerr$time, pattern = "-", simplify = T)[,2] == "02", ]$month <- "2020-02"
  str_split(nucmerr$time, pattern = "-", simplify = T)[,2] == "02"
  
  count<- as.data.frame(table(nucmerr$ID))
  colnames(count)[1] <- "ID"
  count$time <-sapply(strsplit(as.character(count$ID), "[|]"), function(x) x[3])
  count$month <-sapply(strsplit(as.character(count$time), "[-]"), function(x) paste("2020-",x[2], sep = ""))
  dat<- subset(count, str_split(count$month, pattern = "-", simplify = T)[,2] %in% c("02", "03","04", "05","06", "07","08","09"))
  
  ggplot(dat,aes(month,Freq,fill=month))+
    geom_point(shape = 21,size=1,width = 0.3, alpha = 0.3)+
     geom_violin(position = position_dodge(width = 1.75),
                 size = 0.4,alpha = 1,trim = T, width = 1.1)+
    geom_boxplot(outlier.colour = NA,notch = T,size = 0.2, alpha = 1, width = 0.4)+
    theme_classic()+
    theme(legend.position = 'none',
          axis.title.y = element_text(size=12),
          axis.text = element_text(size=12),
          axis.title.x = element_blank())+
    scale_fill_lancet()
    scale_fill_npg()

    
    
#山脊图
    library(ggplot2)
    library(ggridges)  # 绘制山脊图
    library(dplyr)  # 数据处理
    library(reshape) # 数据处理
    library(viridis)  # 调色板

    
    
    windowsFonts('yh' = '微软雅黑')
    
    ggplot(iris) +
      geom_density_ridges_gradient(
        aes(x = Sepal.Length, y = Species, fill = ..x..,height = ..density..), 
        scale = 1, rel_min_height = 0.01) +
      theme_ridges(font_size = 13, grid = TRUE) +
      theme(
        text = element_text(family = 'yh'),
        legend.position = 'right'
      ) +
      scale_y_discrete(name = '', expand = c(0.01, 0)) +
      scale_x_continuous(name = '', expand = c(0.01, 0)) +
      scale_fill_viridis(name = "", option = "C") 
    
    ggplot(dat) +
      geom_density_ridges_gradient(
        aes(x = Freq, y = month, fill = ..x..,height = ..density..), 
        scale = 1, rel_min_height = 0.01) +
      theme_ridges(font_size = 13, grid = TRUE) +
      theme(
        text = element_text(family = 'yh'),
        legend.position = 'right'
      ) +
      scale_y_discrete(name = '', expand = c(0.01, 0)) +
      scale_x_continuous(name = '', expand = c(0.01, 0)) +
      scale_fill_viridis(name = "", option = "C")

    #山脊图调节配色
    library(ggplot2)
    library(ggridges)
    library(RColorBrewer)

    Colormap<- colorRampPalette(rev(brewer.pal(11,'RdYlBu')[c(1,2,3,9,10,11)]))(32)
    ggplot(dat, aes(x = log2(Freq), y = month,fill = 0.5 - abs(0.5 - stat(ecdf)))) +
      stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
      scale_fill_viridis_c(name = "Tail probability", direction = -1)+
      theme_bw()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())#remove grid
    #fill density(color)
    p<- ggplot(dat, aes(x = log2(Freq), y = month, fill = ..density..)) + 
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.00,size = 0.3) + 
      scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32))+
      theme_bw()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
       
    #check hyper-mutated samples in early months:
    high_mut03<- dat[dat$Freq >80 & dat$month == "2020-03", ]
    
 
    
    
    