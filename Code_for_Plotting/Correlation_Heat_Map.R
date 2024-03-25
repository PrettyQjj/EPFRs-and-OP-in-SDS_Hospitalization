library(readxl)
data1<- read_excel("E:/R/data1.xlsx")
data2<- read_excel("E:/R/data2.xlsx")
library(ggcor)
library(dplyr)
library(vegan)
library(ggplot2)
set.seed(11)
mantel <- mantel_test(data2, data1, mantel.fun = 'mantel.randtest',spec.dist.method = 'bray', env.dist.method = 'euclidean', 
                      spec.select = list(EPFRsv = 1,EPFRsm= 2,DTTv= 3,DTTm= 4
                      )) %>% 
    mutate(
        r_value = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf), 
                      labels = c('<0.25', '0.25-0.5', '>=0.5'), right = FALSE),
        p_value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                      labels = c('<0.001', '0.001-0.01', '0.01-0.05', '>=0.05'), right = FALSE))

quickcor(data1, type = "upper") +
    geom_square() +
    anno_link(aes(colour = p_value, size = r_value,linetype = p_value), data = mantel) +
    scale_size_manual(values = c(1, 2, 3)) + 
    scale_colour_manual(values = c("#D95F02", "#1B9E77", "#A2A2A288")) +
    scale_fill_gradient2( low = "#7FFF00", mid = "white",high = "#006400", space = "Lab" )+
    guides(size = guide_legend(title = "Mantel's r",
                               override.aes = list(colour = "grey35"), 
                               order =2),
           colour = guide_legend(title = "Mantel's p", 
                                 override.aes = list(size = 3), 
                                 order = 1),
           fill = guide_colorbar(title = "Pearson's r", order = 3))
