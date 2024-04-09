################
# Forest plots #
################
library(ggplot2)
library(readxl)

EPFRs_TSP = as.data.frame(read_excel("E:/R/Meta/Forest.xlsx", sheet = "EPFRs"))

EPFRs_TSP$Significance = NA
EPFRs_TSP[EPFRs_TSP$pval == "<0.001", "Significance"] = "p<0.05"
EPFRs_TSP[EPFRs_TSP$pval >= "0.05", "Significance"] = "ns"
EPFRs_TSP[EPFRs_TSP$pval < "0.05", "Significance"] = "p<0.05"

EPFRs_TSP$Variable = factor(EPFRs_TSP$Variable,
                               levels = EPFRs_TSP[order(EPFRs_TSP$HR, decreasing = F), "Variable"])

EPFRs_TSP$logHR = log2(EPFRs_TSP$HR)
EPFRs_TSP$logLowerCI = log2(EPFRs_TSP$LowerCI)
EPFRs_TSP$logUpperCI = log2(EPFRs_TSP$UpperCI)

### forest plot with log x-scale
ggplot(data = EPFRs_TSP) +
    geom_point(mapping = aes(x = HR, y = Variable, group = Significance, color = Significance), size = 4, shape = 19) +
    
    geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI, y = Variable), height = 0.2, size = 0.5) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    annotate(geom = "text", x = rep(1.08, length(EPFRs_TSP$Variable)), 
             y = EPFRs_TSP$Variable, label = EPFRs_TSP$pval, size = 5) +
    coord_cartesian(xlim = c(0.93, 1.1), clip = "off") +
    xlab("RR (95% CI)") + ylab("Cause of hospitalization\n") +
    theme(axis.title.x = element_text(size = 18, vjust = 0, face = "bold"),
          axis.text = element_text(size = 14, face = "bold"), axis.title.y = element_text(size = 18, face = "bold"),
          axis.line = element_line(color = "black"), panel.background = element_rect(fill = "white"),
          plot.margin = unit(c(1,6,1,1), "lines"), title = element_text(face = "bold", size = 16),
          legend.text = element_text(size = 18), legend.title = element_text(size = 16),
          legend.position = "bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, face = "plain")) 
#dev.off()
