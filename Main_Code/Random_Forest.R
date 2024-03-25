library(data.table)
library(Boruta)
library(randomForest)
library(ggplot2)
library(readxl)
library(ggpubr)
library(ggExtra)
library(ggpmisc)

data_Dietary <- read_excel("E:/R/E144D.xlsx")
data_Dietary2 <- read_excel("E:/R/E16D.xlsx")
data_Dietary3 <- read_excel("E:/R/EPFRsXD.xlsx")

str(data_Dietary )
names(data_Dietary)

#Boruta
set.seed(3)
boruta.train <- Boruta(Y ~ ., data = data_Dietary, doTrace = 2)#
print(boruta.train)
getSelectedAttributes(boruta.train, withTentative = F)
boruta.df <- attStats(boruta.train)
class(boruta.df)
print(boruta.df)
write.csv(boruta.df, "E:/R/boruta.df.csv")

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
    boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.85)
set.seed(11)
boruta.df <- read.csv("E:/R/boruta.df.csv")
boruta_s <- subset(boruta.df,boruta.df$decision=="Confirmed")
#head
head(boruta_s)
head(data)
write.csv(data_Dietary,"E:/R/data_model.csv")
data <- data.frame(data_Dietary[,c(1:16)], Y= data_Dietary [,17])
head(data)
write.csv(data,"E:/R/data_model.csv")

#outVals = boxplot(data[,'Y'], plot=FALSE)$out
#data <- data[-(which(data[,'Y'] %in% outVals)),]

data2 <- data.frame(data_Dietary2[,c(1:16)], Y= data_Dietary2[,17])
data3 <- data.frame(data_Dietary3[,c(1:16)], Y= data_Dietary3[,17])
train_sample <- sample(nrow(data),nrow(data))
n=floor(nrow(data)/10)
for (j in 1:10) {
    data_test <- data[train_sample[((j-1)*n+1):(j*n)],]
    data_train <- data[-train_sample[((j-1)*n+1):(j*n)],]
    m_rf <- randomForest(subset(data_train,select=- Y), data_train$ Y, ntree =1100, mtry = 15, importance = T)
    print(m_rf)
    
    save(m_rf, file = paste("rfmodel",j,".RData",sep = ""))
    write.csv(m_rf$importance,paste("importance",j,".csv",sep = ""))
    
    p_rf <- predict(m_rf, subset(data_test,select=- Y), type = "response")
    summary(data_test$ Y)
    summary(p_rf)
    cor(data_test$ Y, p_rf)
    data_p <- data.frame(predict=p_rf, Y =data_test$ Y)
    write.csv(data_p, paste("prediction",j,".csv",sep = ""))
    
    pp_rf <- predict(m_rf, subset(data2 ,select=- Y), type = "response")
    summary(data2$ Y)
    summary(pp_rf)
    cor(data2$ Y, pp_rf)
    data2_p <- data.frame(predict=pp_rf, Y =data2$ Y)
    write.csv(data2_p, paste("validate",j,".csv",sep = ""))
    
    
    ppp_rf <- predict(m_rf, subset(data_train,select=- Y), type = "response")
    summary(data_train$ Y)
    summary(ppp_rf)
    cor(data_train$ Y, ppp_rf)
    data_ppp <- data.frame(predict=ppp_rf, Y =data_train$ Y)
    write.csv(data_ppp, paste("train",j,".csv",sep = ""))
    
    
    pppp_rf <- predict(m_rf, subset(data3 ,select=- Y), type = "response")
    summary(data3$ Y)
    summary(pppp_rf)
    cor(data3$ Y, pppp_rf)
    data3_p <- data.frame(predict=pppp_rf, Y =data3$ Y)
    write.csv(data3_p, paste("All",j,".csv",sep = ""))
    
    
    g<-ggplot(data_p, aes(predict, Y)) +
        geom_point() +
        geom_smooth(method='lm') +
        theme_classic()+
        stat_poly_eq(
            aes(label =paste( ..adj.rr.label.., sep = '~~')),
            formula = y ~ x,  parse = TRUE,
            family="serif",
            size = 6.4,
            color="black",
            label.x = 0.1,  
            label.y = 1)+
        xlab("Predicted Concentration") +
        ylab("Observed Concentration")
    png(paste(j,".png",sep = ""))
    print(g)
    dev.off()
    
    
    
    g<-ggplot(data2_p, aes(predict, Y)) +
        geom_point() +
        geom_smooth(method='lm') +
        theme_classic() +
        stat_poly_eq(
            aes(label =paste( ..adj.rr.label.., sep = '~~')),
            formula = y ~ x,  parse = TRUE,
            family="serif",
            size = 6.4,
            color="black",
            label.x = 0.1,  
            label.y = 1)+
        xlab("Predicted Concentration") +
        ylab("Observed Concentration")
    png(paste(j+10,".png",sep = ""))
    print(g)
    dev.off()
    
    g<-ggplot(data_ppp, aes(predict, Y)) +
        geom_point() +
        geom_smooth(method='lm') +
        theme_classic()+
        stat_poly_eq(
            aes(label =paste( ..adj.rr.label.., sep = '~~')),
            formula = y ~ x,  parse = TRUE,
            family="serif",
            size = 6.4,
            color="black",
            label.x = 0.1,  
            label.y = 1)+
        xlab("Predicted Concentration") +
        ylab("Observed Concentration")
    png(paste(j+20,".png",sep = ""))
    print(g)
    dev.off()
    
}

