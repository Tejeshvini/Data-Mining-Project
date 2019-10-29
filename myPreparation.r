library(readr)
ilpd1 <- read_csv("C:/Users/tejes/UQ/Sem-2/INFS7203/Indian-Liver-Patient-Dataset-(ILPD).csv",col_names = FALSE)
ilpd <- data.frame(ilpd1)
names(ilpd) <- c("Age", "Gender", "TB", "DB","Alkphos","Sgpt","Sgot","TP","Albumin","AG_Ratio","Class")
head(ilpd)
#replace missing values of AG_Ratio with the median of non-missing values of AG_Ratio
ilpd$AG_Ratio[is.na(ilpd$AG_Ratio)] <- median(ilpd$AG_Ratio, na.rm=TRUE)
ilpd$AG_Ratio
#replace the value 2 in the class column with 0
ilpd$Class[ilpd$Class==2] <- 0
#convert the class column to integer type
ilpd$Class<-as.numeric(Class)
saveRDS(ilpd,file="ilpd_preprocessed.Rda")

