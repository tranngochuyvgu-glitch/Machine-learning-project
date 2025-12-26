
# SET WORKING DIRECTORY

setwd("C:/bm")

getwd()

# IMPORT REQUIRED LIBRARIES

library(readxl)

library(dplyr)

library(ggplot2)

library(cowplot)

library(ggpubr)

library(ClusterR)

library(cluster)

library(factoextra)

library(plotly)

# LOAD IN DATASET

fin_data <- read_excel("NHOM_1_YEARLY.xlsx")

# DATA CLEANING

## Turn the first row into column names

colnames(fin_data) <- fin_data[1,]

## Remove the first two rows

fin_data <- fin_data[-c(1, 2),]

## Remove the "items" column

fin_data <- subset(fin_data, select = -c(items))

## Set column names for the first 2 columns

colnames(fin_data)[1:2] <- c("ticker", "year")

## Create a dataframe which maps each financial variable to a shortened name

mapping.name <- data.frame(Variable = c(names(fin_data[0, 3:375])),
                                        Name = c(paste0("f", 1:373))) 

## Shorten column name for all financial variables

colnames(fin_data)[3:375] <- c(paste0("f", 1:373))

## Convert all columns except for ticker & year into numeric type

fin_data[,-c(1, 2)] <- apply(fin_data[,-c(1, 2)], 2,            
                    function(x) as.numeric(as.character(x)))

## Check total NA values

sum(is.na(fin_data))

## Check NA values by column

na.list <- stack(sapply(fin_data, function(x) sum(is.na(x))))

na.list <- na.list[, c(2,1)]

## Round numbers to 2 decimals

fin_data[,-c(1, 2)] <- round(fin_data[,-c(1, 2)], digits = 2)

# EXPLORATORY DATA ANALYSIS

## Count the number of unique tickers or companies

length(unique(fin_data$ticker))

# CREATE RELEVANT FINANCIAL VARIABLES

## EBITDA

## EBITDA = Sales - COGS - Chi phi ban hang - Chi phi quan ly DN + Depreciation
## EBITDA = f125 - f126 - f132 - f133 + f176

fin_data$EBITDA <- fin_data$f125 + fin_data$f126 + fin_data$f132 + fin_data$f133 
+ fin_data$f176

## DEBT

## DEBT = Short-term Debt + Long-term debt + Trai phieu chuyen doi + Co phieu uu dai
## DEBT = f78 + f91 + f92 + f93

fin_data$DEBT <- fin_data$f78 + fin_data$f91 + fin_data$f92 + fin_data$f93

## FFO

## FFO = LN tu HDKD truoc thay doi VLD + Total Interest Paid + Total Tax Paid
## FFO = f185 + f191 + f192

fin_data$FFO <- fin_data$f185 + fin_data$f191 + fin_data$f192

## Create 2 new financial ratios based on these 3 financial variables

fin_data$EBITDA.DEBT <- fin_data$EBITDA / fin_data$DEBT

fin_data$FFO.DEBT <- fin_data$FFO / fin_data$DEBT

## Check NA values

sum(is.na(fin_data$EBITDA.DEBT))

sum(is.na(fin_data$FFO.DEBT))

## Plot these new variables and ratios to check their distributions

### EBITDA

summary(fin_data$EBITDA)

plot(density(fin_data$EBITDA, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of EBITDA', xlab='EBITDA')

plot(density(fin_data$EBITDA, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of EBITDA', xlab='EBITDA',
     xlim = c(-1000, 1000))

### DEBT

summary(fin_data$DEBT)

plot(density(fin_data$DEBT, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of DEBT', xlab='DEBT')

plot(density(fin_data$DEBT, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of DEBT', xlab='DEBT',
     xlim = c(-1000, 2000))

### FFO

summary(fin_data$FFO)

plot(density(fin_data$FFO, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of FFO', xlab='FFO')

plot(density(fin_data$FFO, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of FFO', xlab='FFO',
     xlim = c(-1000, 1000))

### EBITDA/DEBT

summary(fin_data$EBITDA.DEBT)

plot(density(fin_data$EBITDA.DEBT, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of EBITDA/DEBT', xlab='EBITDA/DEBT')

plot(density(fin_data$EBITDA.DEBT, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of EBITDA/DEBT', xlab='EBITDA/DEBT',
     xlim = c(-100, 100))

hist(fin_data$EBITDA.DEBT, breaks = 5000, xlim = c(-5, 5))

### FFO/DEBT

summary(fin_data$FFO.DEBT)

plot(density(fin_data$FFO.DEBT, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of FFO/DEBT', xlab='FFO/DEBT')

plot(density(fin_data$FFO.DEBT, na.rm = TRUE), lwd = 3, col='red', 
     main='Density Plot of FFO/DEBT', xlab='FFO/DEBT',
     xlim = c(-50, 50))

hist(fin_data$FFO.DEBT, breaks = 100, xlim = c(-200, 200))

### 2D Density Plot with 2 variables EBITDA/DEBT and FFO/DEBT

ggplot(fin_data, aes(x=FFO.DEBT, y=EBITDA.DEBT)) +
  geom_point(color = "red") +
  theme_bw()

ggplot(fin_data, aes(x=FFO.DEBT, y=EBITDA.DEBT)) +
  geom_point(color = "red") +
  xlim(-2000, 2000) +
  theme_bw()

### Replace inf values with NA in EBITDA.DEBT and FFO.DEBT columns

fin_data[c('EBITDA.DEBT', 'FFO.DEBT')][sapply(fin_data[c('EBITDA.DEBT', 'FFO.DEBT')], 
                                              is.infinite)] <- NA

### Remove observations with NA values in either EBITDA.DEBT or FFO.DEBT

fin_data <- fin_data[!is.na(fin_data$EBITDA.DEBT),]

fin_data <- fin_data[!is.na(fin_data$FFO.DEBT),]


fin_data$scaled.ebitda.debt <- scale(fin_data$EBITDA.DEBT)

fin_data$scaled.ffo.debt <- scale(fin_data$FFO.DEBT)

summary(fin_data$EBITDA.DEBT)

hist(fin_data$scaled.ebitda.debt)

hist(fin_data$scaled.ffo.debt)

### Count the number of each year from 2000 to 2020

year.count <- as.data.frame(fin_data %>% group_by(year) %>% summarize(count=n()))

### Return the percentage numbers

year.count$percent <- year.count$count / sum(year.count$count)

percent.func <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

year.count$percent <- percent.func(year.count$percent)

### Plot the histogram of the number of each year

ggplot(year.count, aes(x=year, y=count)) + 
  geom_bar(stat="identity") + 
  theme_bw()

### Calculate the average throughout all years of each ticker/company

sum.data <- as.data.frame(fin_data %>% group_by(ticker) 
                          %>% summarise(across(c(scaled.ebitda.debt,
                                                     scaled.ffo.debt), mean)))

sum.data[,-1] <- round(sum.data[,-1], digits = 2)

summary(sum.data$scaled.ebitda.debt)

plot(density(sum.data$scaled.ebitda.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled EBITDA/DEBT', xlab='EBITDA/DEBT')

plot(density(fin_data$scaled.ebitda.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled EBITDA/DEBT', xlab='EBITDA/DEBT',
     xlim = c(-1, 1))

summary(sum.data$scaled.ffo.debt)

plot(density(sum.data$scaled.ffo.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled FFO/DEBT', xlab='FFO/DEBT')

plot(density(fin_data$scaled.ffo.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled FFO/DEBT', xlab='FFO/DEBT',
     xlim = c(-1, 1))

ggplot(sum.data, aes(x=scaled.ebitda.debt, y=scaled.ffo.debt)) +
  geom_point(color = "red") +
  theme_bw()

ggplot(sum.data, aes(x=scaled.ebitda.debt, y=scaled.ffo.debt)) +
  geom_point(color = "red") +
  xlim(-0.3, 5) + ylim(-5, 5) +
  theme_bw()

ggplot(fin_data, aes(x=FFO.DEBT, y=EBITDA.DEBT)) +
  geom_point(color = "red") +
  xlim(-10, 10) + ylim(-10, 10) +
  theme_bw()

### Check correlation between the 2 financial ratios

cor(sum.data$scaled.ebitda.debt, sum.data$scaled.ffo.debt)

cor(fin_data$EBITDA.DEBT, fin_data$FFO.DEBT)

### Attempting PCA on the 2 scaled financial ratios

mat <- sum.data[, c(2,3)]

pca <- prcomp(mat, scale=TRUE)

pca$rotation

summary(pca)

pca$x

plot(density(pca$x[, 1]), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1')

plot(density(pca$x[, 1]), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1',
     xlim = c(-1, 1))

pca.df <- as.data.frame(pca$x)

summary(pca.df[,1])

ggplot(pca.df, aes(x=PC1, y=0)) +
  geom_point(color = "red")

stripchart(pca.df$PC1, pch = 1, col = "red")

### Attempting K-Means Clustering

fin_kmeans <- kmeans(pca.df$PC1, centers=8)

print(fin_kmeans)

pca.df$k.pred <- as.factor(fin_kmeans$cluster)

ggplot(pca.df, aes(x = PC1, y = 0, color = k.pred)) +
  geom_point(size = 5) +
  theme_bw()

summary(pca.df[pca.df$k.pred == 1, ]$PC1)

summary(pca.df[pca.df$k.pred == 2, ]$PC1)

summary(pca.df[pca.df$k.pred == 3, ]$PC1)

summary(pca.df[pca.df$k.pred == 4, ]$PC1)

summary(pca.df[pca.df$k.pred == 5, ]$PC1)

summary(pca.df[pca.df$k.pred == 6, ]$PC1)

summary(pca.df[pca.df$k.pred == 7, ]$PC1)

summary(pca.df[pca.df$k.pred == 8, ]$PC1)

### Attempting Gaussian Mixture Model Clustering

### Fitting GMM model

fin_gmm = GMM(as.matrix(pca.df$PC1), 8, dist_mode = "maha_dist", 
              seed_mode = "random_subset") 

pca.df$pred = as.factor(predict(fin_gmm, newdata = as.matrix(pca.df$PC1)))

as.data.frame(table(pca.df$pred))

ggplot(pca.df, aes(x = PC1, y = 0, color = pred)) +
  geom_point(size = 5) +
  theme_bw()

summary(pca.df[pca.df$pred == 1, ]$PC1)

summary(pca.df[pca.df$pred == 2, ]$PC1)

summary(pca.df[pca.df$pred == 3, ]$PC1)

summary(pca.df[pca.df$pred == 4, ]$PC1)

summary(pca.df[pca.df$pred == 5, ]$PC1)

summary(pca.df[pca.df$pred == 6, ]$PC1)

summary(pca.df[pca.df$pred == 7, ]$PC1)

summary(pca.df[pca.df$pred == 8, ]$PC1)

fin_gmm$call

## ONLY CALCULATE FROM OBSERVATIONS BETWEEN 2015 TO 2019

## Retain only observations from 2015 to 2019 and then calculate the average

limited_fin_data <- fin_data[fin_data$year  %in% c(2015,2016,2017,2018,2019), ] 

### Calculate the average of each ticker/company in the limited data frame

sum.limited.data <- as.data.frame(limited_fin_data %>% group_by(ticker) 
                          %>% summarise(across(c(scaled.ebitda.debt,
                                                 scaled.ffo.debt), mean)))

summary(sum.limited.data$scaled.ebitda.debt)

summary(sum.limited.data$scaled.ffo.debt)

plot(density(sum.limited.data$scaled.ebitda.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled EBITDA/DEBT', xlab='EBITDA/DEBT')

plot(density(sum.limited.data$scaled.ebitda.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled EBITDA/DEBT', xlab='EBITDA/DEBT',
     xlim = c(-0.5, 0.5))

summary(sum.limited.data$scaled.ffo.debt)

plot(density(sum.limited.data$scaled.ffo.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled FFO/DEBT', xlab='FFO/DEBT')

plot(density(sum.limited.data$scaled.ffo.debt), lwd = 3, col='red', 
     main='Density Plot of Average Scaled FFO/DEBT', xlab='FFO/DEBT',
     xlim = c(-0.5, 0.5))

ggplot(sum.limited.data, aes(x=scaled.ebitda.debt, y=scaled.ffo.debt)) +
  geom_point(color = "red") +
  theme_bw()

ggplot(sum.limited.data, aes(x=scaled.ebitda.debt, y=scaled.ffo.debt)) +
  geom_point(color = "red") +
  xlim(-0.3, 5) + ylim(-2.5, 5) +
  theme_bw()

### Check correlation between the 2 financial ratios

cor(sum.limited.data$scaled.ebitda.debt, sum.limited.data$scaled.ffo.debt)

### Attempting PCA on the 2 scaled financial ratios

mat.lim <- sum.limited.data[, c(2,3)]

pca.lim <- prcomp(mat.lim, scale=TRUE)

pca.lim$rotation

summary(pca.lim)

pca.lim$x

plot(density(pca.lim$x[, 1]), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1')

plot(density(pca.lim$x[, 1]), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1',
     xlim = c(-1, 1))

pca.lim.df <- as.data.frame(pca.lim$x)

summary(pca.lim.df[,1])

ggplot(pca.lim.df, aes(x=PC1, y=0)) +
  geom_point(color = "red")

stripchart(pca.lim.df$PC1, pch = 1, col = "red")

### Attempting K-Means Clustering on the Limited Dataset from 2015 to 2019

lim_fin_kmeans <- kmeans(pca.lim.df$PC1, centers=8)

print(lim_fin_kmeans)

pca.lim.df$k.pred <- as.factor(lim_fin_kmeans$cluster)

ggplot(pca.lim.df, aes(x = PC1, y = 0, color = k.pred)) +
  geom_point(size = 5) +
  theme_bw()

summary(pca.lim.df[pca.lim.df$k.pred == 1, ]$PC1)

summary(pca.lim.df[pca.lim.df$k.pred == 2, ]$PC1)

summary(pca.lim.df[pca.lim.df$k.pred == 3, ]$PC1)

summary(pca.lim.df[pca.lim.df$k.pred == 4, ]$PC1)

summary(pca.lim.df[pca.lim.df$k.pred == 5, ]$PC1)

summary(pca.lim.df[pca.lim.df$k.pred == 6, ]$PC1)

summary(pca.lim.df[pca.lim.df$k.pred == 7, ]$PC1)

summary(pca.lim.df[pca.lim.df$k.pred == 8, ]$PC1)

### Attempting Gaussian Mixture Model Clustering on the Limited Dataset from 2015 to 2019

### Fitting GMM model on the Limited Dataset from 2015 to 2019

lim_fin_gmm = GMM(as.matrix(pca.lim.df$PC1), 8, dist_mode = "maha_dist", 
              seed_mode = "random_subset") 

pca.lim.df$g.pred = as.factor(predict(lim_fin_gmm, newdata = as.matrix(pca.lim.df$PC1)))

as.data.frame(table(pca.lim.df$g.pred))

ggplot(pca.lim.df, aes(x = PC1, y = 0, color = g.pred)) +
  geom_point(size = 5) +
  theme_bw()

summary(pca.lim.df[pca.lim.df$g.pred == 1, ]$PC1)

summary(pca.lim.df[pca.lim.df$g.pred == 2, ]$PC1)

summary(pca.lim.df[pca.lim.df$g.pred == 3, ]$PC1)

summary(pca.lim.df[pca.lim.df$g.pred == 4, ]$PC1)

summary(pca.lim.df[pca.lim.df$g.pred == 5, ]$PC1)

summary(pca.lim.df[pca.lim.df$g.pred == 6, ]$PC1)

summary(pca.lim.df[pca.lim.df$g.pred == 7, ]$PC1)

summary(pca.lim.df[pca.lim.df$g.pred == 8, ]$PC1)

### Try checking for normality in each cluster

plot(density(pca.lim.df[pca.lim.df$g.pred == 3, ]$PC1), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1',
     xlim = c(-0.2, 0.1))

plot(density(pca.lim.df[pca.lim.df$g.pred == 6, ]$PC1), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1',
     xlim = c(-0.3, 0.3))

plot(density(pca.lim.df[pca.lim.df$g.pred == 7, ]$PC1), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1',
     xlim = c(-0.12, -0.07))

plot(density(pca.lim.df[pca.lim.df$g.pred == 8, ]$PC1), lwd = 3, col='red', 
     main='Density Plot of PCA 1', xlab='PCA 1',
     xlim = c(-3, 3))

### Attempting K-means using both non-scaled financial ratios

# limited_fin_data[, c("EBITDA.DEBT", "FFO.DEBT")]

two.sum.limited.data <- as.data.frame(limited_fin_data %>% group_by(ticker) 
                                  %>% summarise(across(c(EBITDA.DEBT,
                                                         FFO.DEBT), mean)))

two_lim_fin_kmeans <- kmeans(two.sum.limited.data[, c("EBITDA.DEBT", "FFO.DEBT")], 
                             centers=8)

print(two_lim_fin_kmeans)

two.sum.limited.data$k.pred <- as.factor(two_lim_fin_kmeans$cluster)

ggplot(two.sum.limited.data, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = k.pred)) +
  geom_point(size = 3.5) +
  theme_bw()

summary(limited_fin_data[limited_fin_data$k.pred == 1, ]$EBITDA.DEBT)

summary(limited_fin_data[limited_fin_data$k.pred == 2, ]$EBITDA.DEBT)

summary(limited_fin_data[limited_fin_data$k.pred == 3, ]$EBITDA.DEBT)

summary(limited_fin_data[limited_fin_data$k.pred == 4, ]$EBITDA.DEBT)

summary(limited_fin_data[limited_fin_data$k.pred == 5, ]$EBITDA.DEBT)

summary(limited_fin_data[limited_fin_data$k.pred == 6, ]$EBITDA.DEBT)

summary(limited_fin_data[limited_fin_data$k.pred == 7, ]$EBITDA.DEBT)

summary(limited_fin_data[limited_fin_data$k.pred == 8, ]$EBITDA.DEBT)


lim.fin.1 = two.sum.limited.data[two.sum.limited.data$k.pred == 2, ]

lim.clust.1 <- kmeans(lim.fin.1[, c("EBITDA.DEBT", "FFO.DEBT")], 6)$cluster %>%
  as.factor()

lim.fin.1 <- lim.fin.1 %>%
  mutate(k.pred.1 = lim.clust.1)

as.data.frame(table(lim.fin.1$k.pred.1))

ggplot(lim.fin.1, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = k.pred.1)) +
  geom_point(size = 3) +
  theme_bw()


lim.fin.2 = lim.fin.1[lim.fin.1$k.pred.1 == 2 |
                        lim.fin.1$k.pred.1 == 5, ]
lim.clust.2 <- kmeans(lim.fin.2[, c("EBITDA.DEBT", "FFO.DEBT")], 6)$cluster %>%
  as.factor()

lim.fin.2 <- lim.fin.2 %>%
  mutate(k.pred.2 = lim.clust.2)

as.data.frame(table(lim.fin.2$k.pred.2))

ggplot(lim.fin.2, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = k.pred.2)) +
  geom_point(size = 3) +
  theme_bw()

summary(lim.fin.2[lim.fin.2$k.pred.2 == 1, ]$EBITDA.DEBT)

summary(lim.fin.2[lim.fin.2$k.pred.2 == 2, ]$EBITDA.DEBT)

summary(lim.fin.2[lim.fin.2$k.pred.2 == 3, ]$EBITDA.DEBT)

summary(lim.fin.2[lim.fin.2$k.pred.2 == 4, ]$EBITDA.DEBT)

summary(lim.fin.2[lim.fin.2$k.pred.2 == 5, ]$EBITDA.DEBT)


lim.fin.3 = lim.fin.2[lim.fin.2$k.pred.2 == 5, ]

lim.clust.3 <- kmeans(lim.fin.3[, c("EBITDA.DEBT", "FFO.DEBT")], 4)$cluster %>%
  as.factor()

lim.fin.3 <- lim.fin.3 %>%
  mutate(k.pred.3 = lim.clust.3)

as.data.frame(table(lim.fin.3$k.pred.3))

ggplot(lim.fin.3, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = k.pred.3)) +
  geom_point(size = 3) +
  theme_bw()


### Computing and merging all final predictions based on the third K-Means 
### implementation into one single data frame

## Left join first K-means Table with the second K-means Table

final.kmeans.df <- merge(x = two.sum.limited.data, 
                        y = lim.fin.1[, c("ticker", "k.pred.1")], 
                        by = "ticker", all.x = TRUE)

## Left join combined K-means Table with the third K-means Table

final.kmeans.df <- merge(x = final.kmeans.df,
                         y = lim.fin.2[, c("ticker", "k.pred.2")],
                         by = "ticker", all.x = TRUE)

## Assign final clusters to out-liers 

final.kmeans.df[final.kmeans.df$k.pred %in% c(3, 5, 8), ]$k.pred.2 <- as.factor(4)

final.kmeans.df[final.kmeans.df$k.pred %in% c(1, 4, 6, 7), ]$k.pred.2 <- as.factor(2)


final.kmeans.df[final.kmeans.df$k.pred.1 %in% c(4), ]$k.pred.2 <- as.factor(4)

final.kmeans.df[final.kmeans.df$k.pred.1 %in% c(1, 3, 6), ]$k.pred.2 <- as.factor(2)

### Visualization of all data points

ggplot(final.kmeans.df, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = k.pred.2)) +
  geom_point(size = 3) +
  theme_bw()

table(final.kmeans.df$k.pred.2)

### Companies of cluster 2

final.cluster.2 <- as.data.frame(final.kmeans.df[final.kmeans.df$k.pred.2 %in% c(2), ])

companies.cluster.2 <- c("CTCP Mỹ thuật và Truyền thông", "CTCP Chiếu xạ An Phú",
                         "CTCP Dịch vụ Hàng không Taseco", "CTCP Bông Bạch Tuyết",
                         "CTCP 397", "CTCP Khai Thác và Chế Biến khoáng Sản Bắc Giang",
                         "CTCP Nhựa Bình Minh", "CTCP Công trình Đô thị Bến Tre",
                         "Bao Viet Fund", "CTCP Cấp thoát nước và Xây dựng Bảo Lộc",
                         "CTCP Cấp nước Bà Rịa - Vũng Tàu", "CTCP Thế Kỷ 21",
                         "CTCP Cảng An Giang", "CTCP Lâm Nông sản Thực phẩm Yên Bái",
                         "CTCP Tư vấn Xây dựng Công nghiệp và Đô thị Việt Nam",
                         "Công ty TNHH MTV Dịch vụ công ích huyện Cần Giờ",
                         "CTCP CNG Việt Nam", "Tổng Công ty cổ phần Công trình Viettel",
                         "CTCP Cầu Xây", "CTCP Phát triển Đô thị Công nghiệp số 2",
                         "CTCP Xuất nhập khẩu Y Tế Domesco", "CTCP Cấp nước Đà Nẵng",
                         "CTCP Tập đoàn Điện Quang", "CTCP Công trình Đô thị Bảo Lộc",
                         "CTCP Đầu tư Phát triển Thành Đạt", "CTCP VICEM Vật liệu Xây dựng Đà Nẵng",
                         "Tổng Công ty Chuyển phát nhanh Bưu Điện - CTCP",
                         "CTCP Đầu tư và Phát triển Doanh nghiệp Việt Nam",
                         "CTCP Cơ khí đóng tàu Thủy sản Việt Nam", "CTCP Dệt may Gia Định",
                         "CTCP Vàng Lào Cai", "CTCP Kỹ thuật Điện Toàn cầu",
                         "CTCP Dịch vụ Du lịch Đường sắt Hà Nội", "CTCP Hacisco",
                         "CTCP Đầu tư Phát triển Nhà HUD2", "CTCP Bánh kẹo Hải Hà",
                         "CTCP Tập đoàn HIPT", "CTCP Bia và Nước giải khát Hạ Long",
                         "CTCP Sứ Kỹ thuật Hoàng Liên Sơn", "CTCP Hải Minh",
                         "CTCP Đầu tư và Công nghệ HVC", "CTCP Đầu tư Xây dựng Dầu khí IDICO",
                         "CTCP Phát triển Hạ tầng Vĩnh Phúc", "CTCP Thực phẩm Quốc tế",
                         "CTCP Dược phẩm Imexpharm", "CTCP Công nghệ Tiên Phong",
                         "CTCP Đầu tư Địa ốc Khang An", "CTCP Xuất khẩu Thủy sản Khánh Hòa",
                         "CTCP KASATI", "CTCP Licogi 14", "CTCP Đầu Tư và Xây dựng 40",
                         "CTCP Khoáng sản và Vật liệu Xây dựng Lâm Đồng",
                         "CTCP Bất động sản Điện lực Miền Trung",
                         "CTCP Đầu tư và Xây dựng Thủy lợi Lâm Đồng", "CTCP Miền Đông",
                         "Công ty Cổ phần Nhựa Y tế MEDIPLAST", "CTCP Dịch vụ Du lịch Mỹ Trà",
                         "CTCP Phân lân Ninh Bình", "CTCP Sản xuất và Thương mại Nam Hoa",
                         "CTCP Tập Đoàn Danh Khôi", 
                         "CTCP Sản xuất Kinh doanh Nước sạch Số 3 Hà Nội",
                         "CTCP Khu Công nghiệp Nam Tân Uyên", "CTCP Môi trường Đô thị Nha Trang",
                         "Nhà Xuất Bản Trẻ", "CTCP Xuất nhập khẩu Sản xuất Gia công và Bao bì",
                         "CTCP Đầu tư và Xây lắp khí",
                         "Tổng Công ty cổ phần Bia - Rượu - Nước giải khát Sài Gòn",
                         "CTCP Trục vớt Cứu hộ Việt Nam", "CTCP Dịch vụ Hàng hóa Sài Gòn",
                         "CTCP Đóng tàu Sông Cấm", "CTCP Sách Giáo dục tại Thành phố Hồ Chí Minh",
                         "CTCP Vận tải biển Sài Gòn", "Công ty du lịch Saigontourism",
                         "CTCP Đầu tư Tổng hợp Hà Nội", "CTCP Sara Việt Nam",
                         "CTCP Xuất nhập khẩu Thủy sản Sài Gòn", 
                         "CTCP Thương mại Bia Sài Gòn Sông Tiền", "CTCP Sonadezi Long Bình",
                         "CTCP Sonadezi Long Thành", "CTCP Năng lượng và Bất động sản Trường Thành",
                         "CTCP Phát triển Khu Công nghiệp Tín Nghĩa",
                         "CTCP Kim loại màu Thái Nguyên - Vimico", "CTCP Phân phối Top One",
                         "CTCP Transimex Logistics", "CTCP Traphaco",
                         "CTCP Vận tải và Dịch vụ Hàng hải", "CTCP Thủy Tạ",
                         "CTCP Dược phẩm TV.Pharm", "Tổng Công Ty Quản Lý Bay Việt Nam",
                         "CTCP Thương mại Dịch vụ VDA Đà Nẵng", "Tổng Công ty cổ phần May Việt Tiến",
                         "CTCP Mạ Kẽm Công Nghiệp Vingal - Vnsteel",
                         "CTCP Xây dựng và Sản xuất Vật liệu xây dựng Biên Hòa",
                         "Tổng Công ty Chăn Nuôi Việt Nam - CTCP", "CTCP Đầu tư Việt Việt Nhật",
                         "CTCP Sữa Việt Nam", "Tổng Công ty Chuyển phát nhanh Bưu Điện - CTCP",
                         "CTCP Quảng cáo và Hội chợ Thương mại", "CTCP Dịch vụ Vận tải Đường sắt",
                         "CTCP Đại lý Hàng hải Việt Nam", "CTCP Thương mại và Đầu tư VI NA TA BA",
                         "CTCP Thuốc Thú y Trung ương VETVACO", 
                         "CTCP May Thêu Giày Dép W.E.C Sài Gòn")


final.cluster.2$Company.Name <- companies.cluster.2

industries.cluster.2 <- c("Publishing", "Chemicals", "Restaurant", "Manufacturing",
                          "Mining", "Mining", "Manufacturing", "Environment & Facilities",
                          "Finance", "Environment & Facilities", "Environment & Facilities",
                          "Construction", "Transportation", "Manufacturing", 
                          "Professional Services", "Environment & Facilities",
                          "Utilities", "Information Technology", "Materials",
                          "Real Estate", "Health Care", "Utilities", "Consumer",
                          "Environment & Facilities", "Construction", "Materials",
                          "Transportation", "Real Estate", "Industrials", "Textile",
                          "Mining", "Information Technology", "Transportation",
                          "Construction", "Construction", "Consumer", 
                          "Information Technology", "Consumer", "Materials",
                          "Transportation", "Construction", "Construction",
                          "Construction", "Consumer", "Pharmaceuticals", 
                          "Information Technology", "Real Estate", "Consumer",
                          "Information Technology", "Construction", "Construction",
                          "Materials", "Real Estate", "Construction", "Mining",
                          "Materials", "Restaurant", "Materials", "Consumer",
                          "Real Estate", "Utilities", "Real Estate", 
                          "Environment & Facilities", "Media", "Consumer", 
                          "Consumer", "Consumer", "Transportation", "Transportation",
                          "Industrials", "Media", "Transportation", "Consumer",
                          "Materials", "Consumer", "Consumer", "Consumer", 
                          "Real Estate", "Real Estate", "Energy", "Real Estate",
                          "Mining", "Consumer", "Transportation", "Pharmaceuticals",
                          "Transportation", "Consumer", "Pharmaceuticals", 
                          "Transportation", "Real Estate", "Textile", "Materials",
                          "Materials", "Consumer", "Consumer", "Consumer", 
                          "Transportation", "Media", "Transportation", "Transportation",
                          "Materials", "Pharmaceuticals", "Textile")

final.cluster.2$Industry <- industries.cluster.2


### Companies of cluster 4

final.cluster.4 <- as.data.frame(final.kmeans.df[final.kmeans.df$k.pred.2 %in% c(4), ])

companies.cluster.4 <- c("CTCP Vàng bạc đá quý ASEAN", "CTCP Điện tử Biên Hòa",
                         "CTCP Đầu tư CFM", "CTCP Vinaceglass", "CTCP Chíp Sáng",
                         "CTCP Vật liệu Xây dựng Hà Nội", "COTIMEX QUANG NAM",
                         "CTCP Xây dựng Công trình ngầm", "CTCP Đầu tư Tài chính Giáo dục",
                         "CTCP Docimexco", "TỔNG CÔNG TY VẬT LIỆU XÂY DỰNG SỐ 1 - CTCP (FICO)",
                         "CTCP Đầu tư Xây dựng Hà Nội", "CTCP Hưng Đạo Container",
                         "CTCP Du lịch Hương Giang", 
                         "CTCP Xây dựng Thương mại và Khoáng Sản Hoàng Phúc",
                         "CTCP ILA", "CTCP Xuất nhập khẩu Bình Định",
                         "CTCP Tập đoàn Khải Hoàn Land",
                         "CTCP Đầu tư Thương mại và Xuất nhập khẩu CFS",
                         "CTCP Khoáng sản Luyện kim màu", "CTCP CNC Capital Việt Nam",
                         "KSTL", "CÔNG TY TNHH MỘT THÀNH VIÊN LÂM CÔNG NGHIỆP BẮC QUẢNG BÌNH",
                         "CTCP Lương thực Lương Yên", "CTCP khoáng Sản Mangan",
                         "CTCP mỏ và Xuất nhập khẩu Khoáng sản Miền Trung",
                         "CTCP Nam Việt", "CTCP Khách Sạn Bưu Điện Nha Trang",
                         "CTCP Nhà Việt Nam", "CTCP Dịch vụ Một Thế Giới",
                         "CTCP Hóa phẩm Dầu khí DMC - Miền Bắc", 
                         "CTCP Phát triển Bất động sản Phát Đạt", "CTCP PGT Holdings",
                         "CTCP Đầu tư và Phát triển Dự án Hạ tầng Thái Bình Dương",
                         "CTCP Máy - Thiết bị Dầu khí", 
                         "Công ty Cổ phần Dầu khí Dương Đông Kiên Giang",
                         "CTCP Đầu tư PVR Hà Nội", "CTCP Sông Đà 12",
                         "SABEMD", "Tổng Công ty Thương mại Sài Gòn TNHH MTV",
                         "CTCP Muối Việt Nam", "CTCP Simco Sông Đà", 
                         "CTCP Xây dựng Hạ tầng Sông Đà",
                         "CTCP Phòng cháy Chữa cháy và Đầu tư Xây dựng Sông Đà",
                         "CTCP Vinaconex Sài Gòn", "CTCP Sông Đà 19",
                         "CTCP Spiral Galaxy", "CTCP Giáo dục G Sài Gòn", 
                         "CTCP Vận chuyển Sài Gòn Tourist", "CTCP Tập đoàn Vexilla Việt Nam", 
                         "Công ty TNHH MTV Cao su 1-5 Tây Ninh",
                         "TBMG", "CTCP Phát triển Công trình Viễn thông",
                         "CTCP TIE", "CTCP Viễn thông Thăng Long", "TSJC",
                         "CTCP Xi măng Tiên Sơn Hà Tây", "CTCP Tập đoàn Kỹ nghệ gỗ Trường Thành",
                         "CTCP Tư vấn Xây dựng Vinaconex", "CTCP Xây dựng Điện VNECO 1",
                         "CTCP Đầu tư và Xây dựng VNECO 9", "CTCP Đầu tư Phát triển Việt Trung Nam",
                         "CTCP Dệt may Thắng Lợi", "VJTC", "CTCP Thương mại và Dịch vụ Dầu khí Vũng Tàu",
                         "VNLSHIP", "CTCP Bất động sản và Đầu tư VRC",
                         "CTCP Dịch vụ Vận tải Ô Tô Số 8", "CTCP Xà phòng Hà Nội")

industries.cluster.4 <- c("Trading", "Manufacturing", "Education", "Manufacturing",
                          "Manufacturing", "Trading", "Trading", "Construction",
                          "Consulting", "Trading", "Trading", "Real Estate",
                          "Leasing", "Hospitality", "Mining", "Warehousing",
                          "Trading", "Real Estate", "Trading", "Manufacturing",
                          "Mining", "Unknown", "Agriculture", "Trading",
                          "Mining", "Mining", "Manufacturing", "Hospitality",
                          "Real Estate", "Information Tech", "Trading", 
                          "Real Estate", "Consulting", "Construction", 
                          "Manufacturing", "Trading", "Construction", "Construction",
                          "Unknown", "Trading", "Agriculture", "Professional Services",
                          "Construction", "Construction", "Manufacturing",
                          "Construction", "Mining", "Apparel", "Transportation",
                          "Trading", "Manufacturing", "Unknown", "Construction",
                          "Trading", "Information Tech", "Unknown", "Manufacturing",
                          "Manufacturing", "Consulting", "Construction", "Construction",
                          "Manufacturing", "Apparel", "Unknown", "Utilities",
                          "Unknown", "Real Estate", "Transportation", "Manufacturing")

final.cluster.4$Industry <- industries.cluster.4


### Double-checking if there is any missing value

final.kmeans.df[is.na(final.kmeans.df$k.pred.2) == TRUE, ]

table(final.kmeans.df$k.pred.2)

### Try double-checking optimal number of clusters using Elbow method for K-means

fviz_nbclust(two.sum.limited.data[, c("EBITDA.DEBT", "FFO.DEBT")], 
             kmeans, method = "wss")

fviz_nbclust(lim.fin.1[, c("EBITDA.DEBT", "FFO.DEBT")], 
             kmeans, method = "wss")

fviz_nbclust(lim.fin.2[, c("EBITDA.DEBT", "FFO.DEBT")], 
             kmeans, method = "wss")

fviz_nbclust(lim.fin.3[, c("EBITDA.DEBT", "FFO.DEBT")], 
             kmeans, method = "wss")

### Attempting GMM using both non-scaled financial ratios

two_lim_fin_gmm <- GMM(as.matrix(two.sum.limited.data[, c("EBITDA.DEBT", "FFO.DEBT")]),
                            gaussian_comps = 8, dist_mode = "maha_dist",
                            seed_mode = "random_subset")

two.sum.limited.data$g.pred = as.factor(predict(two_lim_fin_gmm, 
                                                newdata = as.matrix(
                                                  two.sum.limited.data[, c("EBITDA.DEBT", "FFO.DEBT")])))

as.data.frame(table(two.sum.limited.data$g.pred))

ggplot(two.sum.limited.data, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = g.pred)) +
  geom_point(size = 3.5) +
  theme_bw()

lim.fin.g1 = two.sum.limited.data[two.sum.limited.data$g.pred == 3 | 
                                   two.sum.limited.data$g.pred == 6 |
                                   two.sum.limited.data$g.pred == 7, ]

two_lim_fin_gmm_1 <- GMM(as.matrix(lim.fin.g1[, c("EBITDA.DEBT", "FFO.DEBT")]),
                       gaussian_comps = 6, dist_mode = "maha_dist",
                       seed_mode = "random_subset")

lim.fin.g1$g.pred = as.factor(predict(two_lim_fin_gmm_1, 
                                                newdata = as.matrix(
                                                  lim.fin.g1[, c("EBITDA.DEBT", "FFO.DEBT")])))

as.data.frame(table(lim.fin.g1$g.pred))

ggplot(lim.fin.g1, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = g.pred)) +
  geom_point(size = 2) +
  theme_bw()

plot_ly(lim.fin.g1, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(g.pred),
        symbol = ~factor(g.pred),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in all Clusters")

### Add border lines seperating groups into plot

vline1 <- function(x = 100, color = "green") {
  list(
    type = "line",
    y0 = -500,
    y1 = 1500,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )
}

plot_ly(lim.fin.g1, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(g.pred),
        symbol = ~factor(g.pred),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in all Clusters") %>%
  layout(shapes = list(vline1(-0.3), vline1(0.5), vline1(1.5), vline1(4.0),
                       vline1(6.5)))


hline1 <- function(y = 100, color = "purple") {
  list(
    type = "line",
    y0 = y,
    y1 = y,
    xref = "paper",
    x0 = -500,
    x1 = 5000,
    line = list(color = color)
  )
}

plot_ly(lim.fin.g1, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(g.pred),
        symbol = ~factor(g.pred),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in all Clusters") %>%
  layout(shapes = list(hline(-0.2), hline(0.4), hline(1.3), hline(3.3),
                       hline(5.5)))

### Try double-checking optimal number of clusters using Elbow method for GMM

fviz_nbclust(two.sum.limited.data[, c("EBITDA.DEBT", "FFO.DEBT")], 
             cluster::pam, method = "wss")

fviz_nbclust(lim.fin.g1[, c("EBITDA.DEBT", "FFO.DEBT")], 
             cluster::pam, method = "wss")











### Attempting K-medoids using both non-scaled financial ratios

kmed <- pam(two.sum.limited.data[, c("EBITDA.DEBT", "FFO.DEBT")], k = 6)

kmed$clusinfo

two.sum.limited.data$km.pred <- as.factor(kmed$cluster)

ggplot(two.sum.limited.data, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = km.pred)) +
  geom_point(size = 4) +
  theme_bw()

lim.fin.k1 = two.sum.limited.data[two.sum.limited.data$km.pred == 1 |
                                    two.sum.limited.data$km.pred == 2, ]

kmed1 <- pam(lim.fin.k1[, c("EBITDA.DEBT", "FFO.DEBT")], k = 6)

kmed1$clusinfo

lim.fin.k1$km.pred.1 <- as.factor(kmed1$cluster)

ggplot(lim.fin.k1, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = km.pred.1)) +
  geom_point(size = 3) +
  theme_bw()


lim.fin.k2 = lim.fin.k1[lim.fin.k1$km.pred.1 == 1 | lim.fin.k1$km.pred.1 == 2 |
                          lim.fin.k1$km.pred.1 == 3, ]

kmed2 <- pam(lim.fin.k2[, c("EBITDA.DEBT", "FFO.DEBT")], k = 6)

kmed2$clusinfo

lim.fin.k2$km.pred.2 <- as.factor(kmed2$cluster)

ggplot(lim.fin.k2, aes(x = EBITDA.DEBT, y = FFO.DEBT, color = km.pred.2)) +
  geom_point(size = 3) +
  theme_bw()

### Attempting Multi-dimensional Scaling on the limited Dataset

lim.mds <- sum.limited.data[, c("scaled.ebitda.debt", "scaled.ffo.debt")] %>%
  dist() %>%          
  cmdscale(k = 1) %>%
  as_tibble()

colnames(lim.mds) <- c("Dim.1")

# Plot MDS

ggscatter(lim.mds, x = "Dim.1", y = 0, size = 2, repel = TRUE,
          col = "red")

### K-means clustering

clust <- kmeans(lim.mds$Dim.1, 8)$cluster %>%
  as.factor()

lim.mds <- lim.mds %>%
  mutate(k.pred = clust)

as.data.frame(table(lim.mds$k.pred))

ggplot(lim.mds, aes(x = Dim.1, y = 0, color = k.pred)) +
  geom_point(size = 5) +
  theme_bw()

lim.mds.1 = lim.mds[lim.mds$k.pred == 6, ]

clust.1 <- kmeans(lim.mds.1$Dim.1, 8)$cluster %>%
  as.factor()

lim.mds.1 <- lim.mds.1 %>%
  mutate(k.pred.1 = clust.1)

as.data.frame(table(lim.mds.1$k.pred.1))

ggplot(lim.mds.1, aes(x = Dim.1, y = 0, color = k.pred.1)) +
  geom_point(size = 5) +
  theme_bw()

summary(lim.mds.1[lim.mds.1$k.pred.1 == 1, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$k.pred.1 == 2, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$k.pred.1 == 3, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$k.pred.1 == 4, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$k.pred.1 == 5, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$k.pred.1 == 6, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$k.pred.1 == 7, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$k.pred.1 == 8, ]$Dim.1)

### Gaussian Mixture Model

test_fin_gmm = GMM(as.matrix(lim.mds.1$Dim.1), 6, dist_mode = "maha_dist", 
                  seed_mode = "random_subset") 

lim.mds.1$g.pred.1 = as.factor(predict(test_fin_gmm, newdata = as.matrix(lim.mds.1$Dim.1)))

as.data.frame(table(lim.mds.1$g.pred.1))

ggplot(lim.mds.1, aes(x = Dim.1, y = 0, color = g.pred.1)) +
  geom_point(size = 5) +
  theme_bw()

summary(lim.mds.1[lim.mds.1$g.pred.1 == 1, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$g.pred.1 == 2, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$g.pred.1 == 3, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$g.pred.1 == 4, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$g.pred.1 == 5, ]$Dim.1)

summary(lim.mds.1[lim.mds.1$g.pred.1 == 6, ]$Dim.1)

### Interactive plot of all companies in each cluster

plot_ly(final.kmeans.df, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(k.pred.2),
        symbol = ~factor(k.pred.2),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in all Clusters")

### Add border lines seperating groups into plot

vline <- function(x = 100, color = "green") {
  list(
    type = "line",
    y0 = -500,
    y1 = 1500,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )
}

plot_ly(final.kmeans.df, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(k.pred.2),
        symbol = ~factor(k.pred.2),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in all Clusters") %>%
  layout(shapes = list(vline(-0.3), vline(0.5), vline(1.5), vline(3.0),
                       vline(6.0)))


hline <- function(y = 100, color = "purple") {
  list(
    type = "line",
    y0 = y,
    y1 = y,
    xref = "paper",
    x0 = -500,
    x1 = 5000,
    line = list(color = color)
  )
}

plot_ly(final.kmeans.df, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(k.pred.2),
        symbol = ~factor(k.pred.2),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in all Clusters") %>%
  layout(shapes = list(hline(-0.2), hline(0.4), hline(1.3), hline(2.6),
                       hline(5.5)))


plot_ly(final.cluster.2, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 250,
        text = ~Company.Name) %>%
  layout(title = "Plot of Companies in Cluster 2")

### Experimental Approach, attempting to find 4 new clusters within these 
### clusters: 5, 6 and 1.

exp.df = final.kmeans.df[final.kmeans.df$k.pred.2 == 5 | 
                           final.kmeans.df$k.pred.2 == 6 |
                           final.kmeans.df$k.pred.2 == 1, ]

plot_ly(exp.df, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        text = ~ticker) %>%
  layout(title = "Plot of Companies in 3 central Clusters")

exp_kmeans <- kmeans(exp.df[, c("EBITDA.DEBT", "FFO.DEBT")], centers=4)

print(exp_kmeans)

exp.df$k.pred <- as.factor(exp_kmeans$cluster)

plot_ly(exp.df, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(k.pred),
        symbol = ~factor(k.pred),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in new Clusters")

## Left join final K-means Table with the new experimental K-means Table

final.exp.df <- merge(x = final.kmeans.df, 
                         y = exp.df[, c("ticker", "k.pred")], 
                         by = "ticker", all.x = TRUE)

summary(final.exp.df$k.pred.y)

### Add 2 new factor levels

levels(final.exp.df$k.pred.y) <- c(levels(final.exp.df$k.pred.y), 5)

levels(final.exp.df$k.pred.y) <- c(levels(final.exp.df$k.pred.y), 6)

final.exp.df[final.exp.df$k.pred.2 == 4, ]$k.pred.y <- as.factor(5)

final.exp.df[final.exp.df$k.pred.2 == 2 |
               final.exp.df$k.pred.2 == 3, ]$k.pred.y <- as.factor(6)

summary(final.exp.df$k.pred.y)

plot_ly(final.exp.df, x = ~EBITDA.DEBT, y = ~FFO.DEBT, 
        size = 150,
        color = ~factor(k.pred.y),
        symbol = ~factor(k.pred.y),
        text = ~ticker) %>%
  layout(title = "Plot of Companies in new Clusters")





