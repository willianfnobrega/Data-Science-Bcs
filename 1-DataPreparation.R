library(corrplot)

setwd('/Users/willianfranco/Library/CloudStorage/OneDrive-GISMAUniversityofAppliedSciencesGmbH/Applied Statistics')

df <- read.csv('/Users/willianfranco/Library/CloudStorage/OneDrive-GISMAUniversityofAppliedSciencesGmbH/Applied Statistics/dataset/sao-paulo-properties-april-2019.csv')

df <- df[!duplicated(df), ]

colSums(is.na(df))
head(df)
summary(df)


df <- subset(df, select = -c(Property.Type))


df_rent <- subset(df, Negotiation.Type == "rent")
df_sale <- subset(df, Negotiation.Type == "sale")


cor_rent <- cor(df_rent[sapply(df_rent, is.numeric)], use = "complete.obs")
cor_sale <- cor(df_sale[sapply(df_sale, is.numeric)], use = "complete.obs")

corrplot(cor_rent, method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix - Rent", mar=c(0,0,2,0))

corrplot(cor_sale, method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix - Sale", mar=c(0,0,2,0))


par(mfrow = c(1, 2))

boxplot(df_rent$Price, main = "Rent - Price", col = "lightblue", outline = TRUE)
boxplot(df_sale$Price, main = "Sale - Price", col = "lightgreen", outline = TRUE)


Q1_rent <- quantile(df_rent$Price, 0.25, na.rm = TRUE)
Q3_rent <- quantile(df_rent$Price, 0.75, na.rm = TRUE)
IQR_rent <- Q3_rent - Q1_rent
df_rent <- subset(df_rent, Price >= (Q1_rent - 1.5*IQR_rent) & Price <= (Q3_rent + 1.5*IQR_rent))

Q1_sale <- quantile(df_sale$Price, 0.25, na.rm = TRUE)
Q3_sale <- quantile(df_sale$Price, 0.75, na.rm = TRUE)
IQR_sale <- Q3_sale - Q1_sale
df_sale <- subset(df_sale, Price >= (Q1_sale - 1.5*IQR_sale) & Price <= (Q3_sale + 1.5*IQR_sale))

