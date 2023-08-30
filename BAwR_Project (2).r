stars.df <- read.csv("star_classification.csv")
head(stars.df)

## To observe the table closely
colnames(stars.df)
summary(stars.df$class)

## to find the datatype of all the columns
str(stars.df)

## To check how many types of classes are
unique(stars.df$class)
table(stars.df$class)

##barplot for different classes
barplot(table(stars.df$class),col = c("dark red", "orange", "light blue"),
        ylim = c(0, 65000))
  
### Check for collinearity

library(CatEncoders)

#define original categorical labels
labs = LabelEncoder.fit(stars.df$class)

#convert labels to numeric values
stars.df$class = transform(labs, stars.df$class)
head(stars.df)

correlation<-cor(stars.df, method=c('pearson'))

correlation[is.na(correlation)] <- 0


library("reshape")
correlation_melt <- melt(correlation)



library("reshape2")
library(ggplot2)
ggheatmap <- ggplot(correlation_melt, aes(x=X1, y=X2, fill = value))+
  geom_tile(color = "white",aes(X1, X2,fill=value))+
  scale_fill_gradient2(low = "red", high = "darkgreen", mid = "burlywood", 
                    name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+geom_text(aes(X1, X2, label = round(value,2)), color = "black", size = 4)
# Print the heatmap
print(ggheatmap)

#after midway
unique(stars.df$rerun_ID)

stars.df<-stars.df[,-10]
head(stars.df)



#remove all the id's it is irrelevant as per domain knowledge
colnames(stars.df)
stars.df<-stars.df[,-c(1,9:12,16:17)]
stars.df


library('car')
model_all <- lm(class ~ ., data=stars.df)  # with all the independent variables in the dataframe

summary(model_all)
vif(model_all)


##Analysis
##alpha and delta
cols<-c('lightpink','darkorange3','darkorchid4')
plot(x=stars.df$alpha,y=stars.df$delta, col=cols[stars.df$class])

legend("topleft", legend = c("Galaxy", "QSO","Star"),
       fill = c("pink","darkorange3","darkorchid4"))
   
##Redshift
df<-data.frame(stars.df$class,stars.df$redshift)
ggp1 <- ggplot(df, aes(x = stars.df.redshift)) +    # Draw each column as histogram
  geom_histogram() + 
  facet_wrap(~ stars.df.class, scales = "free")
ggp1

##ugirz

boxplot(stars.df$g~stars.df$class)

##Classification


library(rpart)
library(rpart.plot)
library(caret)

stars.df

# partition
set.seed(1)  
# 60% going for training dataset
train.index <- sample(c(1:dim(stars.df)[1]), dim(stars.df)[1]*0.6)  
train <- stars.df[train.index, ]
valid <- stars.df[-train.index, ]


# desicion tree

set.seed(1)

cv<- rpart(class ~ ., data = train, parms = list(split = 'information'), method = "class")  # minsplit is the minimum number of observations in a node for a split to be attempted. xval is number K of folds in a K-fold cross-validation.
prp(cv, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(cv$frame$var[cv$frame$var == "<leaf>"])

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
pred.train <- predict(cv,train,type = "class")
# generate confusion matrix for training data
confusionMatrix(pred.train, as.factor(train$class))

### repeat the code for the validation set, and the deeper tree

pred.valid <- predict(cv,valid,type = "class")
confusionMatrix(pred.valid, as.factor(valid$class), mode='everything')



library(randomForest)
## random forest
rf<- randomForest(as.factor(class) ~ ., data = train, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)


## confusion matrix
rf.pred <- predict(rf, valid)
confusionMatrix(rf.pred, as.factor(valid$class), mode='everything')


##BOOSTING Tree
library(adabag)

train$class <- as.factor(train$class)

set.seed(1)
boost <- boosting(class ~ ., data = train)
pred <- predict(boost, valid)
confusionMatrix(as.factor(pred$class), as.factor(valid$class),mode='everything')


##Random forest is the best model for our dataset

