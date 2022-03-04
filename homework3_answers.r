# Matt Castellano || macastel@usc.edu

# exercise 1.1
attenu [is.na(attenu$station),]	
attenu_cleaned = attenu[!is.na(attenu$station),]
attenu_cleaned
head(attenu_cleaned)
dim(attenu_cleaned)
# exercise 1.2
Theoph_2 = Theoph
medTheoph = median(Theoph_2$Dose)
Theoph_2$Dose_Clas = ifelse(Theoph_2$Dose >= medTheoph, "high", "low")
head(Theoph_2)
dim(Theoph_2)
# exercise 1.3
starbucks = read.csv('starbucks.csv')
starbs_na = is.na(starbucks)
is_row_empty = rowSums(starbs_na)
num_rows(starbucks) == length(is_row_empty)
starbucks_cleaned = starbucks[!(is_row_empty == 6),]
plot(starbucks_cleaned$Carb, starbucks_cleaned$Calories, xlab = "Carbs (grams)", ylab="Calories")
starbucks_cleaned[starbucks_cleaned$Calories == max(starbucks_cleaned$Calories),]
starbucks_cleaned$is_highest_fat = ifelse(starbucks_cleaned$Calories == max(starbucks_cleaned$Calories),TRUE,FALSE)
plot(starbucks_cleaned$Carb, starbucks_cleaned$Calories, xlab = "Carbs (grams)", ylab = "Calories", col=factor(starbucks_cleaned$is_highest_fat))
# exercise 1.4 
batting = read.csv('Batting.csv')
sum(batting$HR >= 3)
plot(batting$yearID, batting$HR)
la_angels = batting [batting$teamID == "LAA",]
plot(la_angels$yearID, la_angels$HR)
atl_or_pit = batting[batting$teamID == "PIT" | batting$teamID == "ATL",]
plot(atl_or_pit$yearID, atl_or_pit$HR, col=factor(atl_or_pit$teamID))
# exercise 1.5
easy_plot = function(x,y,color_data){
  medianColor = median(color_data)
  levels = factor(levels)
  print(medianColor)
  plot(x,y,col=levels, pch=20)
  print(cor.test(x,y))
}
easy_plot(starbucks_cleaned$Protein, starbucks_cleaned$Fat, starbucks_cleaned$Carbs)
# exercise 2.1
head(iris)
# exercise 2.2
colnames(iris)
# exercise 2.3
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)
# exercise 2.4 
mean_sepal = mean(iris$Sepal.Width)
iris_copy = iris_copy
sepal_vec = ifelse(iris$Sepal.Width < mean_sepal, "low", "high")
iris_copy$width_bool = sepal_vec
boxplot(iris_copy$Sepal.Width ~ iris_copy$width_bool)
# exercise 2.5
pairs(iris[1:4],pch=21, b=c("red", "green", "blue")[unclass(iris$Species)])
# exercise 3.1 
install.packages("BiocManager")
library(BiocManager)
BiocManager::install("TCGAbiolinks")
install.packages(TCGAbiolinks)
library(TCGAbiolinks)