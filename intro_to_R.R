# Matt Castellano || macastel@usc.edu

# exercise 2.1
name = "Matt"
age = 19
birthday = "04/26/2002"

# exercise 2.2
list_of_numbers = c(1, 3, 4, 8, 1, 44, -3, 29, -12)
list_max_num = max(list_of_numbers)
list_min_num = min(list_of_numbers)
product = list_max_num*list_min_num
print(product)

# exercise 2.3
quadratic_formula = function(a, b, c){
  x = (-b + sqrt(b*b - (4*a*c)))/(2*a) 
  return(x)
}
quadratic_formula(1, 2, 3) # test 

# exercise 2.4
## [1] "Green egs and ham."
## [1] "I do not like them, Sam-I-Am."
## [1] "Would you like them here or there?"
## [1] "I would not like them anywhere." 

# exercise 2.5
x = 0 
while (x<5){
  x = x + runif(1)
  print(x)
}

# exercise 2.6
## [1] "USC"
## [1] "USC"
## [1] "USC"
## [1] "USC"
## [1] "USC"

# exercise 2.7
print(10:1)
# & 
x = 10
while (x!=0){
  print(x)
  x = x - 1 
}

# exercise 2.8
for(i in 1:6){
  for(j in 1:6){
    print (i+j)
  }
}

# exercise 2.9 
mean_mat = matrix(1:100, nrow = 10, ncol = 10)
row_means = rowSums(mean_mat)/nrow
col_means = colSums(mean_mat)/ncol

# exercise 3.1

library(tidyverse)
print("(1)")
str(mtcars)
print("(2)")
print(head(mtcars))
print("(3)")
glimpse(mtcars)

# exercise 3.2
mtcars_4 = mtcars$hp
mtcars_4 = mtcars[, 4]

# exercise 3.3
mtcars$hp[3]
# &
mtcars[3 ,4]

# exercise 3.4
head(mtcars)
mtcars$wt[5]

# exercise 3.5
mtcars[, 1:3]
# dim(mtcars) 
mtcars[1:2,8:11]
even_rows = (1:16)*2
mtcars[even_rows, ]

# exercise 3.6
mtcars_copy = mtcars 
mtcars_copy$mpg * 0.5
mtcars_copy$super_mpg = mtcars_copy$mpg * 100
mtcars_copy$super_mpg = mtcars$mpg
glimpse(mtcars_copy)

# exercise 3.7
cereal = read.csv("cereal.csv")
str(cereal)
cereal_names = cereal$name
favorite_cereals = c("Honey-comb", "Froot Loops", "Apple Jacks")

# exercise 4.1
colors_vector = c("cyan", "yellow", "magenta", "key")
colors_vector[2:3]
# or this one using stuff in 4.2 
colors_vector = c("cyan", "yellow", "magenta", "key")
mask = c(FALSE, TRUE, TRUE, FALSE)
colors_vector[mask]

# exercise 4.2
numeric_values = c(1, 2, NA, 3, NA, NA, NA, 4) #remove the NA ones
mask = !is.na(numeric_values)
numeric_values = numeric_values[mask]
numeric_values

# exercise 4.3
str(cereal)
cereal_names = cereal$name
favorite_cereals = c("Honey-comb", "Froot Loops", "Apple Jacks")
names_checker = cereal_names %in% favorite_cereals 
favorite_cereal_data = cereal[names_checker, ]
dim(favorite_cereal_data)

# exercise 4.4
numeric_values = c(1, 2, NA, 3, NA, NA, NA, 4)
numeric_values[is.na(numeric_values)] = -1 
numeric_values

# exercise 4.5
numeric_values = c(1, 2, NA, 3, NA, NA, NA, 4)
numeric_values[is.na(numeric_values)] = -1
mean_of_nums = sum(numeric_values) / 8
ifelse(numeric_values < mean_of_nums, "small", "large")

# exercise 4.6
cereal$type_full = ifelse(cereal$type == "C", "Cold", "Hot")
cereal

# exercise 5.1
cereal$is_favorite = ifelse(names_checker, "favorite", "not favorite")
plot(x = cereal$carbo, y = cereal$calories, 
     main = "Cereal", # set the title 
     xlab = "carbohydrates (g)", # set the x-axis label 
     ylab = "calories (kcals)") # set the y-axis label
plot(x = cereal$protein, y = cereal$sugars, 
     col = factor(cereal$is_favorite), # set color after change to factor
     main = "Cereal", # set the title 
     xlab = "protein (g)", # set the x-axis label 
     ylab = "sugars (g)") # set the y-axis label 
boxplot(cereal$fiber)
write.csv(cereal, "cereal_favorites.csv")