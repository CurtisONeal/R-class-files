#Sharpspring Data Wrangling Excercise 2 Mar 30, 2016 Curtis O'Neal
#this R script manipulates data in the Titanic dataset and works with missing data
# Assumptions:

#load dependencies
library(plyr) #note must be loaded before dplyr
library(dplyr) #note must be loaded after plyr
library(stringi) #used stri_sub
library(assertthat)#used for noNA(x) Does Object have any missing values
#library(stringr) #did not use
#library(tidyr) #did not use it looks like mutate could have worked for swapping values in vectors

#Start Task zero - 0: Load the data into Rstudio Save the data set as a CSV file called titanic_original.csv and load it in RStudio into a data frame.

new.file <- read.csv("titanic_original.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings=c(""," ","NA"))
View(new.file)
#NOTE at least some of my NA's are " ", or "" chars or NA
#End Task zero

#Begin Task 1: Port of embarkation. The embarked column has one missing value, Find the missing value and replace it with S.
new.file$embarked [ new.file$embarked %in% c("", " ", NA) ] <- 'S' #convert 1 known missing value to "S" for Southhampton, value is " " not NA
#View(new.file[169,]) This is the value to check #End Task 1: Port of embarkation.

#Begin Task 2 Age: Many values in the Age column are missing. While there are many ways to fill these missing values, use the mean or median of the rest of the values is quite common in such cases.
#Calculate the mean of the Age column and use that value to populate the missing values
#View(new.file$age)
meanAge <- round(mean(new.file$age, na.rm = TRUE), digits = 4) #other ages are to 4 decimal places
new.file$age [ new.file$age %in% c("", " ", NA) ] <- meanAge #convert 1 known missing value to "S" for Southhampton, value is " " not NA
NA_age <- noNA(new.file$age) #check that there are no NA's Confirmed
#new.file$age #show me the vector of 1310 items
#countMeanAge <- 0
entry <- 0
for (entry in new.file$age) countMeanAge <- countMeanAge+1

#3: Lifeboat: many passengers did not make it to a boat.Fill these empty slots with a dummy value e.g.NA
#3: Lifeboat: many passengers did not make it to a boat.Fill these empty slots with a dummy value e.g.NA
View(new.file$boat)
new.file$boat [ new.file$boat %in% c("", " ", NA) ] <- "NA"                                                                                                                                                     
#This works, but gives "Error in View : 'names' attribute [16] must be the same length as the vector [1]"
#Separately when writing out the file with write.csv "NA" converts it to NA
#3: Lifeboat: End

#4: Start Cabin: Many passengers don’t have a cabin number associated with them. This might be a useful indicator of survival. Create a new column has_cabin_numberwhich has 1 if there is a cabin number, and 0 otherwise.
names
for (cabin in new.file$cabin){
  new.file$has_cabin_numberwhich <- ifelse( is.na(new.file$cabin), 0, 1)
}
#4: End Cabin:

#5 There is no five, but #6 is to export it.

#6: Submit the project on Github
write.csv(new.file, file = "titanic_clean.csv")
checkfile <- read.csv("titanic_clean.csv", row.names = 1)
View(checkfile)
