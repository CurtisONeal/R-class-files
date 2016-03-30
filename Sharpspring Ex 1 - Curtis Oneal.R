#Sharpspring Data Wrangling Excercise 1 Mar 30, 2016 Curtis O'Neal
#this file manipulates data in a toy dataset and assumes that mistaken values are 
# and will be manually updated in the vector in mapvalues

#load dependencies
library(plyr) #note must be loaded before dplyr
library(dplyr) #note must be loaded after plyr
library(stringi) #used stri_sub
#library(stringr) #did not use
#library(tidyr) #did not use it looks like mutate could have worked for swapping values in vectors

#Start Task zero - load the .xls after manually converting to csv
new.file <- read.csv("refine_original.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
colnames(new.file)[2] <- "product.code" #rename product code Variable
#End Task zero

# Start Task 1: Clean up comapny names 
#NOTE all of this could be done with %>% in  fewer statements and skipping the View()'s
#Warning: there are extra steps that weren't needed but were good to learn, van houton lost its space, company var is charvar not factors
#Warning: Dirty data is cleaned with manual vectors that will need updated

str(new.file$company) #verify that $company came in as str for string manipulation
#View(new.file) #examine file
name.vector <- new.file$company #collect company string vectors into name.vector
#View(name.vector) #examine vector
name.vector1 <- tolower(name.vector) #remove all uppercase
#View(name.vector1) #examine vector
name.vector2 <- trimws(name.vector1) #remove trailing and leading whitespace
#View(name.vector2) #examine vector
name.vector2 <- iconv(name.vector2,to="ASCII//TRANSLIT") #just in case non latin chars with accents
#View(name.vector2) #examine vector
name.vector3 <-sub("0","o",name.vector2) #convert 0 to "o"
#View(name.vector3) #examine vector
name.vector4 <-sub(" ","",name.vector3) #remove inter-word whitespace (collapse)
#View(name.vector4) #examine vector
name.vector4 <- gsub("[^[:alpha:] ]","",name.vector4) #remove any other non-alphas and numericals
new.file$company <-name.vector4 #put character-cleaned-up vector into original variable

#Below are the mapvalues and vectors that would need manually updated by running unique(new.file@company) to find new dirty data
new.file$company <- mapvalues(new.file$company, from = c("philips", "phillips", "phllips", "phillps", "fillips", "phlips"), to = c("phillips", "phillips", "phillips", "phillips", "phillips", "phillips"))
new.file$company <- mapvalues(new.file$company, from = c("unilver"), to = c("unilever"))
#I'm aware that this is not as elegant as a regex solution will come back to it.
#View(new.file$company)

new.file <- group_by(new.file, company) #arrange by group
# End Task 1: Clean up brand names

# 3: Add product categories	p = smartphone, v = tv, x = laptop, q = tablet
new.file$product.cat <- stri_sub(new.file$product.code, from = 1L, to = 1L) #read the first char into new var
#creates a one character code new.file$product.cat based on product.code then map based on this char below
#WARNING below are the mapvalues and vectors that would need manually updated by running unique(new.file@product.code) to find new dirty data
new.file$product.cat.name <- mapvalues(new.file$product.cat, from = c("p", "v", "x", "q"), to = c("smartphone", "tv", "laptop", "tablet"))

#4: Create a new column "full_address" that concatenates the three address fields (address, city, country), separated by commas
new.file$full.address <- paste(new.file$address, new.file$city, new.file$country, sep = ", ")

#5: Create dummy variables for company categories, Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
#For every unique value in the string column, create a new 1/0 column
# via http://randyzwitch.com/creating-dummy-variables-data-frame-r/
#warning I removed the _ in between van and houten if this is a dependency on a later process
for (level in unique(new.file$company)){
  new.file[paste("company", level, sep = "_")] <- ifelse(new.file$company == level, 1, 0)
} #this code is right

#Clean up while debugging
rm(name.vector4)
rm(name.vector3)
rm(name.vector2)
rm(name.vector1)
rm(name.vector)

#5B Add four binary (1 or 0) columns for product category:product_smartphone, product_tv, product_laptop and product_tablet
#For every unique value in the string column, create a new 1/0 column
# via http://randyzwitch.com/creating-dummy-variables-data-frame-r/

for (level in unique(new.file$product.cat.name)){
  new.file[paste("product", level, sep = "_")] <- ifelse(new.file$product.cat.name == level, 1, 0)
} #this code is right - I didn'tstandardize the names enough yet.

write.csv(new.file, file = "refine_new.csv")
checkfile <- read.csv("refine_new.csv", row.names = 1)
View(checkfile)
