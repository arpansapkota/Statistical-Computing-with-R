#Creating a List
my_list <- list("Statistics","R", 100, c(1,2,3), TRUE)

#Naming List Elements
names(my_list) <- c("string_element1","string_element2", "numeric_element", "vector_element", "logical_element")

#Accessing List Emelents
#Using assigned names:
my_list$string_element1
my_list$string_element2
my_list$numeric_element
my_list$vector_element
my_list$logical_element

#Using List Index:
my_list[1]
my_list[2]
my_list[3]
my_list[4]
my_list[5]

#Manipulate List elements 

#Adding a new element using list index : 
my_list[6] <- "new_element"

#Adding a new element using c() :
my_list <- c(my_list, "new_element1")

#Adding a new element using list() : 
my_list <- list(my_list, "new_element2")

# Remove the last element. 
my_list[6] <- NULL

# Removing the first element
my_list <- my_list[-1]

#updating the list
# Modifying the first element using index
my_list[[1]] <- "Data Science"

# Modifying the second element using name
my_list[["string_element2"]] <- "Python"

#convert List to Vector 
my_vector <- unlist(my_list)
class(my_vector)

