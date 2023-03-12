##How to get the website updated

##Shiny app to create blog post
## Add description
## slugify title
## fix path issues so can run from 

 
## slugify title
x <- "Test Text a1~!@#$%^&*(){}_+:\"<>?,./;'[]=" #or whatever
x_slug <-   stringr::str_replace_all(stringr::str_to_lower(x), "[^[:alnum:]]", "-") 

stringr::str_replace_all(stringr::str_to_lower("A sticky bun, but not every day"), "[^[:alnum:]]", "-") 
