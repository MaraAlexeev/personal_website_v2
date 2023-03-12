# Source https://www.r-bloggers.com/2022/08/creating-posts-for-quarto-blog/
#
# add description to shiny app
# slug without spaces

library(shiny)
library(miniUI)
library(stringr)
library(whisker)

Blog_post <- function(title, categories, description){
  data <- list(title = title,
               author = "'Mara Alexeev'",
               date = Sys.Date(),
               date_modified = "'`r Sys.Date()`'",
               categories = categories,
               slug = stringr::str_replace_all(stringr::str_to_lower(title), "[^[:alnum:]]", "-"), 
               draft = 'true',
               description = description,
               image = "''",
               archives = format(date, "%Y/%m"),
               toc = 'false',
               fold = "true",
               tools = 'true',
               link =  'false')
  
  Template <- '---
title: {{title}}
author: {{author}}
date: {{date}}
date-modified: {{date_modified}}
categories: [{{categories}}]
draft: {{draft}}
description: {{description}}
slug: {{slug}}
image: {{image}}
archives:
  - {{archives}}
toc: {{toc}}

format:
  html:
    code-fold: {{fold}}
    code-tools: {{tools}}
---

# Introduction

# Conclusion'
  
  
  dir.create(paste0("./posts/", data$date, "-", data$slug))
  writeLines(whisker::whisker.render(Template, data), paste0("./posts/",data$date, "-", data$slug, "/index.qmd"))
  file.edit(paste0("./posts/",data$date, "-", data$slug, "/index.qmd"))
  # dir.create(paste0("./posts/", data$date, "-", title))
  # writeLines(whisker::whisker.render(Template, data), paste0("./posts/",data$date, "-", title, "/index.qmd"))
  # file.edit(paste0("./posts/",data$date, "-", title, "/index.qmd"))
}


QGadget <- function() {....}

ui <- miniUI::miniPage(
  miniUI::gadgetTitleBar("Quarto Blog Post"),
  miniUI::miniContentPanel(
    shiny::textInput("title", "Title", placeholder = "Post Title"),
    shiny::textInput("description", "Description", placeholder = "Post Description"),
    shiny::selectInput("categories", "Categories", 
                       choices = list("How-to", "Book Review",
                                      "R", "Python", "Quarto", "Project",
                                      "Medicine", 
                                      "Family Life", "Cooking"),
                       multiple = TRUE)

  )
)

server <- function(input, output, session) {
  
  shiny::observeEvent(input$done, {
    Blog_post(input$title, input$categories, input$description)
    stopApp("Post Created")
  })
}

shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Quarto Blog Post"))

# Blog_post <- function(title, categories){
#   data <- list(title = title,
#                author = "'Mara Alexeev'",
#                date = Sys.Date(),
#                date_modified = "'`r Sys.Date()`'",
#                categories = categories,
#                  
#                draft = 'true',
#                description = "''",
#                image = "''",
#                archives = format(date, "%Y/%m"),
#                toc = 'false',
#                fold = "true",
#                tools = 'true',
#                link =  'false')
#   
#   Template <- '---
# title: {{title}}
# author: {{author}}
# date: {{date}}
# date-modified: {{date_modified}}
# categories: [{{categories}}]
# draft: {{draft}}
# description: {{description}}
# image: {{image}}
# archives:
#   - {{archives}}
# toc: {{toc}}
# 
# format:
#   html:
#     code-fold: {{fold}}
#     code-tools: {{tools}}
# ---
# 
# # Introduction
# 
# # Conclusion'
# 
# 
#   dir.create(paste0("./personal_website_v2.2023-03-05/posts/", data$date, "-", title))
#   writeLines(whisker::whisker.render(Template, data), paste0("./personal_website_v2.2023-03-05/posts/",data$date, "-", title, "/index.qmd"))
#   file.edit(paste0("./personal_website_v2.2023-03-05/posts/",data$date, "-", title, "/index.qmd"))
#   # dir.create(paste0("./posts/", data$date, "-", title))
#   # writeLines(whisker::whisker.render(Template, data), paste0("./posts/",data$date, "-", title, "/index.qmd"))
#   # file.edit(paste0("./posts/",data$date, "-", title, "/index.qmd"))
# }

