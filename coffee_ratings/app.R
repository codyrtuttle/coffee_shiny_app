################################################
####
#### Coffee Ratings
#### Shiny App
#### Cody R Tuttle
#### Started: 3/31/2021
#### Last Updated: 3/31/2021
###############################################

library(tidyverse)
library(shinythemes)
library(glue)
library(readr)
library(ggiraph)

## load data
coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

## clean data
coffee_ratings_unique <- coffee_ratings %>% 
    mutate(unique_coffee_id = 
               paste(country_of_origin, farm_name, variety, 
                     processing_method, harvest_year, total_cup_points, 
                     sep = " - ")) %>% 
    mutate(unique_num_id = row_number())

############# ------ Build UI ------ #############

ui <- fluidPage(
    theme = shinytheme("united"), 
    titlePanel(strong("World Coffee Ratings")), 
    fluidRow(
        column(6, offset = 4,
              selectizeInput(
                   "coffees", "Select Coffee:", 
                   choices = unique(coffee_ratings_unique$unique_coffee_id), 
                   helpText("Select coffee to view ratings across cup categories.")
               )
        )

    ),
    girafeOutput("coffee_coords")

)

############# ------ Build Server ------ #############

server <- function(input, output) {

    ## load data
    coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
    
    ## clean data
    coffee_ratings_unique <- coffee_ratings %>% 
        mutate(unique_coffee_id = 
                       paste(country_of_origin, farm_name, variety, 
                             processing_method, harvest_year, total_cup_points, 
                             sep = " - ")) %>% 
        mutate(unique_num_id = row_number())
        
    ## build coffee polar coordinates
    output$coffee_coords <- renderGirafe({
        coffee_coord <- coffee_ratings_unique %>% 
            filter(unique_coffee_id == input$coffees) %>% 
            rename(
                "Aroma" = aroma,
                "Flavor" = flavor, 
                "Aftertaste" = aftertaste,
                "Acidity" = acidity,
                "Body" = body,
                "Balance" = balance, 
                "Uniformity" = uniformity,
                "Clean Cup" = clean_cup,
                "Sweetness" = sweetness,
                "Cupper Points" = cupper_points
            ) %>% 
            pivot_longer(cols = "Aroma":"Cupper Points", names_to = "rating_cat", values_to = "rating") %>% 
            ggplot(aes(x = rating_cat, y = rating/2)) +
            geom_bar_interactive(aes(tooltip = glue("{rating_cat}: {round(rating/2, 2)}"), 
                                 data_id = rating_cat), 
                                 stat = "identity", 
                                 fill = "#2e1715") +
                coord_polar() +
                labs(x = " ", y = " ") +
                theme_classic() +
                theme(
                    line = element_blank(),
                    axis.text.y = element_blank()
              )
        girafe(ggobj = coffee_coord, 
               options = list(
                   opts_hover(css = "fill:red")
               )) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
