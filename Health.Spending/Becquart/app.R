library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)

HCdf <- read.csv("savingthehealthcarecrisis.csv")
HCdf$country <- as.character(HCdf$country)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Where do we see the best healthcare efficiency?"),
    p("The United States spends the most on healthcare per person, $9,237 per person, according to a Lancet paper from 2017 (1). Yet the United States does not have the highest life expectancy in the world, and is spending much more on healthcare than other countries with similar life expectancies (1). In this figure, we demonstrate the drastic difference in healthcare spending between all countries, but specifically between the countries with the highest mean life expectancies in for the years 1995 to 2010. Several European countries, as well as Japan, have high life expectancies as well as fairly high total healthcare spending, though nowhere close to what the United States is spending. Singapore, on the other hand, stands out for having high life expectancy and very low total healthcare spending compared to most countries, and thus stands out to us as an outlier we could learn from."),
    # Show a plot of the generated distribution
    mainPanel(
        fluidRow(
            column( width = 12,
                    plotOutput(outputId = "distPlot",
                               click = "splot_click")
                    )
        ) ,
        fluidRow(
            column(width = 12,
                   h4("Points near click"),
                   tableOutput("moneyTable"),
                   h4("References:"),
                   p("[1] Global Burden of Disease Health Financing Collaborator Network. Evolution and patterns of global health financing 1995-2014: Development assistance for health, and government, prepaid private, and out-of-pocket health spending in 184 countries. Lancet 2017; 389: 1981-2004.")
            )
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Scatterplot of countries and their spending efficiency
    output$distPlot <- renderPlot({
        #plot of healthcare spending versus dalys
        HCdf %>%
            ggplot() +
            geom_point(aes(total_healthspending, life_expectancy, group = country), colour = "grey") +
            geom_line(aes(total_healthspending, life_expectancy, group =country), colour = "grey") +  geom_point(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "United States"), colour = "dark green") +
            geom_point(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Iceland"), colour = "red") +
            geom_point(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Japan"), colour = "blue") +
            geom_point(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Singapore"), colour = "orange") +
            geom_point(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Switzerland"), colour = "purple") +
            geom_point(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Australia"), colour = "brown") +
            
            geom_line(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "United States"), colour = "dark green") +
            geom_line(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Iceland"), colour = "red") +
            geom_line(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Japan"), colour = "blue") +
            geom_line(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Singapore"), colour = "orange") +
            geom_line(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Switzerland"), colour = "purple") +
            geom_line(aes(total_healthspending, life_expectancy), data = filter(HCdf, country == "Australia"), colour = "brown") +
            theme_bw() + 
            xlab("Total Health Spending") +
            ylab("Life expectancy (years)")
        
    })
    
    #Point and click
    output$moneyTable <- renderTable({
        nearPoints(HCdf[c("country", "year", "total_healthspending", "life_expectancy")]
                   , input$splot_click)
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
