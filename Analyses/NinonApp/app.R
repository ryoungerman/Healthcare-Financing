library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(scales)

HCdf <- read.csv("savingthehealthcarecrisis.csv")
HCdf$country <- as.character(HCdf$country)

# Define UI for application that draws a histogram
ui <- fluidPage(
        tabsetPanel(    
        # Show a plot of the generated distribution
            tabPanel("Healthcare Efficiency",
                 fluidRow(
                     column( width = 10,
                             h1("Where do we see the best healthcare efficiency?"),
                             p("The United States spends the most on healthcare per person, $9,237 per person, according to a Lancet paper from 2017 (1). Yet the United States does not have the highest life expectancy in the world, and is spending much more on healthcare than other countries with similar life expectancies (1). In this figure, we demonstrate the drastic difference in healthcare spending between all countries, but specifically between the countries with the highest mean life expectancies in for the years 1995 to 2010. Several European countries, as well as Japan, have high life expectancies as well as fairly high total healthcare spending, though nowhere close to what the United States is spending. Singapore, on the other hand, stands out for having high life expectancy and very low total healthcare spending compared to most countries, and thus stands out to us as an outlier we could learn from."),
                    plotOutput(outputId = "distPlot",
                              click = "splot_click"))), 
            
                
        fluidRow(
        column(width = 10,
               h4("Points near click"),
               tableOutput("moneyTable"),
               h4("References:"),
               p("[1] Global Burden of Disease Health Financing Collaborator Network. Evolution and patterns of global health financing 1995-2014: Development assistance for health, and government, prepaid private, and out-of-pocket health spending in 184 countries. Lancet 2017; 389: 1981-2004.")
            ))
        ),
    tabPanel("US vs Singapore",
         fluidRow(
             h1("How does the United States compare to our outlier, Singapore?"),
             column(width = 10,
                    plotOutput(outputId = "gridPlot"),
                    p("Here we are comparing the United States to Singapore, the outlier we saw in our previous figure. Singapore has higher life expectancy, lower Disability-Adjusted Life Years (DALYs), but also lower total healthcare spending and government share of healthspending in recent years. It is interesting to note that government share of healthspending has decreased in Singapore despite increasing life expectancy and decreasing DALYs. Meanwhile, total healthcare spending and government share of healthspending has risen in the United States, without an accompanying change in life expectancy or DALYs.")
             )
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
    
    
    #Grid plot of 4 different graphs
    output$gridPlot <- renderPlot({
        p1 <- HCdf %>%
            filter(country == "Singapore" | country == "United States") %>%
            ggplot() + 
            geom_line(aes(year, life_expectancy, colour = country)) +
            ylab("life expectancy (years)") +
            theme(legend.position = "none", axis.title = element_text(size = 6)) 
        
        
        p2 <- HCdf %>%
            filter(country == "Singapore" | country == "United States") %>%
            ggplot() + 
            geom_line(aes(year, dalys, colour = country)) +
            ylab("Disability-Adjusted Life Years (DALYs)") +
            theme(legend.position = "none", axis.title = element_text(size = 6)) 
        
        p3 <- HCdf %>%
            filter(country == "Singapore" | country == "United States") %>%
            ggplot() + 
            geom_line(aes(year, total_healthspending, colour = country))+
            ylab("total health care spending") +
            theme(legend.position = "none", axis.title = element_text(size = 6)) 
        
        p4 <- HCdf %>%
            filter(country == "Singapore" | country == "United States") %>%
            ggplot() + 
            geom_line(aes(year, gov_share_healthspending, colour = country)) +
            ylab("Government share of healthcare spending") +
            theme(legend.position = "none", axis.title = element_text(size = 6)) 
        
        #legend stuff
        ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom") 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)