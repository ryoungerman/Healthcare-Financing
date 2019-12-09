rm(list=ls())
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(forcats)
library(dslabs)
library(ggplot2)
library(ggthemes)
library(maps)
library(rsconnect)
health <- read.csv("savingthehealthcarecrisis.csv")
pop_total <- read.csv("population_total.csv")
names(pop_total) <- substring(names(pop_total), 2)
colnames(pop_total)[colnames(pop_total)=="ountry"] <- "country"



#gather the data -- formatting for the join
tidy_pop <- pop_total %>%
    gather(year, population, '1800':'2100') 


#restrict population data to the appropriate timeframe
tidy_pop <- tidy_pop[ which(tidy_pop$year >= 1995
                            & tidy_pop$year <= 2010), ]

#change datatype so it matches for the join
tidy_pop$year <- as.numeric(tidy_pop$year)
head(tidy_pop)

#join the datasets
health_overall <- left_join(health, tidy_pop, by= c("country", "year"))

world_map <- map_data("world")

world_map <-
    world_map %>%
    rename(country = region)

health_overall <-
    health_overall %>%
    rename(Child.Mortality = child_mortality, DALYs = dalys, GINI.Coefficient = GINI_coef, 
           GDP.Per.Capita = GDP_percap, Total.Health.Spending = total_healthspending, Life.Expectancy = life_expectancy)

health_overall$country[which(health_overall$country=="United States")] <- "USA"
health_overall$country[which(health_overall$country=="Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"

world_map$country[which(world_map$country=="Aruba")] <- "Antigua and Barbuda"
world_map$country[which(world_map$country=="Barbuda")] <- "Antigua and Barbuda"
world_map$country[which(world_map$country=="South Sudan")] <- "Sudan"
health_overall$country[which(health_overall$country=="Congo, Rep.")] <- "Republic of Congo"
health_overall$country[which(health_overall$country=="Cote d'Ivoire")] <- "Ivory Coast"
health_overall$country[which(health_overall$country=="Kyrgyz Republic")] <- "Kyrgyzstan"
health_overall$country[which(health_overall$country=="Lao")] <- "Laos"
health_overall$country[which(health_overall$country=="Micronesia, Fed. Sts.")] <- "Micronesia"
health_overall$country[which(health_overall$country=="United Kingdom")] <- "UK"


health_map <- left_join(health_overall, world_map, by = "country")



# Define ui
ui <- fluidPage(
    theme = shinythemes::shinytheme("cosmo"),
    fluidRow(
        h1("Health and Financing Around the World", align = "center")
    ),
    fluidRow(
        column(width = 3, 
               sliderInput(inputId = "year",
                           label = "Choose a year",
                           value = 2000, min = 1995, max = 2010, sep = "", ticks = FALSE),
               radioButtons(inputId ="measure", 
                            label = "Change Color Scale By...",
                            c("Total Population" = "population",
                              "Child Mortality" = "Child.Mortality",
                              "Life Expectancy" = "Life.Expectancy",
                              "DALYs" = "DALYs",
                              "GINI Coefficient" = "GINI.Coefficient",
                              "GDP Per Capita" = "GDP.Per.Capita",
                              "Percent of Spending to Healthcare" = "govspending_percentoftotal",
                              "Total Health Spending" = "Total.Health.Spending"),
                            selected = "population"),
               htmlOutput("unit_description")
        ),
        column(width = 9, 
               plotOutput("mapPlot")
        )
    ),
    fluidRow(
        h1("Compare Two Countries", align = "center")
    ),
     
        fluidRow(
            column(width = 3,
                   selectInput(inputId = "countries", label = 
                                   "Select a country", 
                               choices = as.list(unique(health_map$country)),
                               selected = "USA"),
                   selectInput(inputId = "countries2", label = 
                                   "Select a country to compare", 
                               choices = as.list(unique(health_map$country)),
                               selected = "China"),
                   selectInput(inputId = "measure2", label = 
                                   "Select a measure for comparison", 
                               choices = c("Total Population" = "population",
                                           "Child Mortality" = "Child.Mortality",
                                           "Life Expectancy" = "Life.Expectancy",
                                           "DALYs" = "DALYs",
                                           "GINI Coefficient" = "GINI.Coefficient",
                                           "GDP Per Capita" = "GDP.Per.Capita",
                                           "Percent of Spending to Healthcare" = "govspending_percentoftotal",
                                           "Total Health Spending" = "Total.Health.Spending"))
                   
                
            ),
            column(width = 9, 
            plotOutput("linePlot")
            )
        )
)


# Define server
server <- function(input, output){
    
    pretty_names <- function(name){
        if (name == "Child.Mortality") {
            "Child Mortality"
        }
        else if (name == "Life.Expectancy") {
            "Life Expectancy"
        }
        else if (name == "GINI.Coefficient"){
            "GINI Coefficient"
        }
        else if (name == "GDP.Per.Capita"){
            "GDP Per Capita"
        }
        else if(name == "Total.Health.Spending"){
            "Total Health Spending"
        }
        else if(name == "govspending_percentoftotal"){
            "Percent of Spending to Healthcare"
        }
        else if(name == "population"){
            "Total Population"
        }
        else{
            "DALYs"
        }
        
    }
    
    output$mapPlot <- renderPlot({
        ggplot(dplyr::filter(health_map, year == input$year), aes(long, lat, group = group)) +
            geom_polygon(aes_string(fill = input$measure), color = "black") +
            scale_fill_gradient(low = "white", high = "red") + 
            #Labels
            xlab("") +
            ylab("") +
            ggtitle(paste(pretty_names(input$measure),"in", input$year)) + 
            #Theme
            theme_economist() +
            theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
            labs(fill = "") +
            guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5)) +
            theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank())
        
        
        
    })
    
    output$unit_description <- renderUI({ 
        if (input$measure == "Child.Mortality") {
            HTML(paste("<b>Measure Description</b><br>",
                       "Child mortality is measured by deaths of children 
                       under five years of age per 1,000 live births.<br>",
                       a("More Information Here", href="https://ourworldindata.org/child-mortality")))
            
        }
        else if (input$measure == "Life.Expectancy") {
            HTML(paste("<b>Measure Description</b><br>",
                       "Life expectancy is measured by the average number of years 
                       a newborn child would live if current mortality patterns 
                       were to stay the same.<br>",
                       a("More Information Here", href="https://ourworldindata.org/life-expectancy")))
            
        }
        else if (input$measure == "DALYs") {
            HTML(paste("<b>Measure Description</b><br>",
                       "DALYs - Disability Life Adjected Years - is a measure of 
                       overall disease burden, expressed as the number of years 
                       lost due to ill-health, disability or early death.<br>",
                       a("More Information Here", href="https://www.who.int/healthinfo/global_burden_disease/metrics_daly/en/")))
        }
        else if (input$measure == "GINI.Coefficient") {
            HTML(paste("<b>Measure Description</b><br>",
                       "The Gini Coefficient is a measure of statistical dispersion 
                       intended to represent the income or wealth distribution of a 
                       nation's residents, and is the most commonly used measurement of inequality.
                       A Gini coefficient of zero expresses perfect equality, where all 
                       values are the same (for example, where everyone has the same income). 
                       A Gini coefficient of one (or 100%) expresses maximal inequality among values 
                       (e.g., for a large number of people, where only one person has all the income 
                       or consumption, and all others have none, the Gini coefficient will be very nearly one).<br>",
                       a("More Information Here", href="https://www.investopedia.com/terms/g/gini-index.asp")))
        }
        else if (input$measure == "GDP.Per.Capita") {
            HTML(paste("<b>Measure Description</b><br>",
                       "GDP per capita is a measure of a country's economic output that accounts 
                       for its number of people. It divides the country's gross domestic product by its total population.<br>",
                       a("More Information Here", href="https://www.thebalance.com/gdp-per-capita-formula-u-s-compared-to-highest-and-lowest-3305848")))
        }
        
        else if (input$measure == "govspending_percentoftotal") {
            HTML(paste("<b>Measure Description</b><br>",
                       "The percent of spending that a nation spends on healthcare is defined
                 as the sum of public and private health expenditure as a percentage of GDP.<br>",
                       a("More Information Here", href="https://www.statista.com/statistics/268826/health-expenditure-as-gdp-percentage-in-oecd-countries/")))
        }
        
        else if (input$measure == "Total.Health.Spending") {
            HTML(paste("<b>Measure Description</b><br>",
                       "Total Health Spending is a measure of the percent of a nation's 
                       Gross Domestic Product (GDP) that is allocated to healthcare.<br>",
                       a("More Information Here", href="https://apps.who.int/nha/database")))
        }
        else if (input$measure == "population") {
            HTML(paste("<b>Measure Description</b><br>",
                       "Raw count of total people in a given country."))
        }
    })
    
    output$linePlot <- renderPlot({
        health_map %>% 
            filter(country %in% c(input$countries, input$countries2)) %>%
            ggplot(aes_string("year", input$measure2, color = "country")) + 
            geom_line(size = 2) +
            xlab("Year") + 
            ylab(pretty_names(input$measure2)) +
            ggtitle(paste(pretty_names(input$measure2), "in", input$countries, "and", input$countries2)) + 
            scale_color_discrete(name = "", labels = c(input$countries, input$countries2)) +
            scale_color_manual(values=c("#6699ff", "#FA8072")) +
            theme(
                text = element_text(size=16),
                panel.background = element_rect(fill = "white",
                                                colour = "white",
                                                size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightblue"), 
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                colour = "lightblue")
            )
    })
    
    
    
    
}

shinyApp(ui = ui, server = server)