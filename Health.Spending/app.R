library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(plyr)
library(scales)
library(reshape2)
library(forcats)
library(ggfortify)
dat <- read.csv("~/Documents/savingthehealthcarecrisis.csv")
final <- dat %>% filter(!is.na(GDP_percap)&!is.na(total_healthspending)&!is.na(dalys))
final$income_level[final$GDP_percap<1026] <- "Low" 
final$income_level[final$GDP_percap<4036 & final$GDP_percap>=1026] <- "Lower Middle"
final$income_level[final$GDP_percap<12476 & final$GDP_percap>=4036] <- "Upper Middle"
final$income_level[final$GDP_percap>=12476] <- "High"

ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("Government Healthcare Spending and Health Outcomes"),
  fluidRow(
    br(),
    column(12,"We constructed a linear regression model to measure the association between", (strong("disability-adjusted life years (DALYs)")), "and", (strong("the percentage of total government
                                                                                                                                                                spending that was spent on healthcare"))," in that country. After adjusting for country, year, and GDP/capita, we arrived at the model below. Note that estimates of other covariates
           included in the model are not reported for simplicity and that all reported estimates have a p-value less than 0.0001.")
    ),
  fluidRow(
    br(),
    column(12, p(strong("DALYs = 1423000 - 366(% Government Spent on Health) + 0.0133([% Government Spent on Health][GDP/capita])"))),
    br() 
  ),
  fluidRow(
    column(12, "From this model, we observe that an increase in the percentage of total government spending that is spent on healthcare is strongly associated with a decrease in DALYs.
           The interaction term with GDP/capita informs that the association becomes smaller, and possibly insignificant for countries with higher GDP/capita. This trend may exist because
           a higher income country may contribute more government funds to projects other than healthcare, though they still spend a larger absolute amount than lower income countries. Conversely,
           a lower income country may be more sensitive to what percentage of government spending is put towards healthcare because resources are already more limited. You can explore these trends across
           countries through different years, and examine how income level modifies the effect, in the interactive plot below.")
    ), 
  fluidRow(
    br(),
    column(2,
           sliderInput("Year1",label="Choose a Year", min = 1995, max = 2010, value = 2000, sep = "", ticks =FALSE),
           checkboxGroupInput("IncomeLevel",label = "Choose country income level(s)",choices=as.list(levels(as.factor(final$income_level))), selected=levels(as.factor(final$income_level)))
    ),
    column(10,plotOutput("Plot1") )
  ),
  fluidRow(
    br(),
    br(),
    column(12,"Next, we constructed a linear regression model to measure the association between", (strong("disability-adjusted life years (DALYs)")), "and", (strong("the percentage of total healthcare
                                                                                                                                                                      spending that was spent by the government")), "(as opposed to the private sector) in that country. After adjusting for country, year, and GDP/capita, we arrived at the model below. 
           Note that estimates of other covariates included in the model are not reported for simplicity and that all reported estimates have a p-value less than 0.0001.")
    ),
  fluidRow(
    br(),
    column(12, p(strong("DALYs = 1474000 - 95(% Government Share of Health Spending)"))),
    br() 
  ),
  fluidRow(
    column(12,"From this model, we observe that an increase the government share of health spending is strongly associated with a decrease in DALYs. Unlike, our previous model, GDP/capita does
           not signficantly modify the effect of this association. This implies that a government contributing to a greater share of healthcare spending is associated with better health outcomes
           in countries of any income level. You can explore this trend across countries through different years in the interactive plot below.")
    ),
  fluidRow(
    br(),
    column(2,
           sliderInput("Year2",label="Choose a Year", min = 1995, max = 2010, value = 2000, sep = "", ticks =FALSE)),
    column(10,plotOutput("Plot2"))),
  br(),
  br()
    )

server <- function(input, output) {
  output$Plot1 <- renderPlot({
    final %>% filter(year==input$Year1 & income_level %in% c(input$IncomeLevel)) %>% ggplot() + geom_point(aes((govspending_percentoftotal),dalys,color=income_level)) +
      ylab("Disability Adjusted Life Years (DALYs)") + xlab("% of Government Expenditures Spent on Healthcare") + scale_y_continuous(lim=c(1000,125000)) + 
      scale_x_continuous(lim=c(0,25)) + labs(color="Income Level") + ggtitle("Association Between Percent of Total Government Expenditures Spent on Healthcare and DALYs")
  })
  
  output$Plot2 <- renderPlot({
    final %>% filter(year==input$Year2) %>% ggplot() + geom_point(aes(gov_share_healthspending,dalys,color=income_level)) + ylab("Disability Adjusted Life Years (DALYs)") +
      xlab("Government Share of Total Healthcare Spending") +labs(color="Income Level") + ggtitle("Association Between Government Share of Healthcare Spending and DALYs")
    
  })
  
}


shinyApp(ui = ui, server = server)
