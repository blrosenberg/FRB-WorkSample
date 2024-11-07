library(shiny)
library(tidyverse)
options(scipen=999)

all_age = read.csv('all_age.csv') #Relative path may need adjusting on your machine

factorize = function(tbl, groupby="month") { #Converts data into summable booleans
  return(
    tbl %>% 
      mutate(inLF = PREMPNOT > 0 & PREMPNOT < 3, 
             outLF = PREMPNOT > 2,
             employed = PREMPNOT == 1,
             unemployed = PREMPNOT == 2,
             other = PREMPNOT == -1,
             all = 1,
             complete = all-other) %>%
      group_by(tbl[groupby]) %>%
      summarize_at(.vars=vars(inLF, outLF, employed, unemployed, other, all, complete), .funs=sum)
  )
}

monthscale = scale_x_continuous(breaks=c(1:12), labels=month.abb)
my_palette= c("#64afff","#785ef0","#dc267f", "#fe6100","#ffb000")
my_theme = theme_bw() +
  theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
        plot.caption = element_text(hjust=0, face="italic", size=8))


## ShinyApp

ui = fluidPage(
  fluidRow(
    column(12, plotOutput('unemp_sex')),
    column(12, sliderInput('age', label="Age Range:", value=c(24, 54), min=15, max=85))
  )
)

server = function(input, output, session) {
  ageFiltered = reactive(
    all_age %>% filter(PRTAGE >= input$age[1] & PRTAGE <= input$age[2])
  )
  
  All = reactive(factorize(ageFiltered()))
  Male = reactive(factorize(ageFiltered() %>% filter(PESEX==1)))
  Female = reactive(factorize(ageFiltered() %>% filter(PESEX==2)))
  
  unemp = reactive(
    data.frame(
      month = c(All()$month, All()$month),
      sex = c(rep("Male", 12), rep("Female", 12)),
      pop = c(Male()$inLF, Female()$inLF),
      rate = c(Male()$unemployed / Male()$inLF, Female()$unemployed / Female()$inLF)
    ) %>% mutate(
      SE = sqrt(rate*(1-rate)/pop),
      err = 1.96*SE)
  )
  
  output$unemp_sex = renderPlot(
    unemp() %>% 
      ggplot(aes(x=month, y=rate, col=sex, ymin=rate-err, ymax=rate+err)) + 
      geom_line(linetype=2) +
      geom_point() +
      geom_errorbar(alpha=0.3, width=0.2) +
      scale_y_continuous(labels=scales::percent) +
      scale_color_manual(values=c(my_palette[5], my_palette[1])) +
      monthscale +
      labs(x="Month", 
           y="Unemployment Rate", 
           subtitle=paste0(nrow(ageFiltered()), ' respondents'),
           color="Sex",
           title=paste0("Unemployment rate by sex\namong workers ages ",input$age[1],"–",input$age[2],"\nin the United States, 2023"),
           caption="Data Source: US Census Bureau — Current Population Survey") +
      my_theme +
      theme(legend.position="bottom")
    , res=96
  )
}

shinyApp(ui, server)
  
