library(shiny)
library(ggplot2)
library(glue)
library(patchwork)

load("~/Documents/full_matched.RData")
drg_list <- list("Nervous system" = 20:103,
                 "Eye disorder" = 113:125,
                 "ENT disorder" = 135:159,
                 "Respiratory disorder" = 163:208,
                 "Cardiovascular disorder" = 215:320,
                 "GI disorder" = 326:446,
                 "Musculoskeletal disorder" = 453:556,
                 "Skin disorder" = 570:607,
                 "Metabolic disorder" = 614:645,
                 "Genitourinary/Reproductive disorder" = 650:795,
                 "Blood/cancer disorder" = 799:872,
                 "Mental disorder" = 876:887
)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      # sliderInput("integer", "Age:",
      #             min = 60, max = 64,
      #             value = 64),
      
      selectInput("drg", "Diagnosis:", 
                  choices= names(drg_list))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput("plot1"),
      plotOutput("plot2")
      
    )
  )
)

get_value <- function() {
  as.character(test_df$mpg[1])
}

get_percent_change <- function(matched_df, var_name, drg_in) {
  
  old_young <- full_matched_df %>% 
    filter(drg %in% drg_list[[drg_in]]) %>% 
    group_by(final_grouping) %>% 
    summarise(mean = mean(!!sym(var_name), na.rm = T)) %>% 
    pull(mean)
  
  scales::percent((old_young[1] - old_young[2]) / old_young[1])
  
}


server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
 
      ggplot() +
        annotate("text", x = 0, y = 0, 
                 label = glue("What if 64 year olds \n diagnosed with \n {input$drg}\nwere eligible for Medicare?"),
                 size = 20) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
  })
  
  output$plot2 <- renderPlot({
    
    plot1 <- ggplot() +
      annotate("text", x = 0, y = 0, 
               label = glue("Deaths from \n in-patient stays \n would change by \n {get_percent_change(full_matched_df, 'died', input$drg)}"),
               size = 10) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    
    plot2 <- ggplot() +
      annotate("text", x = 0, y = 0, 
               label = glue("ED usage \n would change by \n {get_percent_change(full_matched_df, 'ed_entry', input$drg)}"),
               size = 10) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    
    plot3 <- ggplot() +
      annotate("text", x = 0, y = 0, 
               label = glue("Cost per \n in-patient visit \n would change by \n {get_percent_change(full_matched_df, 'total_charges', input$drg)}"),
               size = 10) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    
    plot1 + plot2 + plot3
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)