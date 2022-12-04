library(plotly)
library(shiny)
library(gridlayout)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)


# we have a tabulation separator
campaign_data <- read.csv('data/marketing_campaign.csv', sep='\t')

# add age category column
campaign_data['Age'] <- 2022 - campaign_data['Year_Birth']
campaign_data <- campaign_data %>%
  mutate(Age_Category = case_when(
    Age <= 14 ~ 'Children',
    Age <= 24 ~ 'Youth',
    Age <= 64 ~ 'Adult',
    Age >= 65 ~ 'Senior',
  ))

# cleaning data
# Change Alone to single and remove YOLO and absurd
campaign_data["Marital_Status"][campaign_data["Marital_Status"] == "Alone"] <- "Single"
campaign_data <- campaign_data[!(campaign_data$Marital_Status %in% c("YOLO", "Absurd")), ]


age_category_list <- unique(campaign_data$Age_Category)
education_list <- unique(campaign_data$Education)
marital_status_list <- unique(campaign_data$Marital_Status)


ui <- grid_page(
  layout = c(
    "area2 header",
    "area1 area1 "
  ),
  row_sizes = c(
    "70px",
    "1.73fr"
  ),
  col_sizes = c(
    "140px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "Marketing Analysis Dashboard",
    alignment = "center",
    is_title = TRUE
  ),
  grid_card(
    area = "area1",
    tabsetPanel(
      tabPanel(
        title = "POC",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area2             area3             area0            ",
            "area4             area4             area0            "
          ),
          row_sizes = c(
            "0.27fr",
            "1.22fr",
            "1.51fr"
          ),
          col_sizes = c(
            "1.58fr",
            "0.96fr",
            "0.46fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Filters (Not meant to work for poc)",
            tabPanel(
              title = "My Shiny App",
              grid_container(
                layout = "area1",
                row_sizes = "1.61fr",
                col_sizes = "1.01fr",
                gap_size = "10px",
                grid_card(
                  area = "area1",
                  
                  sliderInput(
                    inputId = "inputId",
                    label = "Example Slider",
                    min = 0L,
                    max = 10L,
                    value = 5L,
                    width = "100%"
                  ),
                  textInput(
                    inputId = "myTextInput",
                    label = "Text Input",
                    value = ""
                  ),
                  checkboxGroupInput(
                    inputId = "myCheckboxGroup",
                    label = "Checkbox Group",
                    choices = list(
                      `choice a` = "a",
                      `choice b` = "b"
                    )
                  ),
                  radioButtons(
                    inputId = "myRadioButtons",
                    label = "Radio Buttons",
                    choices = list(
                      `choice a` = "a",
                      `choice b` = "b"
                    ),
                    width = "100%"
                  )
                )
              )
            )
          ),
          grid_card_text(
            area = "title_demographic",
            content = "Proof of Concept (data not cleaned)",
            alignment = "start"
          ),
          grid_card(
            area = "area2",
            plotOutput(
              outputId = "ex1",
              width = "100%",
              height = "400px"
            )
          ),
          grid_card(
            area = "area3",
            plotOutput(
              outputId = "ex3",
              width = "100%",
              height = "400px"
            )
          ),
          grid_card(
            area = "area4",
            plotOutput(
              outputId = "ex2",
              width = "100%",
              height = "350px"
            )
          )
        )
      ),
      tabPanel(
        title = "Demographic",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area2             area7             area0            ",
            "area4             area4             area0            "
          ),
          row_sizes = c(
            "0.27fr",
            "1.73fr",
            "1fr"
          ),
          col_sizes = c(
            "1.27fr",
            "1.27fr",
            "0.46fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Filters",
            tabPanel(
              title = "My Shiny App",
              grid_container(
                layout = ".",
                row_sizes = "1.72fr",
                col_sizes = "1.01fr",
                gap_size = "10px"
              )
            )
          ),
          grid_card_text(
            area = "title_demographic",
            content = "Demographic Analysis",
            alignment = "start"
          ),
          grid_card(area = "area2"),
          grid_card(area = "area4"),
          grid_card(area = "area7")
        )
      ),
      tabPanel(
        title = "Marketing",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area2             area3             area0            ",
            "area4             area4             area0            "
          ),
          row_sizes = c(
            "0.27fr",
            "1.73fr",
            "1fr"
          ),
          col_sizes = c(
            "1.27fr",
            "1.27fr",
            "0.46fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Filters",
            tabPanel(
              title = "My Shiny App",
              grid_container(
                layout = ".",
                row_sizes = "1.61fr",
                col_sizes = "1.01fr",
                gap_size = "10px"
              )
            )
          ),
          grid_card_text(
            area = "title_demographic",
            content = "Marketing Analysis",
            alignment = "start"
          ),
          grid_card(area = "area2"),
          grid_card(area = "area3"),
          grid_card(area = "area4")
        )
      ),
      tabPanel(
        title = "Sales",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area8             area8             area0            ",
            "area9             area4             area0            "
          ),
          row_sizes = c(
            "0.27fr",
            "1.73fr",
            "1fr"
          ),
          col_sizes = c(
            "1.27fr",
            "1.27fr",
            "0.46fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Filters",
            tabPanel(
              title = "My Shiny App",
              grid_container(
                layout = "area1",
                row_sizes = "1.61fr",
                col_sizes = "1.01fr",
                gap_size = "10px",
                grid_card(
                  area = "area1",
                  title = "Fig. 1:",
                  # Select which the age category to plot, select all by default
                  checkboxGroupInput(inputId = "selected_age",
                                     label = "Select the age category:",
                                     choices = age_category_list,
                                     selected = age_category_list),
                  
                  # Select which the education level to plot, select all by default
                  checkboxGroupInput(inputId = "selected_education",
                                     label = "Select the education level:",
                                     choices = education_list,
                                     selected = education_list),
                  
                  # Select which the marital status to plot, select all by default
                  checkboxGroupInput(inputId = "selected_marital_status",
                                     label = "Select the marital status:",
                                     choices = marital_status_list,
                                     selected = marital_status_list)
                )
              )
            )
          ),
          grid_card_text(
            area = "title_demographic",
            content = "Sales Analysis",
            alignment = "start"
          ),
          grid_card(area = "area4"),
          grid_card(area = "area8", 
                    plotOutput("amountGraph")),
          grid_card(area = "area9")
        )
      )
    )
  ),
  grid_card_text(
    area = "area2",
    content = "Logo",
    alignment = "center"
  )
)

server <- function(input, output) {
  
  data <- read.csv('data/marketing_campaign.csv', sep='\t')
  
  
  output$ex1 <- renderPlot({
    ggplot(data = data, mapping = aes(x = Income, y = MntWines,
                                      color = Marital_Status)) +
      geom_point() +
      labs(x = "Income ($)",
           y = "Money spent on wines ($)",
           title = "Example 1") +
      xlim(0, 105000) +
      theme_minimal()
  })
  
  output$ex2 <- renderPlot({
    ggplot(data = data, mapping = aes(x = factor(Education, 
             level=c('Basic', 'Graduation', '2n Cycle', 'Master', 'PhD')), 
             y = NumWebVisitsMonth, fill = Education)) +
      geom_violin() +
      geom_point(size = 0.5, position = position_jitter(width = 0.1)) +
      guides(fill = "none") +
      labs(x = "Level of education",
           y = "Number of monthly visits to the website",
           title = "Example 2") +
      theme_minimal()
  })
  
  data_ex3 <- data %>%
    group_by(Marital_Status, Education) %>%
    summarize(percCmp1 = mean(AcceptedCmp1))
  
  output$ex3 <- renderPlot({
    ggplot(data = data_ex3,
           mapping = aes(x = Marital_Status, y = Education, 
                         fill = percCmp1)) +
      geom_tile() +
      labs(x = "Marital Status", y = "Education level",
           title = "Example 3",
           fill = "Accepted 
promotion 
rate") +
      coord_equal() +
      theme_minimal()
  })
  
  
  # filter data
  amount_graph_filtered_data = reactive({
    req(input$selected_age) # ensure availablity of value before proceeding
    req(input$selected_education)
    req(input$selected_marital_status)
    filter(campaign_data, Age_Category %in% input$selected_age) %>%
      filter(Education %in% input$selected_education) %>%
      filter(Marital_Status %in% input$selected_marital_status)
  })
  
  
  # merge the MntWines, MntFruits, ... and the other amount columns in on column MntCategory
  product_data <- reactive({
    amount_graph_filtered_data()[c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")] %>%
      pivot_longer(cols=c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds"),
                   names_to="MntCategory",
                   values_to="Amount")
  })
  
  # get the percentage of the total amount per MntCategory
  product_data_percentage <- reactive({
    product_data() %>%
      mutate(MntCategory = factor(MntCategory)) %>%
      group_by(MntCategory) %>%
      summarise(total = sum(Amount)) %>%
      mutate(proportion = total / sum(total)) %>%
      arrange(desc(proportion)) %>%
      mutate(proportion = round(proportion, 2))
  })
  
  # plot the graph
  output$amountGraph <- renderPlot({
    ggplot(data = product_data_percentage()) +
      geom_pointrange(mapping = aes(x = reorder(MntCategory, -total), y = total, ymin = 0, ymax = total)) +
      labs(title="Percentage of amount spent on each category",
           x ="Category", y = "Total Expenses")
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = product_data_percentage(), 
                  options = list(pageLength = 10), 
                  colnames = c('Product category', 'Total expenses', 'Percentage'),
                  rownames = FALSE)
  })
  
               
}

shinyApp(ui, server)
