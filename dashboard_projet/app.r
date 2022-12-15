library(plotly)
library(shiny)
library(gridlayout)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)
library(ggthemr)
library(treemapify)
library(ggrepel)
library(ggfittext)

ggthemr('pale')

# we have a tabulation separator
campaign_data <- read.csv('data/marketing_campaign.csv', sep='\t')

# add age category column
campaign_data['Age'] <- 2019 - campaign_data['Year_Birth']
campaign_data <- campaign_data %>%
  mutate(Age_Category = case_when(
    Age <= 35 ~ '35 and -',
    Age <= 45 ~ '36-45',
    Age <= 55 ~ '46-55',
    Age <= 65 ~ '55-65',
    Age > 65 ~ '66 and +'
  ))

# add income range
campaign_data <- campaign_data %>%
  mutate(Income_Range = case_when(
    Income < 10000 ~ '0 - 10k',
    Income < 20000 ~ '10k - 20k',
    Income < 45000 ~ '20k - 45k',
    Income < 60000 ~ '45k - 60k',
    Income < 75000 ~ '60k - 75k',
    Income >= 75000 ~ '75k+',
  ))

campaign_data['Total_childrens'] <- campaign_data['Kidhome'] + campaign_data['Teenhome']


# cleaning data
# Change Alone to single and remove YOLO and absurd
campaign_data["Marital_Status"][campaign_data["Marital_Status"] == "Alone"] <- "Single"
campaign_data <- campaign_data[!(campaign_data$Marital_Status %in% c("YOLO", "Absurd")), ]
campaign_data <- campaign_data[complete.cases(campaign_data), ]


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
        title = "Demographic",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area2             area7             area0            ",
            "area2             area7             area0            "
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
            radioButtons(
              inputId = "demo_filter_group",
              label = "Trier par",
              choices = list(
                `Âge` = "Age_Category",
                `Nombre d'enfant` = "Total_childrens",
                `Éducation` = "Education",
                `Revenu Annuel` = "Income_Range",
                `Statut Marital` = "Marital_Status"
              ),
              width = "100%"
            )
          ),
          grid_card_text(
            area = "title_demographic",
            content = "Demographic Analysis",
            alignment = "start"
          ),
          grid_card(area = "area2", plotOutput("demoGraph")),
          grid_card(area = "area7", plotOutput("demoComplain"))
        )
      ),
      tabPanel(
        title = "Marketing",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area2             area2             area0            ",
            "area3             area3             area10            "
          ),
          row_sizes = c(
            "0.27fr",
            "1.15fr",
            "1.58fr"
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
            selectInput(inputId = "place_y", 
                        label = "Y-axis:",
                        choices = c("Education" = "Education", 
                                    "Marital status" = "Marital_Status", 
                                    "Age category" = "Age_Category"), 
                        selected = "Age_Category")
          ),
          grid_card(
            area = "area10",
            title = "Filters",
            selectInput(inputId = "xpromotionGraph",
                        label = "Visualisation2_Category",
                        choices = c("Age_Category", "Income_Category","children_count"),
                        selected = "Age_Category")
          ),
          grid_card_text(
            area = "title_demographic",
            content = "Marketing Analysis",
            alignment = "start"
          ),
          grid_card(area = "area2", plotOutput("placeGraph")),
          grid_card(area = "area3", plotOutput("promotionGraph"))
        )
      ),
      tabPanel(
        title = "Sales",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area8             area8             area0            ",
            "area9             area9             area10            "
          ),
          row_sizes = c(
            "0.27fr",
            "1.25fr",
            "1.48fr"
          ),
          col_sizes = c(
            "1.27fr",
            "1.27fr",
            "0.46fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Fig. 1:",
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
          ),
          grid_card(
            area = "area10",
            title = "Fig. 2:",
            selectInput(inputId = "x", 
                        label = "Visualisation1_Category",
                        choices = c("Age_Category", "Income_Range"), 
                        selected = "Age_Category"),
            
            
            selectInput(inputId = "y", 
                        label = "Visualisation1_Product_type",
                        choices = c("mean_MntWines","mean_MntFruits","mean_MntMeatProducts","mean_MntFishProducts","mean_MntSweetProducts","mean_MntGoldProds"), 
                        selected = "mean_MntWines")
          ),
          grid_card_text(
            area = "title_demographic",
            content = "Sales Analysis",
            alignment = "start"
          ),
          grid_card(area = "area8", 
                    plotOutput("amountGraph")),
          grid_card(area = "area9",
                    plotOutput("consumptionGraph"))
        )
      )
    )
  ),
  img(src="logo.jpg", width="130px", height="80px")
)

server <- function(input, output) {
  
  data <- read.csv('data/marketing_campaign.csv', sep='\t')
  
  
  ###Demographic plots
  
  ##Graph répartition Consommateurs
  demo_per_filter_graph <- reactive({
    #Remove any N/A data from selected column
    campaign_data <- campaign_data[!(is.na(campaign_data[input$demo_filter_group])),]
    
    
    #Make proportion from selected column
    campaign_data %>%
      group_by_at(input$demo_filter_group) %>%
      summarise(total = n()) %>%
      mutate(proportion = (total / sum(total))*100)
  })
  
  output$demoGraph <- renderPlot({
    ggplot(demo_per_filter_graph(), 
           mapping = aes_string(
             x = input$demo_filter_group,
             y = 'proportion',
             fill = input$demo_filter_group
           )) +
      geom_col() +
      labs(title= "Répartition des comsommateurs", y = "%")
  })
  
  
  ##Graph répartition Consommateurs s'ayant plain
  demo_per_filter_complain <- reactive({
    #Remove any N/A data from selected column
    
    campaign_data <- filter(campaign_data, Complain == 1)
    
    #Make proportion from selected column
    campaign_data %>%
      group_by_at(input$demo_filter_group) %>%
      summarise(total = n()) %>%
      mutate(proportionC = (total / sum(total))*100)
  })
  
  output$demoComplain <- renderPlot({
    ggplot(demo_per_filter_complain(), 
           mapping = aes_string(
             x = input$demo_filter_group,
             y = 'proportionC',
             fill = input$demo_filter_group
           )) +
      geom_col() +
      labs(title= "Répartition des comsommateurs qui se sont plain.", y = "%")
  })
  
  ###### Marketing places
  place_data <- campaign_data %>%
    pivot_longer(cols=c("NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases"),
                 names_to="Places",
                 values_to="Count")
  
  place_data_per_filter <- reactive({
    place_data %>%
      group_by_at(c(input$place_y, 'Places')) %>%
      summarise(total = sum(Count)) %>%
      group_by_at(input$place_y) %>%
      mutate(proportion = total / sum(total))
  })
  
  output$placeGraph <- renderPlot({
    ggplot(place_data_per_filter()) +
      geom_tile(aes_string(x = 'Places', y = input$place_y, fill = 'proportion')) +
      labs(title="Percentage of the number of purchases per place")
  })
  
  
  ###### Popular products
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
      geom_pointrange(mapping = aes(x = reorder(MntCategory, -total), y = total, ymin = 0, ymax = total, color = reorder(MntCategory, -total)), linewidth = 2, size = 3) +
      labs(title="Percentage of amount spent on each category",
           x ="Category", y = "Total Expenses") +
      theme(legend.position="none")
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = product_data_percentage(), 
                  options = list(pageLength = 10), 
                  colnames = c('Product category', 'Total expenses', 'Percentage'),
                  rownames = FALSE)
  })
  
  #### Consumption graph
  df1<-campaign_data 
  
  habitude_consommation <-reactive({ 
    df1 %>%group_by_(input$x)  %>%
      summarize(mean_MntWines = 
                  mean(MntWines),
                mean_MntFruits = mean(MntFruits),
                mean_MntMeatProducts = mean(MntMeatProducts),
                mean_MntFishProducts = mean(MntFishProducts),
                mean_MntSweetProducts = mean(MntSweetProducts),
                mean_MntGoldProds = mean(MntGoldProds)
      ) })
  
  output$consumptionGraph <- renderPlot({
    ggplot(habitude_consommation(), aes_string(area = input$y, fill = input$y, label = input$x)) +
      geom_treemap() +
      geom_treemap_text(colour = "black",
                        place = "centre",
                        reflow = TRUE,
                        size = 17) +
      labs(title="Habitude de consommation par type de client")
  })
  
  #### Promotion graph
  
  df2 <- campaign_data
  
  df2$campaign_accepted <- df2$AcceptedCmp1 | df2$AcceptedCmp2 | df2$AcceptedCmp3 | df2$AcceptedCmp4 | df2$AcceptedCmp5
  
  df2$children_count <- rowSums(df2[, c("Kidhome", "Teenhome")])
  
  accepted_percentage_campaign <- reactive({
    
    df2 %>% group_by_(input$xpromotionGraph) %>%
      summarise(total_accepted = sum(campaign_accepted),
                total_per_category = n()) %>%
      mutate(Pourcentage = as.integer(total_accepted / total_per_category*100))
    
  })
  
  
  
  output$promotionGraph<- renderPlot({
    
    
    
    ggplot( accepted_percentage_campaign(),aes_string(x=input$xpromotionGraph, y='Pourcentage')) +
      geom_segment( aes_string(xend=input$xpromotionGraph, yend=0)) +
      geom_point( size=4, color="orange") +coord_flip() +
      theme_bw() +
      xlab("")+labs(x=input$xpromotionGraph,y='Pourcentage',
                    title="Pourcentage à répondre aux réductions de prix")
    
  })
}

shinyApp(ui, server)