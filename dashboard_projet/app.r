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
library(scales)

ggthemr('pale')

###Default color palette at the application launch
colorblind_state <<- 1

# we have a tabulation separator
campaign_data <- read.csv('data/marketing_campaign.csv', sep='\t')

# add age category column
campaign_data['Age'] <- 2019 - campaign_data['Year_Birth']
campaign_data <- campaign_data %>%
  mutate(Age_Category = case_when(
    Age <= 35 ~ '35 and -',
    Age <= 45 ~ '36 - 45',
    Age <= 55 ~ '46 - 55',
    Age <= 65 ~ '55 - 65',
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
    Income >= 75000 ~ '75k+'
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
    "colorB header area2",
    "area1 area1 area1"
  ),
  row_sizes = c(
    "70px",
    "1.73fr"
  ),
  col_sizes = c(
    "140px",
    "1fr",
    "140px"
  ),
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "Marketing Analysis Dashboard",
    alignment = "center",
    is_title = TRUE
  ),
  actionButton("buttonCB", "Colorblind Mode OFF", 
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  grid_card(
    area = "area1",
    tabsetPanel(
      tabPanel(
        title = "Demographic",
        grid_container(
          layout = c(
            "title_demographic title_demographic title_demographic",
            "area2             area2             area0            ",
            "area7             area7             area0            "
          ),
          row_sizes = c(
            "0.27fr",
            "1.15fr",
            "1.50fr"
          ),
          col_sizes = c(
            "1.27fr",
            "1.27fr",
            "240px"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Filter",
            radioButtons(
              inputId = "demo_filter_group",
              label = "Filter by",
              choices = list(
                `Age group` = "Age_Category",
                `Number of children` = "Total_childrens",
                `Education level` = "Education",
                `Yearly income` = "Income_Range",
                `Marital Status` = "Marital_Status"
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
            "240px"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Filters",
            selectInput(inputId = "place_y", 
                        label = "X-axis:",
                        choices = c("Education" = "Education", 
                                    "Marital status" = "Marital_Status", 
                                    "Age group" = "Age_Category"), 
                        selected = "Age_Category"),
            selectInput(inputId = "place_x", 
                        label = "Y-axis:",
                        choices = c("In-store purchases" = "NumStorePurchases", 
                                    "Web purchases" = "NumWebPurchases", 
                                    "Catalog purchases" = "NumCatalogPurchases"), 
                        selected = "NumStorePurchases")
          ),
          grid_card(
            area = "area10",
            title = "Filter",
            selectInput(inputId = "xpromotionGraph",
                        label = "Filter by",
                        choices = list(
                          `Age group` = "Age_Category",
                          `Number of children` = "children_Count",
                          `Yearly income` = "Income_Range"
                        ),
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
            "1.32fr",
            "1.41fr"
          ),
          col_sizes = c(
            "1.27fr",
            "1.27fr",
            "240px"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            title = "Include/Exclude groups from the total",
            checkboxGroupInput(inputId = "selected_age",
                               label = "Available age groups:",
                               choices = age_category_list,
                               selected = age_category_list),
            
            # Select which the education level to plot, select all by default
            checkboxGroupInput(inputId = "selected_education",
                               label = "Available education levels:",
                               choices = education_list,
                               selected = education_list),
            
            # Select which the marital status to plot, select all by default
            checkboxGroupInput(inputId = "selected_marital_status",
                               label = "Available marital statuses:",
                               choices = marital_status_list,
                               selected = marital_status_list)
          ),
          grid_card(
            area = "area10",
            title = "Filters",
            selectInput(inputId = "x", 
                        label = "Demographic classification",
                        choices = c("Number of children" = "Children_Count", 
                                    "Income Range" = "Income_Range", 
                                    "Age category" = "Age_Category"), 
                        selected = "Age_Category"),
            
            
            selectInput(inputId = "y", 
                        label = "Product category",
                        choices = c("Wines" = "mean_MntWines", 
                                    "Fruits" = "mean_MntFruits",
                                    "Meat products" = "mean_MntMeatProducts", 
                                    "Fish products" = "mean_MntFishProducts",
                                    "Sweet product" = "mean_MntSweetProducts", 
                                    "Gold products" = "mean_MntGoldProds"), 
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

server <- function(input, output, session) {
  
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
      labs(title= "Distribution of consumers", y = "Proportion (%)") +
      theme(legend.position="none") +
      theme(text = element_text(size=18)) +
      labs(x = gsub("_", " ", input$demo_filter_group))
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
    ggplot(demo_per_filter_complain(), aes_string(
             x = input$demo_filter_group,
             y = 'proportionC',
             fill = input$demo_filter_group
           )) +
      geom_col() +
      labs(title= "Distribution of consumers who have complained at least once", y = "Proportion (%)") +
      theme(legend.position="none") +
      theme(text = element_text(size=18)) +
      labs(x = gsub("_", " ", input$demo_filter_group))
  })
  
  ###### Marketing places

  
  output$placeGraph <- renderPlot({
    ggplot(campaign_data) + 
      geom_boxplot(
        aes_string(x = input$place_y, y = input$place_x, fill = input$place_y),
        size = 1.5, outlier.size = 5
      ) +
      labs(title="Percentage of the number of purchases per place") +
      labs(title=if_else(input$place_x == "NumStorePurchases", "Number of in-store transactions per customer", if_else(input$place_x == "NumWebPurchases", "Number of web transactions per customer", "Number of catalog transactions per customer"))) +
      theme(legend.position="none") +
      theme(text = element_text(size=18)) +
      labs(x = gsub("_", " ", input$place_y))
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
      labs(title="Total amount spent on each product category",
           x ="Category", y = "Total Expenses ($)") +
      theme(legend.position="none") +
      theme(text = element_text(size=18)) + 
      scale_y_continuous(labels = label_comma())
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
      theme(text = element_text(size=18)) + 
      labs(title="Average expenses by customer and product type", fill=if_else(input$y == "mean_MntWines", "Average expense for\nwines ($)", if_else(input$y == "mean_MntMeatProducts", "Average expense for\nmeat products ($)", if_else(input$y == "mean_MntGoldProds", "Average expense for\ngold products ($)", if_else(input$y == "mean_MntFishProducts", "Average expense for\nfish products ($)", if_else(input$y == "mean_MntSweetProducts", "Average expense for\nsweets ($)", "Average expense for\nfruits ($)"))))))
  })
  
  #### Promotion graph
  
  df2 <- campaign_data
  
  df2$campaign_accepted <- df2$AcceptedCmp1 | df2$AcceptedCmp2 | df2$AcceptedCmp3 | df2$AcceptedCmp4 | df2$AcceptedCmp5
  
  df2$Children_Count <- rowSums(df2[, c("Kidhome", "Teenhome")])
  
  accepted_percentage_campaign <- reactive({
    
    df2 %>% group_by_(input$xpromotionGraph) %>%
      summarise(total_accepted = sum(campaign_accepted),
                total_per_category = n()) %>%
      mutate(Pourcentage = as.integer(total_accepted / total_per_category*100))
    
  })
  
  
  
  output$promotionGraph<- renderPlot({
    
    
    
    ggplot( accepted_percentage_campaign(),aes_string(x=input$xpromotionGraph, y='Pourcentage', color=input$xpromotionGraph)) +
      geom_segment( aes_string(xend=input$xpromotionGraph, yend=0), size=2) +
      geom_point( size=8) +coord_flip() +
      xlab("")+labs(x = gsub("_", " ", input$xpromotionGraph),y='Percentage (%)',
                    title="Most likely groups to respond to price reductions") +
      theme(legend.position="none") +
      theme(text = element_text(size=18))
    
  })
  observeEvent(input$buttonCB, {
    if (colorblind_state == 1)
    {
      colorblind_state <<- 2
      ggthemr('fresh')
      output$consumptionGraph <- renderPlot({
        ggplot(habitude_consommation(), aes_string(area = input$y, fill = input$y, label = input$x)) +
          geom_treemap() +
          geom_treemap_text(colour = "white",
                            place = "centre",
                            reflow = TRUE,
                            size = 17) +
          theme(text = element_text(size=18)) +
          labs(title="Average expenses by customer and product type", fill=if_else(input$y == "mean_MntWines", "Average expense for\nwines ($)", if_else(input$y == "mean_MntMeatProducts", "Average expense for\nmeat products ($)", if_else(input$y == "mean_MntGoldProds", "Average expense for\ngold products ($)", if_else(input$y == "mean_MntFishProducts", "Average expense for\nfish products ($)", if_else(input$y == "mean_MntSweetProducts", "Average expense for\nsweets ($)", "Average expense for\nfruits ($)"))))))
      })
      output$promotionGraph<- renderPlot({
        ggplot( accepted_percentage_campaign(),aes_string(x=input$xpromotionGraph, y='Pourcentage', color=input$xpromotionGraph)) +
          geom_segment( aes_string(xend=input$xpromotionGraph, yend=0), size=2) +
          geom_point( size=8) +coord_flip() +
          xlab("")+labs(x = gsub("_", " ", input$xpromotionGraph),y='Percentage (%)',
                        title="Most likely groups to respond to price reductions") +
          theme(legend.position="none") +
          theme(text = element_text(size=18))
      })
      output$amountGraph <- renderPlot({
        ggplot(data = product_data_percentage()) +
          geom_pointrange(mapping = aes(x = reorder(MntCategory, -total), y = total, ymin = 0, ymax = total, color = reorder(MntCategory, -total)), linewidth = 2, size = 3) +
          labs(title="Total amount spent on each product category",
               x ="Category", y = "Total Expenses ($)") +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) + 
          scale_y_continuous(labels = label_comma())
      })
      output$placeGraph <- renderPlot({
        ggplot(campaign_data) + 
          geom_boxplot(
            aes_string(x = input$place_y, y = input$place_x, fill = input$place_y),
            size = 1.5, outlier.size = 5
          ) +
          labs(title=if_else(input$place_x == "NumStorePurchases", "Number of in-store transactions per customer", if_else(input$place_x == "NumWebPurchases", "Number of web transactions per customer", "Number of catalog transactions per customer"))) +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) +
          labs(x = gsub("_", " ", input$place_y))
      })
      output$demoGraph <- renderPlot({
        ggplot(demo_per_filter_graph(), 
               mapping = aes_string(
                 x = input$demo_filter_group,
                 y = 'proportion',
                 fill = input$demo_filter_group
               )) +
          geom_col() +
          labs(title= "Distribution of consumers", y = "Proportion (%)") +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) +
          labs(x = gsub("_", " ", input$demo_filter_group))
      })
      output$demoComplain <- renderPlot({
        ggplot(demo_per_filter_complain(), 
               mapping = aes_string(
                 x = input$demo_filter_group,
                 y = 'proportionC',
                 fill = input$demo_filter_group
               )) +
          geom_col() +
          labs(title= "Distribution of consumers who have complained at least once", y = "Proportion (%)") +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) +
          labs(x = gsub("_", " ", input$demo_filter_group))
      })
      updateActionButton(session, "buttonCB", "Colorblind Mode ON")
    }
    else
    {
      colorblind_state <<- 1
      ggthemr('pale')
      output$consumptionGraph <- renderPlot({
        ggplot(habitude_consommation(), aes_string(area = input$y, fill = input$y, label = input$x)) +
          geom_treemap() +
          geom_treemap_text(colour = "black",
                            place = "centre",
                            reflow = TRUE,
                            size = 17) +
          theme(text = element_text(size=18)) +
          labs(title="Average expenses by customer and product type", fill=if_else(input$y == "mean_MntWines", "Average expense for\nwines ($)", if_else(input$y == "mean_MntMeatProducts", "Average expense for\nmeat products ($)", if_else(input$y == "mean_MntGoldProds", "Average expense for\ngold products ($)", if_else(input$y == "mean_MntFishProducts", "Average expense for\nfish products ($)", if_else(input$y == "mean_MntSweetProducts", "Average expense for\nsweets ($)", "Average expense for\nfruits ($)"))))))
      })
      output$promotionGraph<- renderPlot({
        ggplot( accepted_percentage_campaign(),aes_string(x=input$xpromotionGraph, y='Pourcentage', color=input$xpromotionGraph)) +
          geom_segment( aes_string(xend=input$xpromotionGraph, yend=0), size=2) +
          geom_point( size=8) +coord_flip() +
          xlab("")+labs(x = gsub("_", " ", input$xpromotionGraph),y='Percentage (%)',
                        title="Most likely groups to respond to price reductions") +
          theme(legend.position="none") +
          theme(text = element_text(size=18))
      })
      output$amountGraph <- renderPlot({
        ggplot(data = product_data_percentage()) +
          geom_pointrange(mapping = aes(x = reorder(MntCategory, -total), y = total, ymin = 0, ymax = total, color = reorder(MntCategory, -total)), linewidth = 2, size = 3) +
          labs(title="Total amount spent on each product category",
               x ="Category", y = "Total Expenses ($)") +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) + 
          scale_y_continuous(labels = label_comma())
      })
      output$placeGraph <- renderPlot({
        ggplot(campaign_data) + 
          geom_boxplot(
            aes_string(x = input$place_y, y = input$place_x, fill = input$place_y),
            size = 1.5, outlier.size = 5
          ) +
          labs(title=if_else(input$place_x == "NumStorePurchases", "Number of in-store transactions per customer", if_else(input$place_x == "NumWebPurchases", "Number of web transactions per customer", "Number of catalog transactions per customer"))) +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) +
          labs(x = gsub("_", " ", input$place_y))
      })
      output$demoGraph <- renderPlot({
        ggplot(demo_per_filter_graph(), 
               mapping = aes_string(
                 x = input$demo_filter_group,
                 y = 'proportion',
                 fill = input$demo_filter_group
               )) +
          geom_col() +
          labs(title= "Distribution of consumers", y = "Proportion (%)") +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) +
          labs(x = gsub("_", " ", input$demo_filter_group))
      })
      output$demoComplain <- renderPlot({
        ggplot(demo_per_filter_complain(), 
               mapping = aes_string(
                 x = input$demo_filter_group,
                 y = 'proportionC',
                 fill = input$demo_filter_group
               )) +
          geom_col() +
          labs(title= "Distribution of consumers who have complained at least once", y = "Proportion (%)") +
          theme(legend.position="none") +
          theme(text = element_text(size=18)) +
          labs(x = gsub("_", " ", input$demo_filter_group))
      })
      updateActionButton(session, "buttonCB", "Colorblind Mode OFF")
    }  
  })
  
}

shinyApp(ui, server)