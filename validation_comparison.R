library(ggplot2)
# Cairo provides nicer ggplot2 output when deployed on Linux
library(Cairo)
library(shinythemes)
library(dplyr)
library(DT)

# ##################### #
#                       #
#       Data Load       #
#                       #
# ##################### #

# Set directory
setwd("R:/Staff Folders/Brock/FY2018/Special Projects/Tobacco Smoke and Health/")

# Load data
proj <-
    read.csv(file = "20180904_tobacco_smoking_validation_scores.csv",
             header = TRUE,
             stringsAsFactors = FALSE)
val <- read.csv(file = "20180924_tobacco_validation_data.csv",
                header = TRUE,
                stringsAsFactors = FALSE)

# Join data
projvaldata <- merge(proj, val, by = "APPL_ID")

# Reduce data to desired columns
projval <-
    projvaldata[, c(
        'FY.y',
        'APPL_ID',
        'Activity.Code',
        'Stage',
        'Title',
        'Match.Score',
        'DESCRIP',
        'CREATED_DATE',
        'Category',
        'Threshold'
    )]

# Convert date fields into year
projval$CREATED_DATE <- format(as.Date(projval$CREATED_DATE), "%Y")

# Rename columns to improve readability
names(projval) <-
    c('Year',
      'Appl',
      'Activity',
      'Stage',
      'Title',
      'Score',
      'Comment',
      'Comment Year',
      'Category',
      'Threshold')

# ##################### #
#                       #
#       Shiny UI        #
#                       #
# ##################### #

ui <- fluidPage(
    # Page title and theme
    title = "Category Definition Validation Comparison", 
    titlePanel('Category Definition Validation Comparison'), 
    theme = shinytheme("yeti"), 
     
    
    # Filters
    
    # Category input; work in progress; just this one category for now
    fluidRow(column(
      width = 9,
      selectInput('select_cat',
                  'Category',
                  c(unique(
                      # projval$Category
                      as.character('Tobacco Smoke and Health')
                    ))))),
    
    fluidRow(column(width = 9,
                    h5("Select filters from the drop-down menu below to narrow the data in the charts shown."))),
    
    # Project year filter
    fluidRow(column(
        width = 3,
        selectInput('proj_year',
                    'Project Year',
                    c("All",
                      unique(
                          as.character(projval$Year)
                      )))),
        
        # Slider input; work in progress
        # column(
        #   width = 6,
        #   sliderInput(
        #     "com_year",
        #     label = "Comment Year",
        #     sep = "",
        #     step = 1,
        #     min = min(projval$Year),
        #     max = max(projval$Year),
        #     value = c(min(projval$Year), max(projval$Year))
        #   )
        # )), 
        
        # Comment year filter
        column(
            width = 3,
            selectInput('com_year',
                        'Comment Year',
                        c("All",
                          unique(
                              as.character(projval$'Comment Year')
                          )))),
        
        # Activity code filter
        column(
            width = 3,
            selectInput('activity_code',
                        'Activity',
                        multiple = TRUE,
                        c(unique(
                            as.character(projval$'Activity')
                        ))))
    ),
    
    # Tab panel for various chart or table outputs
    mainPanel(tabsetPanel(
        type = "tabs",
        tabPanel(
            "Distribution",
            h5("This chart depicts the distribution of match scores across validated projects. Each dot represents an individual project. Each solid horizontal line represents a median match score. Each dotted horizontal line represents a threshold value. Click or click and drag to select projects. More information about selected projects will be displayed in the table below the chart."),
            # Displays distribution chart
            fluidRow(column(
                width = 12,
                plotOutput(
                    "plot1",
                    height = 600,
                    # Equivalent to: click = clickOpts(id = "plot_click")
                    click = "plot1_click",
                    brush = brushOpts(id = "plot1_brush")
                )
            )),
            
            # Displays project table based on selection from distribution chart
            fluidRow(column(
                width = 12,
                h4("Project listing"),
                dataTableOutput("table1")
            ))
        ),
        tabPanel("Count",
                 h5("These charts depict the count of projects in and out of each category based on category definition stage."),
                 # Displays count chart (in)
                 fluidRow(column(
                     width = 6,
                     plotOutput(
                         "plot2",
                         height = 600
                         # Click operations to get projects in/near selection; disabled
                         # click = "plot2_click",
                         # brush = brushOpts(id = "plot2_brush")
                     )
                 ),
                 # Displays count chart (out)
                 column(
                     width = 6,
                     plotOutput(
                         "plot3",
                         height = 600
                         # Click operations to get projects in/near selection; disabled
                         # click = "plot3_click",
                         # brush = brushOpts(id = "plot3_brush")
                     )
                 ))),
        tabPanel(
            "Difference",
            h5(
                "This chart and table display the project difference between the category definition stages by comment type."
            ),
            fluidRow(
                column(width = 6,
                       # Comment filter selection
                       selectInput('diff_comment',
                                   'Comment',
                                   c(unique(
                                       as.character(projval$Comment)
                                   )))),
                fluidRow(
                    column(width = 12,
                           # Displays difference chart
                           plotOutput(
                               "plot4",
                               height = 600
                           ))
                ),
                # Displays difference table  
                column(width = 12,
                       dataTableOutput("table2"))
            )
        )
    ))
)

# #################### #
#                      #
#     Shiny server     #
#                      #
# #################### #

server <- function(input, output, session) {
    # Returns selected category
    output$selected_cat <- renderText({ 
        paste(input$select_cat)
    })
    
    # Filter distribution chart data based on filter input
    plotdata <- reactive({
        plotdata <- projval
        
        # Checks commente year filter
        if (input$com_year != "All") {
            plotdata <- plotdata %>% filter(plotdata$'Comment Year' == input$com_year)
        }
        
        # Checks project year filter
        if (input$proj_year != "All") {
            plotdata <- plotdata %>% filter(plotdata$Year == input$proj_year)
        }
        
        # Checks activity code filter (multiple values)
        if (!is.null(input$activity_code)) {
            plotdata <- plotdata %>% filter(plotdata$Activity %in% input$activity_code)
        }
        
        plotdata <- plotdata %>% as.data.frame()
    })
    
    # Renders distribution chart
    output$plot1 <- renderPlot({
        # Checks for bad data source (i.e., no projects after filters), otherwise displays message
        # req(nrow(plotdata()) > 0)
        validate(
            need(nrow(plotdata()) > 0, message = "There are no projects to display that meet your selection criteria.")
        )
        
        val_in_counts <- plotdata() %>%
            group_by(Stage, Comment) %>%
            filter(Score > Threshold) %>%
            summarise(in_count = n()) %>%
            as.data.frame()
        
        val_out_counts <- plotdata() %>%
            group_by(Stage, Comment) %>%
            filter(Score < Threshold) %>%
            summarise(out_count = n()) %>%
            as.data.frame()
        
        ggplot(plotdata(), aes(x = Stage, y = Score)) +
            facet_grid(~ Comment) +
            geom_point(aes(color = Comment)) +
            # Threshold read from dataset here; can be set manually if desired
            # Adds label for threshold; not enabled
            # geom_text(aes(1, projval$Threshold, label = projval$Threshold),
            #           vjust = 2, hjust = -0.2) +
            geom_hline(yintercept = projval$Threshold, linetype = 2) +
            stat_summary(
                fun.y = median,
                fun.ymin = median, 
                fun.ymax = median,
                geom = "crossbar",
                width = 0.2
            ) +
            ylab("Match Score") +
            xlab("Category Definition") +
            theme_minimal() +
            theme(legend.position = "none")
    })
    
    # Renders table of distribution projects
    output$table1 <- DT::renderDataTable({
        table1_data()
    }, selection = 'single', style = 'bootstrap', extensions = 'Buttons', rownames = FALSE, options = list(
        autoWidth = TRUE,
        # Dom denotes what and where the buttons are placed
            # B - Buttons extension
            # l - Length menu (10, 25, etc.); options set below
            # f - Search option
            # i - Page information
            # p - Page navigation
        dom = "<'row'<'col-md-3'B><'col-md-3'l><'col-md-6'f>><'row'<'col-md-6'><'col-md-6'>><'row'<'col-md-12't>><'row'<'col-md-6'i><'col-md-6'p>>",
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = c(10, 25, 50, 100, 250)
    ))
    
    # Get project output for distribution chart based on chart selection (click or brush)
    table1_data <- reactive({
        if (is.null(input$plot1_brush)) {
            table1_data <- nearPoints(plotdata(), input$plot1_click, addDist = FALSE)
        } else {
            table1_data <- brushedPoints(plotdata(), input$plot1_brush)
        }
    })
    
    # Gets row of selected project (in distribution table) and displays modal when clicked
    observeEvent(input$table1_rows_selected,
                 {
                     # Filters modal data
                     modalData <-
                         projvaldata %>% 
                         filter(APPL_ID == table1_data()[input$table1_rows_selected,]$Appl) %>% 
                         filter(Stage == table1_data()[input$table1_rows_selected,]$Stage) %>% 
                         t() %>%
                         as.data.frame()
                     
                     # Renders modal
                     showModal(modalDialog(
                         title = a(table1_data()[input$table1_rows_selected,]$Title, href=sprintf(
                             "https://apps.era.nih.gov/rcdc/viewExtractedText.era?applicationID=%s",
                             table1_data()[input$table1_rows_selected, ]$Appl
                         ), target="_blank"),
                         HTML(sprintf("It looks like this project has %s validations. Click the title of the project in the header above to view the project text in the eRA system.
                          <br><br>", ncol(modalData))),
                         renderTable(modalData, rownames = TRUE, colnames = FALSE),
                         easyClose = TRUE, 
                         size = 'l'
                     ))
                 })
    
    # Renders count chart (in)
    output$plot2 <- renderPlot({
        # Checks for bad data source (i.e., no projects after filters), otherwise displays message
        validate(
            need(nrow(plotdata()) > 0, message = "There are no projects to display that meet your selection criteria.")
        )
        
        val_in_counts <- plotdata() %>%
            group_by(Stage, Comment) %>%
            filter(Score > Threshold) %>%
            summarise(Count = n()) %>%
            as.data.frame()
        
        # Total counts for percentage (validated projects / total projects); WIP
        # total_counts <- plotdata() %>%
        #   group_by(Stage) %>%
        #   filter(Score > Threshold) %>%
        #   summarise(count = n()) %>%
        #   as.data.frame()
        
        ggplot(val_in_counts, aes(x = Stage, y = Count)) +
            facet_wrap( ~ Comment, scales = "free_y") +
            geom_bar(stat = 'identity', aes(fill = Comment)) +
            geom_text(aes(label = Count), vjust = 10) +
            ylab("Count In") +
            theme_minimal() +
            theme(legend.position = "none")
    })
    
    # Renders count chart (out)
    output$plot3 <- renderPlot({
        # Checks for bad data source (i.e., no projects after filters), otherwise displays message
        validate(need(nrow(plotdata()) > 0, message = "There are no projects to display that meet your selection criteria."))
        
        val_out_counts <- plotdata() %>%
            group_by(Stage, Comment) %>%
            filter(Score < Threshold) %>%
            summarise(Count = n()) %>%
            as.data.frame()
        
        ggplot(val_out_counts, aes(x = Stage, y = Count)) +
            facet_wrap(~ Comment, scales = "free_y") +
            geom_bar(stat = 'identity', aes(fill = Comment)) +
            geom_text(aes(label = Count), vjust = 10) + 
            ylab("Count Out") +
            theme_minimal() +
            theme(legend.position = "none")
    })
    
    # Difference chart output
    output$plot4 <- renderPlot({
        diff_trend <- plotdata() %>%
            group_by(Year, Stage, Comment) %>%
            filter(Score > Threshold) %>%
            filter(Comment == input$diff_comment) %>%
            summarise(count = n()) %>%
            as.data.frame()
        diff_trend$Year <- as.factor(diff_trend$Year)
        
        # Checks for bad data source (i.e., no projects after filters), otherwise displays message
        validate(need(nrow(diff_trend) > 0, message = "There are no projects to display that meet your selection criteria."))
        
        ggplot(diff_trend, aes(x = Year, y = count)) +
            facet_wrap(~ Comment, scales = "free_y") +
            geom_point(aes(group = Stage, color = Stage)) +
            geom_line(aes(group = Stage, color = Stage)) +
            geom_text(aes(label = count), vjust=-1) +
            labs(x = "Project Year", y = "Count") +
            theme_minimal() +
            theme(legend.position = "top")
        
    })
    
    # Provides data for difference table based on input filters
    table2_data <- reactive({
        analytical_proj <-
            filter(plotdata(), Stage == "Analytical", Score >= Threshold) %>%
            filter(Comment == input$diff_comment)
        official_proj <-
            filter(plotdata(), Stage == "Official", Score >= Threshold) %>%
            filter(Comment == input$diff_comment)
        
        # Symmetric difference of two project lists by stage (in)
        table2_diff <- setdiff(union(analytical_proj$Appl, official_proj$Appl),
                               intersect(analytical_proj$Appl, official_proj$Appl))
        table2_data <- plotdata() %>% filter(Appl %in% table2_diff) %>% as.data.frame()
    })
    
    # Difference table output based on in/out selection
    output$table2 <- DT::renderDataTable({
        table2_data()
    }, selection = 'single', extensions = 'Buttons', style = 'bootstrap', rownames = FALSE, options = list(
        autoWidth = TRUE,
        # Denotes where the buttons are placed
        dom = "<'row'<'col-md-3'B><'col-md-3'l><'col-md-6'f>><'row'<'col-md-6'><'col-md-6'>><'row'<'col-md-12't>><'row'<'col-md-6'i><'col-md-6'p>>",
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = c(10, 25, 50, 100)
    ))
    
    # Gets row of selected project (in difference table) and displays modal when clicked
    observeEvent(input$table2_rows_selected,
                 {
                     # Filters modal data
                     modalData <-
                         projvaldata %>% 
                         filter(APPL_ID == table2_data()[input$table2_rows_selected,]$Appl) %>% 
                         t() %>%
                         as.data.frame()
                     
                     # Renders modal
                     showModal(modalDialog(
                         title = a(table2_data()[input$table2_rows_selected,]$Title, href=sprintf(
                             "https://apps.era.nih.gov/rcdc/viewExtractedText.era?applicationID=%s",
                             table2_data()[input$table2_rows_selected, ]$Appl
                         ), target="_blank"),
                         HTML(sprintf("This project has %s records. Click the title of the project in the header above to view the project text in the eRA system.
                                  <br><br>", ncol(modalData))),
                         renderTable(modalData, rownames = TRUE, colnames = FALSE),
                         easyClose = TRUE, 
                         size = 'l'
                     ))
                 })
}

# Runs app
shinyApp(ui, server)