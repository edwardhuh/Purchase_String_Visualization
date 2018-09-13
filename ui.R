# -----------------------------------------------------------------------------
# Purchase String Visualization (ui.R)
# By : Edward Huh 
# Date: August 31, 2018
# Details : Addition of view 5, additional comment for documentation
# -----------------------------------------------------------------------------


shinyUI(
  dashboardPage(
    ## use to modify the title of the application.
    dashboardHeader(title = "Purchase String"),
    # details on the sidebar of the dashboard
    dashboardSidebar(
      fileInput(inputId = "file",
                # modify this 'label' element to change the comment displayed 
                # above 'Upload' button
                label = "Upload Purchase Data",
                # this part allows the Shiny app to only take in csv files.
                # Modify this to include other file formats
                multiple = TRUE, accept =  c(
                  "text/csv",
                  ".csv"),
                buttonLabel = "Upload"),
      sidebarMenu(
        # modify first item to alter display name of tabs
        menuItem("% Tiers", tabName = "vis1", 
                 icon = icon("bar-chart")),
        menuItem("Entry Trial and Repeat", tabName = "vis2", 
                 icon = icon("bar-chart")),
        menuItem("Purchase Strings", tabName = "vis3", 
                 icon = icon("share")),
        menuItem("Repeat and Switch", tabName = "vis4", 
                 icon = icon("bar-chart")),
        menuItem("Visualization 5", tabName = "vis5", 
                 icon = icon("bar-chart"))
      )
    ), # /end of sidebar
    dashboardBody(
      tabItems(
        # First tab content.
        # Do NOT alter 'tabName = "vis1"'. Instead, alter 'title' in box() 
        # to change graph title. 
        tabItem(tabName = "vis1",
                fluidRow(
                  box(title = "% Tiers", background = "maroon", 
                      solidHeader = TRUE, plotlyOutput(outputId = "vis1plot"),
                      height = 500, width = 1000),
                  box(colourInput("vis1col", "Select colour", "steelblue", 
                                  palette = c("limited")))
                )
        ),
        # Second tab content.
        # Do NOT alter 'tabName = "vis2"' This is reference. 
        # NOT the actual title of the dashboard.
        # Instead, alter 'title' in box() to change graph title. 
        tabItem(tabName = "vis2",
                fluidRow(
                  box(title = "Point of Entry Trial and Repeat", 
                      background = "blue", solidHeader = TRUE,
                      plotlyOutput(outputId = "vis2plot"), height = 500, 
                      width = 1000),
                  box(colourInput("vis2col_t", "Select colour for Triers", 
                                  "steelblue", palette = c("limited"))),
                  box(colourInput("vis2col_r", "Select colour for Repeaters",
                                  "orange", palette = c("limited")))
                )
        ),
        # Third tab content.
        # Do NOT alter 'tabName = "vis3"' This is reference. 
        # NOT the actual title of the dashboard.
        tabItem(tabName = "vis3",
                fluidRow(
                  box(uiOutput(outputId = "choose_prod1"), width = 200),
                  uiOutput("sankey_plots")
                )
        ),
        # Fourth tab content.
        # Do NOT alter 'tabName = "vis4"' This is reference. 
        # NOT the actual title of the dashboard.
        # Instead, alter 'title' in box() to change graph title. 
        tabItem(tabName = "vis4",
                fluidRow(
                  box(title = "Repeat and Switch by Point of Entry", 
                      background = "green", solidHeader = TRUE,
                      plotlyOutput(outputId = "vis4plot"), height = 500, 
                      width = 1000),
                  box(colourInput("vis4col_r", "Select colour for Repeat",
                                  "steelblue", palette = c("limited"))),
                  box(colourInput("vis4col_s", "Select colour for Switch", 
                                  "orange", palette = c("limited")))
                )
        ),
        # Fifth tab content.
        # Do NOT alter 'tabName = "vis5"' This is reference. NOT the actual title of the dashboard.
        # Instead, alter 'title' in box() to change graph title. 
        # Altering color with these reactive elements are more difficult. I would not recommend.
        tabItem(tabName = "vis5",
                fluidRow(
                  box(uiOutput(outputId = "choose_prod2"), width = 200),
                  uiOutput("bar_plots")
                )
        )
      ) # /end of tabItems
    ) # /end of body
  )
) # /end of ShinyUI

