suppressMessages(library("shiny"))
suppressMessages(library("htmltools"))
suppressMessages(library("DT"))
suppressMessages(library("rhdx"))
suppressMessages(library("tidyverse"))
set_rhdx_config(hdx_site = "prod")
## ds <- search_datasets(rows = 50L)

## df <- map_df(ds, as_tibble) %>%
##   select(dataset_title, dataset_name, organization_name) %>%
##   mutate(url = paste0("https://data.humdata.org/dataset/", dataset_name),
##          url =  paste0("<a href='", url, "'>", url,"</a>"))

search_datasets_tbl <- function(query = "*:*", rows = 10L) {
  search_datasets(query = query, rows = rows) %>%
    map_df(as_tibble) %>%
    select(dataset_title, organization_name, resources_format, locations_name, tags_name)
  ## %>%
    ## select(dataset_title, dataset_name, organization_name) %>%
    ## mutate(url = paste0("https://data.humdata.org/dataset/", dataset_name),
    ##        url =  paste0("<a href='", url, "'>", url,"</a>"))
  
}

## df <- search_datasets_tbl(rows = 1000)
## names(df)
## df

## map(df$tags_name, ~ any(.x %in% "hxl"))

## df %>%
##   filter(map_lgl(tags_name, ~ any(.x %in% c("idp", "hxl")))) %>%
##   select(dataset_title, organization_name)

## Start with full search that will be filtered

ui <- fluidPage(
  titlePanel("HDX browser"),
  sidebarPanel(
    textInput("search",
              h3("Search datasets"),
              value = "*:*"),
    numericInput("rows",
                 h3("Numbers of matching rows to return"),
                 value = 10),
    selectInput("tags",
                "Tags",
                multiple = TRUE,
                choices = "")
  ),
  mainPanel(
    DTOutput("tbl")
  )
)


server <- function(input, output, session) {
  search_results <- reactive({
    search_datasets_tbl(input$search, input$rows)
  }) 

  observe({
    tags <- unique(unlist(search_results()$tags_name))
    updateSelectInput(session = session,
                      inputId = "tags",
                      label = "Tags",
                      choices = tags)})
  
  filtered_results <- reactive({search_results() %>%
                                 filter(map_lgl(tags_name, ~ any(.x %in% input$tags))) %>%
                                 select(dataset_title, organization_name)})
  
  ## dt <- datatable(search_results(), escape = FALSE)
  output$tbl <- renderDT(filtered_results())
}


shinyApp(ui = ui, server = server)

