suppressMessages(library("shiny"))
suppressMessages(library("DT"))
suppressMessages(library("rhdx"))
suppressMessages(library("tidyverse"))

rhdx_setup(hdx_site = "prod")
ds <- search_in_hdx(rows = 50L)

df <- map_df(ds, as_tibble) %>%
  select(dataset_title, dataset_name, organization_name) %>%
  mutate(url = paste0("https://data.humdata.org/dataset/", dataset_name),
         url =  paste0("<a href='", url, "'>", url,"</a>"))

ui <- fluidPage(
  titlePanel("HDX browser"),
  mainPanel(
    DTOutput("tbl")))

server <- function(input, output) {
  dt <- datatable(df, escape = FALSE)
  output$tbl <- renderDT(dt)
}


shinyApp(ui = ui, server = server)