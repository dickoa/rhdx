searchHDX <- function() {
  ui <- miniPage()
  server <- function(input, output, session) {
  }
   viewer <- dialogViewer("Search HDX", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}
