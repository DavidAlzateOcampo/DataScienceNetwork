source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = main.Title,titleWidth = 360),
  print.dashboard.main.side(),
  print.dashboard.Body()
)
