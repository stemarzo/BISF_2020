ui <- navbarPage("Descriptive Analysis",
                 tabPanel("Price",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h5("Seleziona gli Assets:"),
                                checkboxInput("appl_c", "AAPL", value = TRUE),
                                checkboxInput("msft_c", "MSFT", value = TRUE),
                                checkboxInput("amzn_c", "AMZN", value = TRUE),
                                checkboxInput("goog_c", "GOOG", value = TRUE)
                              ),
                              mainPanel(
                                plotOutput("plotAdjusted"))
                            )
                          )
                        ),
                 tabPanel("Return",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h5("Seleziona gli Assets:"),
                                checkboxInput("appl_r", "AAPL", value = TRUE),
                                checkboxInput("msft_r", "MSFT", value = TRUE),
                                checkboxInput("amzn_r", "AMZN", value = TRUE),
                                checkboxInput("goog_r", "GOOG", value = TRUE)
                             ),
                              mainPanel(
                                plotOutput("plotReturn"))
                              )
                            )
                          ),
                 
                 tabPanel("Istogramma",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h5("Seleziona l'Asset:"),
                                selectInput("ass_l", "Asset:", 
                                            choices=assets_name),
                                checkboxInput("densita", "Mostra la Dispersione", value = FALSE),
                              ),
                              mainPanel(
                                plotOutput("plotHisto"))
                              )
                            )
                          ),
                 tabPanel("Boxplot",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h5("Seleziona l'Asset:"),
                                selectInput("ass_bp", "Asset:", 
                                            choices=assets_name),
                                ),
                              mainPanel(
                                plotOutput("plotBox"))
                            )
                          )
                 ),
                 tabPanel("qq-Plot",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h5("Seleziona l'Asset:"),
                                selectInput("ass_qq", "Asset:", 
                                            choices=assets_name),
                                checkboxInput("qqline", "Mostra qq-line", value = FALSE),
                                ),
                              mainPanel(
                                plotOutput("plotQQ"))
                            )
                          )
                 ),
                 tabPanel("Diagnostic Plot",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h5("Seleziona gli Assets:"),
                                checkboxInput("appl_d", "AAPL", value = TRUE),
                                checkboxInput("msft_d", "MSFT", value = TRUE),
                                checkboxInput("amzn_d", "AMZN", value = TRUE),
                                checkboxInput("goog_d", "GOOG", value = TRUE)
                              ),
                              mainPanel(
                                plotOutput("plotDiagn"))
                            )
                          )
                 ))