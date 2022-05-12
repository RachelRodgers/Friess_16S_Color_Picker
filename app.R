# 16S Family Community Composition Plot Color Picker
# For choosing abundance filtering and coloring
# Rachel Rodgers, May 2022

library("shiny")
library("phyloseq")
library("colourpicker")
library("ggplot2")

source("shared_R_scripts/Helper_Functions.R")

options(shiny.sanitize.errors = FALSE)

#----- Plot Data -----#

familyAbdDF <- readRDS("data/familyAbdDF.RDS")
plottedTaxa <- sort(unique(familyAbdDF$Family))

# Color data:
# Emulate ggplot2's default color scheme -
GetGGColors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100) [1:n]
}

defaultColorPalette <- GetGGColors(length(plottedTaxa))

colorLUT <- defaultColorPalette
names(colorLUT) <- plottedTaxa

#----- Globals -----#

originalColorLUT <- colorLUT
wasReset <- FALSE

#----- UI -----#

ui <- navbarPage("Friess 16S: Community Plot Color Picker",
                 # first tab
                 tabPanel(title = "Family",
                          fluidRow(
                            column(4, h4("Color Picker"),
                                   # select a family to be re-colored
                                   selectInput(inputId = "taxa", 
                                               label = "Taxa to Re-color:",
                                               choices = plottedTaxa),
                                   br(),
                                   # bring up the color picker wheel
                                   colourInput(inputId = "newColor", label = "New Color:"),
                                   br(),
                                   # apply the new color
                                   actionButton(inputId = "recolor", label = "Apply"),
                                   br(),
                                   br())),
                          # here's a reset button to go back to default colors
                          fluidRow(column(4,
                                          actionButton(inputId = "reset", 
                                                       label = "Reset Default Colors"))
                                   ),
                          hr(),
                          # show the plot
                          plotOutput("plot"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          hr(),
                          fluidRow(
                            # list the colors
                            column(5, offset = 1, h4("Current Color Palette:"),
                                   tableOutput(outputId = "hex_codes"),
                                   br(),
                                   # download button for file of colors
                                   downloadButton(outputId = "download_colors",
                                                  label = "Download"))),
                          br(),
                          br()
                          ))

server <- function(input, output) {
  #----- Logic -----#
  ResetStatus <- observeEvent(input$reset, {
    wasReset <<- TRUE
  })
  
  GetColors <- eventReactive(c(input$recolor, input$reset), {
    #observe({print(wasReset)})
    if (input$recolor == 0) {
      newPalette <- defaultColorPalette
      
    } else if (wasReset == TRUE) {
      newPalette <- defaultColorPalette
      colorLUT <<- originalColorLUT
      wasReset <<- FALSE 
      
    } else {
      selectedName <- input$taxa
      newColor <- input$newColor
      newColorLUT <- colorLUT
      newColorLUT[[selectedName]] <- newColor
      # update colorLUT so changes accumulate
      colorLUT <<- newColorLUT
      # generate the new palette
      newPalette <- unname(newColorLUT)
    }
    
    return(newPalette)
    
  }, ignoreNULL = FALSE)
  
output$plot <- renderPlot({
  
  ggplot(familyAbdDF,
         aes_string(x = "Sample", y = "Abundance", fill = "Family")) +
    geom_bar(stat = "identity", width = 1, color = "grey14") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    facet_wrap("Group ~ Day", scales = "free", nrow = 2, ncol = 2) +
    scale_fill_manual(values = GetColors())
  
  }, height = 650, width = 950)
  
MakeColorDF <- reactive({
  currentColors <- GetColors()
  colorDF <- data.frame("colors" = currentColors,
                        "taxa" = plottedTaxa)
  return(colorDF)
  })

  
output$hex_codes <- renderTable({
  MakeColorDF()
  })
  
output$download_colors <- downloadHandler(
  filename = "selected_colors.txt",
  content = function(file) {
    write.table(MakeColorDF(), file, 
                sep = "\t", row.names = FALSE, quote = FALSE)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
