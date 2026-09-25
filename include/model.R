#Install needed packages #TODO move to package installation file
#install.packages(c("randomForest", "dplyr", "caret", "shiny", "plotly")) #Commented out since you only have to install them once

#Initilize libraries #TODO: Move to library initialization
library(randomForest)
library(dplyr)
library(caret)
library(shiny)
library(plotly)
library(htmlwidgets)

trainingdata <- list()
testingdata <- list()

wtb_vec <- seq(0, 43, by = 1) #Creates a vector, numbering the wtb flies, to be used as training data
rut_vec <- seq(0, 26, by = 1) #Creates a vector, numbering the rut flies, to be used as testing data

# Generate training data from the wtb flies
for(x in seq_along(wtb_vec)) {
  
  file_number <- wtb_vec[x]
  
  trainingdata[[x]] <- flyDataImport(
    paste0(
      "/media/fridik/Data 1/Project/DTSevaluations/example_data/Self-learning/wtb-",
      file_number,
      ".xml"
    )
  )
}

# Generate testing data from the rut flies
for(i in seq_along(rut_vec)) {
  
  file_number <- rut_vec[i]
  
  testingdata[[i]] <- flyDataImport(
    paste0(
      "/media/fridik/Data 1/Project/DTSevaluations/example_data/Self-learning/rut-",
      file_number,
      ".xml"
    )
  )
}

#Cycles through zoomable plots of the data, promting you to mark the pauses or declare no pauses, drag to zoom, use shift-drag to mark a pause, 
#r to save and continue and u to undo
  
  
data <- trainingdata[[1]]$rawdata

ui <- fluidPage(
  
  titlePanel("Pause annotation"),
  
  plotlyOutput("trace", height = "700px")
)

server <- function(input, output, session) {
  
  # Current training file
  current_file <- reactiveVal(1)
  
  # Pauses for the current file
  pauses <- reactiveVal(
    data.frame(
      start = numeric(0),
      end = numeric(0)
    )
  )
  
  # All saved annotations
  pause_annotations <- reactiveVal(list())
  
  # File used to permanently save annotations
  annotation_file <- "pause_annotations.rds"
  
  # Load previous annotations if they exist
  if (file.exists(annotation_file)) {
    
    pause_annotations(
      readRDS(annotation_file)
    )
  }
  
  output$trace <- renderPlotly({
    
    # Get current file
    file_number <- current_file()
    
    data <- trainingdata[[file_number]]$rawdata
    
    # Get pauses for this file
    current_pauses <- pauses()
    
    
    p <- plot_ly(
      data,
      x = ~time,
      y = ~fly,
      type = "scatter",
      mode = "lines",
      source = "pause_plot"
    ) %>%
      layout(
        title = paste0("wtb-", file_number - 1),
        xaxis = list(title = "Time (ms)"),
        yaxis = list(title = "Flight position"),
        dragmode = "zoom"
      )
    
    
    # Add shaded pause regions
    if (nrow(current_pauses) > 0) {
      
      shapes <- lapply(seq_len(nrow(current_pauses)), function(i) {
        
        list(
          type = "rect",
          x0 = current_pauses$start[i],
          x1 = current_pauses$end[i],
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          fillcolor = "red",
          opacity = 0.2,
          line = list(width = 0)
        )
      })
      
      p <- p %>%
        layout(shapes = shapes)
    }
    
    
    # JavaScript
    p <- onRender(
      p,
      "
  function(el, x) {
    
    // Only attach the keyboard listeners once
    if (!window.pauseAnnotationKeysAttached) {
      
      window.pauseAnnotationKeysAttached = true;
      
      
      // Shift pressed = selection mode
      document.addEventListener('keydown', function(e) {
        
        if (e.key === 'Shift') {
          
          var plot = document.querySelector('.js-plotly-plot');
          
          if (plot) {
            Plotly.relayout(plot, {
              dragmode: 'select',
              selectdirection: 'h'
            });
          }
        }
        
        
        // r = save and advance
        if (e.key === 'r' || e.key === 'R') {
          
          Shiny.setInputValue(
            'save_and_next',
            Math.random(),
            {priority: 'event'}
          );
        }
        if (e.key === 'u' || e.key === 'U') {
  
          Shiny.setInputValue(
            'undo_pause',
            Math.random(),
           {priority: 'event'}
          );
        }
      });
      
      
      // Shift released = zoom mode
      document.addEventListener('keyup', function(e) {
        
        if (e.key === 'Shift') {
          
          var plot = document.querySelector('.js-plotly-plot');
          
          if (plot) {
            Plotly.relayout(plot, {
              dragmode: 'zoom'
            });
          }
        }
      });
    }
    
    
    // Selection event belongs to the current plot
    el.on('plotly_selected', function(eventData) {
      
      if (eventData && eventData.range) {
        
        var xRange = eventData.range.x;
        
        Shiny.setInputValue(
          'pause_selection',
          {
            start: xRange[0],
            end: xRange[1],
            nonce: Math.random()
          },
          {priority: 'event'}
        );
      }
    });
    
  }
  "
    )
    
    p
  })
  
  
  # Add a pause
  observeEvent(input$pause_selection, {
    
    new_pause <- data.frame(
      start = input$pause_selection$start,
      end = input$pause_selection$end
    )
    
    pauses(
      rbind(
        pauses(),
        new_pause
      )
    )
  })
  
  # Undo a pause
  observeEvent(input$undo_pause, {
    
    current_pauses <- pauses()
    
    # Do nothing if there are no pauses
    if (nrow(current_pauses) == 0) {
      return()
    }
    
    # Remove the most recently added pause
    current_pauses <- current_pauses[
      -nrow(current_pauses),
      ,
      drop = FALSE
    ]
    
    pauses(current_pauses)
  })
  
  
  # Save pauses and advance to next file
  observeEvent(input$save_and_next, {
    
    file_number <- current_file()
    
    # Save current pauses
    saved <- pause_annotations()
    
    saved[[file_number]] <- pauses()
    
    pause_annotations(saved)
    
    # Permanently save to disk
    saveRDS(saved, annotation_file)
    
    
    # Print what was saved
    cat(
      "\nSaved wtb-",
      file_number - 1,
      ":\n",
      sep = ""
    )
    
    print(pauses())
    
    
    # Move to next file
    if (file_number < length(trainingdata)) {
      
      current_file(file_number + 1)
      
      # Start next file with no pauses
      pauses(
        data.frame(
          start = numeric(0),
          end = numeric(0)
        )
      )
      
    } else {
      
      cat("\nAll training files have been annotated!\n")
      
    }
  })
  
}

shinyApp(ui, server)



