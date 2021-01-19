## ###########################################################
##
## appletrees/app.R
##
## Exploring block design
##
## @author: Craig Lazarski & Jeffery Painter
## @modified: 2021-Jan-02
##
## ###########################################################

library(shiny)
library(ggplot2)
library(dplyr)


blockingOptions = c("Completely Random" = 1,
                    "Horizontal" = 2,
                    "Vertical" = 3)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://www.w3schools.com/w3css/4/w3.css")
  ),
  
  title = "Block Design Explorer",
  h1("Block Design Explorer"),
  
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(
    type = "tabs",
    
    ## ###########################################################
    ## Panel 1
    ## ###########################################################
    tabPanel(
      "App",
      
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "panel1_input_number_trials",
            "Number of Experiments",
            min = 1,
            max = 300,
            value = 1
          ),
          
          # input
          selectInput(
            inputId = "panel1_input_block",
            label = "Experiment Design",
            blockingOptions,
            selected = 1
          ),
          
          actionButton(inputId = "panel1_btn_graph", label =
                         "Grow Trees"),
          
          checkboxInput(inputId = "panel1_chkbox_block_effect", "Add Block Effect", FALSE),
          
          hr(),
          
          # Explore individual plots
          p(
            "The slider below will let you explore each trial
            from lowest to highest mean difference. Hint, you
            can click on the slider, then use the arrow keys
            to explore."
          ),
          
          sliderInput(
            inputId = "panel1_select_trial",
            "Explore Observations",
            min = 1,
            max = 1,
            value = 1
          ),
          
          hr(),
          
          # Tree block
          htmlOutput(outputId = "panel1_html_layout")
          
          
          ),
        
        mainPanel(
          #includeHTML("www/img_header.html"),
          
          # output
          plotOutput(outputId = "panel1_graph_mean_diff"),
          hr(),
          tableOutput(outputId = "panel1_graph_summary_stats"),
          
          hr(),
          includeHTML("www/task_01.html")
          
        )
        ),
      
    ),
    tabPanel("About",
             includeHTML("www/about.html"))
    
)
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Initialize the data frame with no data
  mydata <- reactiveValues(
    # Each panel needs to track it's own data for trials and mean diffs
    p1_trials = NULL,
    p1_mean_differences = NULL,
    
    # This is the current trial being displayed on our graph
    p1_current_trial = NULL,
    
    # Block effect is ENABLED by default
    p1_block_effect = TRUE,
    
    # This will allow us to turn off the explore mean diff the first time
    p1_first_time = TRUE,
    p1_exploring = FALSE,
    
    # Apple tree settings: variety A
    a_mean = 47,
    a_sigma = 3,
    
    # Apple tree settings: variety B
    b_mean = 53,
    b_sigma = 3
  )
  
  ## ###########################################################
  ## These are the functions to generate new samples for each
  ## of the block designs.
  ## ###########################################################
  
  ## ###########################################################
  ## Set a complete randomized distribution. This will update
  ## two reactive variables at the end which contain
  ## the raw data from all the trial runs plus the mean
  ## difference values of all the trial runs in two data
  ## frames.
  ##
  ##  Updates:
  ##     mydata$trials            (raw data)
  ##     mydata$mean_differences  (mean diff of each trial)
  ##
  ## ###########################################################
  setCompleteRandomDesign <- function()
  {
    # How many trials are we going to execute
    num_trials = input$panel1_input_number_trials
    
    # Initialize the data frames
    trials_df = data.frame()
    means_df  = data.frame()
    for (trial in 1:num_trials)
    {
      # Get the yield of 4 trees from each variety
      a_samples = round(rnorm(
        mean = mydata$a_mean,
        sd = mydata$a_sigma,
        n = 4
      ))
      b_samples = round(rnorm(
        mean = mydata$b_mean,
        sd = mydata$b_sigma,
        n = 4
      ))
      samples = c(a_samples , b_samples)
      
      # shuffle all possible positions
      positions = sample(1:8)
      
      observed_values = c()
      for (idx in 1:8)
      {
        sample_value    = samples[idx]
        sample_position = positions[idx]
        
        # Are we using the block effect?
        if (mydata$p1_block_effect == TRUE)
        {
          # add the effect due to proximity to the forest
          if (sample_position %% 2) {
            new_value = sample_value - 10
            observed_values = c(observed_values, new_value)
          } else {
            new_value = sample_value + 10
            observed_values = c(observed_values, new_value)
          }
        } else {
          # no block effect applied
          observed_values = c(observed_values, sample_value)
        }
      }
      
      # This is the column value indicating our trial run
      trials = rep(trial, 8)
      
      # This is the column value indicating tree variety
      trees  = c(rep("A", 4), rep("B", 4))
      
      # Final matrix for this round combining all elements together
      tree_matrix = matrix(
        data = c(trials, trees, positions, observed_values),
        ncol = 4,
        byrow = F
      )
      tm_df = as.data.frame(tree_matrix)
      colnames(tm_df) <- c("trial", "variety", "position", "yield")
      
      # add this trial to the total data frame
      trials_df = rbind(trials_df, tm_df)
      
      # compute the mean difference in the two varieties
      new_mean_a = mean (observed_values[1:4])
      new_mean_b = mean (observed_values[5:8])
      mean_diff = new_mean_b - new_mean_a
      
      # Store the mean differences in the data frame
      new_row = data.frame("trial" = trial, "mean_diff" = mean_diff)
      means_df = rbind(means_df, new_row)
    }
    
    # The raw data for all trials run
    mydata$p1_trials = trials_df
    
    # The mean differences of all trials run
    mydata$p1_mean_differences = means_df %>% arrange(mean_diff)
    
    # sort the mean differences from low to high
    min_diff_trial_id = means_df[1, ]$trial
    mydata$p1_current_trial = trials_df %>% filter(trial == min_diff_trial_id)
    
    return()
  }
  
  
  
  ## ###########################################################
  ## Generate a horizontal block design
  ## return the mean difference
  ## ###########################################################
  setHorizontalBlockDesign <- function()
  {
    # How many trials are we going to execute
    num_trials = input$panel1_input_number_trials
    
    # Initialize the data frames
    trials_df = data.frame()
    means_df  = data.frame()
    for (trial in 1:num_trials)
    {
      final_obs = c()
      final_pos = c()
      for (block in 1:2)
      {
        # Generate samples for the block
        a_samples = round(rnorm(
          mean = mydata$a_mean,
          sd = mydata$a_sigma,
          n = 2
        ))
        b_samples = round(rnorm(
          mean = mydata$b_mean,
          sd = mydata$b_sigma,
          n = 2
        ))
        samples = c(a_samples , b_samples)
        
        # Shuffle positions for single block
        positions = sample(1:4)
        
        observed_values = c()
        for (idx in 1:4)
        {
          sample_value    = samples[idx]
          sample_position = positions[idx]
          # Is block effect enabled?
          if (mydata$p1_block_effect == TRUE)
          {
            if (sample_position %% 2) {
              new_value = sample_value - 10
              observed_values = c(observed_values, new_value)
            } else {
              new_value = sample_value + 10
              observed_values = c(observed_values, new_value)
            }
          } else {
            observed_values = c(observed_values, sample_value)
          }
        }
        
        
        final_obs = c(final_obs, observed_values)
        final_pos = c(final_pos, positions)
      }
      
      # move these to the second block
      final_pos[5:8] = final_pos[5:8] + 4
      
      # put back in the {a} {b} order
      observed_values = final_obs[c(1, 2, 5, 6, 3, 4, 7, 8)]
      positions       = final_pos[c(1, 2, 5, 6, 3, 4, 7, 8)]
      
      # This is the column value indicating our trial run
      trials = rep(trial, 8)
      
      # This is the column value indicating tree variety
      trees  = c(rep("A", 4), rep("B", 4))
      
      # Final matrix for this round combining all elements together
      tree_matrix = matrix(
        data = c(trials, trees, positions, observed_values),
        ncol = 4,
        byrow = F
      )
      tm_df = as.data.frame(tree_matrix)
      colnames(tm_df) <- c("trial", "variety", "position", "yield")
      
      # add this trial to the total data frame
      trials_df = rbind(trials_df, tm_df)
      
      # compute the mean difference in the two varieties
      new_mean_a = mean (observed_values[1:4])
      new_mean_b = mean (observed_values[5:8])
      mean_diff = new_mean_b - new_mean_a
      
      # Store the mean differences in the data frame
      new_row = data.frame("trial" = trial, "mean_diff" = mean_diff)
      means_df = rbind(means_df, new_row)
    }
    
    # The raw data for all trials run
    mydata$p1_trials = trials_df

    # The mean differences of all trials run
    mydata$p1_mean_differences = means_df %>% arrange(mean_diff)
    
    # sort the mean differences from low to high
    min_diff_trial_id = means_df[1, ]$trial
    mydata$p1_current_trial = trials_df %>% filter(trial == min_diff_trial_id)
    return()
  }
  
  
  ## ###########################################################
  ## Generate a vertical block design
  ## return the mean difference
  ## ###########################################################
  setVerticalBlockDesign <- function()
  {
    # How many trials are we going to execute
    num_trials = input$panel1_input_number_trials
    
    # Initialize the data frames
    trials_df = data.frame()
    means_df  = data.frame()
    for (trial in 1:num_trials)
    {
      final_obs = c()
      final_pos = c()
      for (block in 1:2)
      {
        # Generate samples for the block
        a_samples = round(rnorm(
          mean = mydata$a_mean,
          sd = mydata$a_sigma,
          n = 2
        ))
        b_samples = round(rnorm(
          mean = mydata$b_mean,
          sd = mydata$b_sigma,
          n = 2
        ))
        samples = c(a_samples , b_samples)
        
        # Vertical block positions
        positions = sample(c(1, 3, 5, 7))
        
        observed_values = c()
        # Is block effect enabled?
        if (mydata$p1_block_effect == TRUE)
        {
          if (block == 1)
          {
            observed_values = samples - 10
          } else {
            observed_values = samples + 10
          }
        } else {
          # no block effect
          observed_values = samples
        }
        
        final_obs = c(final_obs, observed_values)
        final_pos = c(final_pos, positions)
      }
      
      # update block (2)
      final_pos[5:8] = final_pos[5:8] + 1
      
      # put back in the {a} {b} order
      observed_values = final_obs[c(1, 2, 5, 6, 3, 4, 7, 8)]
      positions       = final_pos[c(1, 2, 5, 6, 3, 4, 7, 8)]
      
      # This is the column value indicating our trial run
      trials = rep(trial, 8)
      
      # This is the column value indicating tree variety
      trees  = c(rep("A", 4), rep("B", 4))
      
      # Final matrix for this round combining all elements together
      tree_matrix = matrix(
        data = c(trials, trees, positions, observed_values),
        ncol = 4,
        byrow = F
      )
      tm_df = as.data.frame(tree_matrix)
      colnames(tm_df) <- c("trial", "variety", "position", "yield")
      
      # add this trial to the total data frame
      trials_df = rbind(trials_df, tm_df)
      
      # compute the mean difference in the two varieties
      new_mean_a = mean (observed_values[1:4])
      new_mean_b = mean (observed_values[5:8])
      mean_diff = new_mean_b - new_mean_a
      
      # Store the mean differences in the data frame
      new_row = data.frame("trial" = trial, "mean_diff" = mean_diff)
      means_df = rbind(means_df, new_row)
    }
    
    # The raw data for all trials run
    mydata$p1_trials = trials_df
    
    # The mean differences of all trials run
    mydata$p1_mean_differences = means_df %>% arrange(mean_diff)
    
    # sort the mean differences from low to high
    min_diff_trial_id = means_df[1, ]$trial
    mydata$p1_current_trial = trials_df %>% filter(trial == min_diff_trial_id)
    return()
  }
  
  ## ###########################################################
  ## Starting code for tree HTML
  ## ###########################################################
  getTreeStartHTML <- function()
  {
    output = (
      '
      <div class="w3-container">
      <div class="w3-container w3-cell">
      <img src="forest.png" style="height: 100px;">
      </div>
      <div class="w3-container w3-cell">
      <table class="w3-table-all w3-border w3-center">
      '
    )
    return(output)
  }
  
  getTreeEndHTML <- function()
  {
    output = (
      '
      </div>
      <hr/>
      <p>
      <table class="w3-table w3-border w3-center w3-xsmall">
      <tr>
      <td> Key </td>
      <td class="w3-blue"> Variety A </td>
      <td class="w3-yellow"> Variety B </td>
      </tr>
      </table>
      
      </div>
      
      '
    )
    return(output)
  }
  
  # Compute bin width based on number of samples
  getBinWidth <- function(total_samples)
  {
    bw = 1
    if (total_samples < 100) {
      bw = .85
    } else if (total_samples >= 100 && total_samples < 200) {
      bw = .80
    } else if (total_samples >= 200 && total_samples < 300) {
      bw = .75
    } else {
      bw = .7
    }
    return(bw)
  }
  
  ## ###########################################################
  ## Generate the HTML table that shows us the tree production
  ## ###########################################################
  plantTrees <- function(tree_data_frame)
  {
    output = ""
    current_position = 1
    for (row in 1:4)
    {
      # start of row
      output = paste(output, "<tr>")
      
      # column one in row
      current_tree = tree_data_frame %>% filter(position == current_position)
      variety = current_tree$variety
      yield   = current_tree$yield
      
      if (variety == "A")
      {
        output = paste(output, '<td class="w3-blue">', yield, '</td>')
      } else {
        output = paste(output, '<td class="w3-yellow">', yield, '</td>')
      }
      current_position = current_position + 1
      
      # column 2 in row
      # column one in row
      current_tree = tree_data_frame %>% filter(position == current_position)
      if (!is.null(current_tree))
      {
        variety = current_tree$variety
        yield   = current_tree$yield
        
        if (variety == "A")
        {
          output = paste(output, '<td class="w3-blue">', yield, '</td>')
        } else {
          output = paste(output, '<td class="w3-yellow">', yield, '</td>')
        }
        current_position = current_position + 1
      } else {
        output = paste(output, '<td>', 0, '</td>')
      }
      
      # end of row
      output = paste(output, "</tr>")
    }
    output = paste(output, "</table>")
    return(output)
  }
  
  ## ###########################################################
  ## Generate the HTML table that shows us the tree production
  ## ###########################################################
  plantHorizontalTrees <- function(tree_data_frame)
  {
    output = ""
    current_position = 1
    for (row in 1:4)
    {
      # start of row
      output = paste(output, "<tr>")
      
      # column one in row
      current_tree = tree_data_frame %>% filter(position == current_position)
      if (!is.null(current_tree))
      {
        variety = current_tree$variety
        yield   = current_tree$yield
        if (variety == "A")
        {
          output = paste(output, '<td class="w3-blue">', yield, '</td>')
        } else {
          output = paste(output, '<td class="w3-yellow">', yield, '</td>')
        }
      } else {
        output = paste(output, '<td> NA </td>')
      }
      current_position = current_position + 1
      
      # column 2 in row
      current_tree = tree_data_frame %>% filter(position == current_position)
      variety = current_tree$variety
      yield   = current_tree$yield
      if (variety == "A")
      {
        output = paste(output, '<td class="w3-blue">', yield, '</td>')
      } else {
        output = paste(output, '<td class="w3-yellow">', yield, '</td>')
      }
      current_position = current_position + 1
      
      
      # end of row
      output = paste(output, "</tr>")
      
      # horizontal blocking
      if (row == 2)
      {
        output = paste(output,
                       '<tr class="w3-white"><td colspan="2">&nbsp;</td></tr>')
      }
      
    }
    output = paste(output, "</table>")
    return(output)
  }
  
  
  ## ###########################################################
  ## Generate the HTML table that shows us the tree production
  ## ###########################################################
  plantVerticalTrees <- function(tree_data_frame)
  {
    output = ""
    current_position = 1
    for (row in 1:4)
    {
      # start of row
      output = paste(output, "<tr>")
      
      # column one in row
      current_tree = tree_data_frame %>% filter(position == current_position)
      variety = current_tree$variety
      yield   = current_tree$yield
      
      if (variety == "A")
      {
        output = paste(output, '<td class="w3-blue">', yield, '</td>')
      } else {
        output = paste(output, '<td class="w3-yellow">', yield, '</td>')
      }
      current_position = current_position + 1
      
      # vertical blocking
      output = paste(output, '<td class="w3-white">&nbsp;</td>')
      
      # column 2 in row
      current_tree = tree_data_frame %>% filter(position == current_position)
      variety = current_tree$variety
      yield   = current_tree$yield
      
      if (variety == "A")
      {
        output = paste(output, '<td class="w3-blue">', yield, '</td>')
      } else {
        output = paste(output, '<td class="w3-yellow">', yield, '</td>')
      }
      current_position = current_position + 1
      
      # end of row
      output = paste(output, "</tr>")
      
    }
    output = paste(output, "</table>")
    return(output)
  }
  
  ######################################################
  # Panel 1
  ######################################################
  observeEvent(input$panel1_btn_graph, {
    # Reset exploring mode to false
    mydata$p1_exploring  = FALSE
    mydata$p1_first_time = TRUE
    
    # Setup new complete random design
    block_design = input$panel1_input_block
    if (block_design == 1)
    {
      setCompleteRandomDesign()
    } else if (block_design == 2)
    {
      setHorizontalBlockDesign()
    } else if (block_design == 3)
    {
      setVerticalBlockDesign()
    }
    
    # How many trials were run?
    trials = isolate(input$panel1_input_number_trials)
    init_spot = 1
    if (trials > 3)
    {
      init_spot = trials / 2
    }
    
    # Update the explorer slider based on the total number of trials
    updateSliderInput(
      session,
      "panel1_select_trial",
      value = init_spot,
      min = 1,
      max = trials,
      step = 1
    )
    
  })
  
  # Test to see if we are enabling or disabling the block effect
  observeEvent(input$panel1_chkbox_block_effect, {
    # update the reactive variable
    mydata$p1_block_effect = input$panel1_chkbox_block_effect
  })
  
  # This listens to the slider and finds
  # the current selected event.  If it changes,
  # we will reset the current selected trial which
  # is a reactive variable
  observeEvent(input$panel1_select_trial, {
    all_trials = mydata$p1_trials
    mean_diffs = mydata$p1_mean_differences
    if (!is.null(all_trials))
    {
      selected_trial = input$panel1_select_trial
      #print(paste("Position marker: ", selected_trial))
      
      selected_trial_id = mean_diffs[selected_trial, ]$trial
      #print(paste("Mapped trial: ", selected_trial_id))
      
      mdiff = mean_diffs[selected_trial, ]$mean_diff
      #print(paste("Mean diff: ", mdiff))
      
      # Update the reactive variable
      trial_data = all_trials %>% filter(trial == selected_trial_id)
      mydata$p1_current_trial = trial_data
      
      # Only update if not first time
      if (mydata$p1_first_time == FALSE)
      {
        mydata$p1_exploring = TRUE
      } else {
        mydata$p1_first_time = FALSE
      }
    }
  })
  
  ## This is where the current selected trial of trees is printed
  output$panel1_html_layout <- renderPrint({
    # This is the reactive value. When it is updated, we
    # will invalidate this render output and generate new
    # outputs
    current_trial = mydata$p1_current_trial
    if (!is.null(current_trial))
    {
      # now we have the yield and position of all the trees
      output = getTreeStartHTML()
      
      # what kind of block design are we using?
      block_design = isolate(input$panel1_input_block)
      if (block_design == 1) {
        #print("Plant trees!")
        output = paste(output, plantTrees(current_trial))
      } else if (block_design == 2)
      {
        #print("Plant hor trees!")
        output = paste(output, plantHorizontalTrees(current_trial))
      } else if (block_design == 3) {
        #print("Plant ver trees!")
        output = paste(output, plantVerticalTrees(current_trial))
      }
      
      output = paste(output, getTreeEndHTML())
      
      # print the table
      HTML(output)
    }
  })
  
  ##
  ## Plot the mean differences
  ##
  output$panel1_graph_summary_stats <- renderTable({
    # This is the reactive value. When it is updated, we
    # will invalidate this render output and generate new
    # outputs
    df = mydata$p1_mean_differences
    if (!is.null(df))
    {
      ms = mean(df$mean_diff)
      msd = sd(df$mean_diff)
      summary_stat_df = as.data.frame(cbind(ms, msd))
      colnames(summary_stat_df) = c("Mean of Samples", "Std Dev of Samples")
      return(summary_stat_df)
    }
  })
  
  ## Plot the mean differences
  output$panel1_graph_mean_diff <- renderPlot({
    # This is the reactive value. When it is updated, we
    # will invalidate this render output and generate new
    # outputs
    df = mydata$p1_mean_differences
    if (!is.null(df))
    {
      selected_trial = input$panel1_select_trial
      selected_mean_diff = df[selected_trial, ]$mean_diff
      selected_mean_label = paste("Mean diff = ", round(selected_mean_diff, 3))
      
      # compute bin width to use
      bw = getBinWidth(nrow(df))
      actual_mean_diff = mydata$b_mean - mydata$a_mean
      
      myplot = ggplot(df, aes(mean_diff)) +
        theme_classic() +
        theme(
          plot.title = element_text(
            lineheight = 0.8,
            size = 20,
            face = "bold"
          ),
          
          axis.title.y = element_text(size=14,face="bold"),
          axis.text.y = element_blank(),
          
          axis.title.x = element_text(size=14,face="bold"),
          axis.text.x = element_text(size=14)          
          
        ) +
        coord_cartesian(xlim = c(-20, 35), ylim = c(-.01, 0.6)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_dotplot(binwidth = bw) +
        # geom_vline(xintercept = actual_mean_diff, col = "blue") +
        # annotate(
        #   geom = "text",
        #   x = 11.5,
        #   y = 0.55,
        #   label = "Actual mean\ndiff = 6",
        #   size = 7,
        #   color = "blue"
        # ) +
        labs(x = "Mean Difference in Apple Production", y = "Frequency", size=14)
      
      # Only add the individual trial mean diff if exploring
      if (mydata$p1_exploring == TRUE)
      {
        myplot = myplot + geom_vline(xintercept = selected_mean_diff, col = "red") +
          annotate(
            geom = "text",
            x = (selected_mean_diff + 3),
            y = 0.4,
            label = selected_mean_label,
            size = 5,
            color = "red"
          )
      }
      
      return(myplot)
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
