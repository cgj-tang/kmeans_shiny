# Loading libraries and scripts
library(shiny)
library(tidyverse)
source('euclidean.R')

# Define UI for k-means visualization example
ui <- fluidPage(
        
        # App title
        titlePanel('Visualizing K-Means Clustering in Iris Dataset'),
        
        # Sidebar layout with input and output definitions
        sidebarLayout(
                
                # Sidebar Panel for inputs
                sidebarPanel('',
                             
                             # Input: Choose number of clusters
                             sliderInput('k_cluster', label = 'Number of clusters', min = 1, max = 6, value = 3),
                             
                             # Input: Choose number of iterations
                             sliderInput('iterations', label = 'Number of iterations', min = 0, max = 10, value = 0),
                             
                             # Input: Choose first variable to cluster by
                             selectInput('var1', 'Select first variable:',
                                         choices = list('Sepal Length' = 'Sepal.Length',
                                                        'Sepal Width' = 'Sepal.Width',
                                                        'Petal Length' = 'Petal.Length',
                                                        'Petal Width' = 'Petal.Width'),
                                         selected = 'Sepal.Length'),
                             
                             # Input: Choose second variable to cluster by
                             selectInput('var2', 'Select second variable:',
                                         choices = list('Sepal Length' = 'Sepal.Length',
                                                        'Sepal Width' = 'Sepal.Width',
                                                        'Petal Length' = 'Petal.Length',
                                                        'Petal Width' = 'Petal.Width'),
                                         selected = 'Sepal.Width'),
                             
                             # Input: Choose seed for reproducibility
                             numericInput('seed', 'Set seed:', value = 1),
                             
                             # Input: Choose whether to jitter the data
                             checkboxInput('jitter', 'Jitter data', value = TRUE),
                             
                             # Input: Choose whether to draw ellipse around clusters
                             checkboxInput('ellipse', 'Draw ellipses', value = TRUE)),
                
                
                
                # Main panel for displaying outputted graph
                mainPanel(p(strong('K-means clustering'), 
                            'is one of the most popular and intuitive machine learning technique out there. The general aim is to group ',
                            em('n'), 
                            ' observations into', 
                            em('k'), 
                            ' clusters by minimizing the distance between cluster centroids and the data points that are assigned to those clusters. K-means clustering is an unsupervised algorithm, meaning that the data labels are not needed before carrying out clustering.'),
                          br(),
                          p('This Shiny app illustrates K-means clustering in the iris dataset. Select the variables you wish to cluster the dataset by, define the number of clusters and advance the ', 
                            em('Number of iterations'), 
                            ' slider to the right. Watch as the data points are iteratively re-grouped depending on their proximity to the cluster centroids. In parallel, the cluster centroids will start to converge towards their optima as their coordinates (i.e., the means for the data points assigned to each cluster) are re-calculated.'),
                          br(),
                          plotOutput('graph'),
                          br(),
                          p('Note that the app will return an error in the event that the slider is advanced while any of the ',
                            em('k'),
                            ' clusters contain zero data points. If this happens, ensure that you have selected two different variables, reduce the number of clusters, or assign a new seed.')),
        )
)


# Define server logic to carry out k-means clustering and create outputted graph
server <- function(input, output, session) {
        
        # Output: Graph of clustered data
        output$graph = renderPlot({
                set.seed(input$seed)
                df <- 
                        iris[c(input$var1, 
                               input$var2, 
                               'Species')] %>%
                        setNames(c('col1', 
                                   'col2', 
                                   'Species'))
                
                # Returns original data if no clustering is performed
                if (input$iterations == 0) {
                        df %>%
                                ggplot(aes(x = col1, 
                                           y = col2, 
                                           color = Species)) +
                                {if (input$jitter) {
                                        geom_point(size = 3,
                                                   position = position_jitter(seed = 1))}
                                        else {
                                                geom_point(size = 3)
                                        }
                                } +
                                xlab(input$var1) +
                                ylab(input$var2) +
                                theme_bw()
                }
                else {
                        df$cluster_assignment <- NA
                        
                        # Initializing empty dataframe to hold k cluster centroids
                        k_frame <- 
                                data.frame(matrix(ncol = 4,
                                                  nrow = 0, 
                                                  dimnames = list(NULL, 
                                                                  c(input$var1, 
                                                                    input$var2, 
                                                                    'k_centroid', 
                                                                    'n_id'))),
                                           stringsAsFactors = FALSE) %>%
                                setNames(c('col1', 
                                           'col2', 
                                           'k_centroid', 
                                           'n_id'))
                        
                        # Defining and iteratively refining k cluster centroids
                        for (i in 1:input$iterations) {
                                test_list <- list()
                                
                                # Initializing centroids
                                if (all(is.na(df$cluster_assignment))) {
                                        for (j in 1:input$k_cluster) {
                                                assign(x = paste('k', 
                                                                 j, 
                                                                 sep = ''),
                                                       c(df[sample(1:nrow(df), 
                                                                   size = 1), 
                                                            1],
                                                         df[sample(1:nrow(df), 
                                                                   size = 1), 
                                                            2],
                                                         j,
                                                         i))
                                                k_frame <-
                                                        k_frame %>%
                                                        add_row(col1 = get(paste('k', 
                                                                                 j, 
                                                                                 sep = ''))[1],
                                                                col2 = get(paste('k', 
                                                                                 j, 
                                                                                 sep = ''))[2],
                                                                k_centroid = get(paste('k', 
                                                                                       j, 
                                                                                       sep = ''))[3],
                                                                n_id = get(paste('k', 
                                                                                 j, 
                                                                                 sep = ''))[4])
                                                test_list[[j]] <- get(paste('k', 
                                                                            j, 
                                                                            sep = ''))[-c(3, 4)]
                                        }
                                } 
                                
                                # Refining centroid locations
                                else {
                                        k_coord <- df %>%
                                                group_by(cluster_assignment) %>%
                                                summarise(mean_length = mean(col1),
                                                          mean_width = mean(col2))
                                        for(j in 1:input$k_cluster) {
                                                assign(x = paste('k', 
                                                                 j, 
                                                                 sep = ''), 
                                                       c(k_coord[[j, 2]], 
                                                         k_coord[[j, 3]], 
                                                         j, 
                                                         i))
                                                k_frame <-
                                                        k_frame %>%
                                                        add_row(col1 = get(paste('k', 
                                                                                 j, 
                                                                                 sep = ''))[1],
                                                                col2 = get(paste('k', 
                                                                                 j, 
                                                                                 sep = ''))[2],
                                                                k_centroid = get(paste('k', 
                                                                                       j, 
                                                                                       sep = ''))[3],
                                                                n_id = get(paste('k', 
                                                                                 j, 
                                                                                 sep = ''))[4])
                                                test_list[[j]] <- get(paste('k', 
                                                                            j, 
                                                                            sep = ''))[-c(3, 4)]
                                        }
                                }
                                
                                # Assignment to nearest cluster
                                df$cluster_assignment <- factor(apply(X = df[c(1, 2)],
                                                                      MARGIN = 1,
                                                                      FUN = euclidean,
                                                                      test_list))
                                
                        }
                        
                        # Formatting clustering data in preparation for plotting
                        k_frame <-
                                k_frame %>%
                                mutate(k_centroid = as.factor(k_centroid))
                        k_frame$cluster_assignment <- NA
                        max_k <- 
                                k_frame %>%
                                slice_tail(n = input$k_cluster)
                        
                        # Plotting clustering results
                        ggplot(data = df, aes(x = col1,
                                              y = col2,
                                              color = cluster_assignment)) +
                                {if (input$jitter) {
                                        geom_point(size = 3,
                                                   position = position_jitter(seed = 1))}
                                        else {
                                                geom_point(size = 3)
                                        }
                                } +
                                {if (input$ellipse) {stat_ellipse()}} +
                                geom_point(data = k_frame,
                                           size = 5,
                                           aes(x = col1,
                                               y = col2,
                                               shape = k_centroid,
                                               group = n_id),
                                           size = 3) +
                                geom_point(data = max_k, 
                                           size = 5, 
                                           color = 'black', 
                                           aes(shape = k_centroid)) +
                                geom_path(data = k_frame,
                                          aes(x = col1,
                                              y = col2,
                                              group = k_centroid)) +
                                xlab(input$var1) +
                                ylab(input$var2) +
                                theme_bw()
                        
                }
        })
        
}

# Creating Shiny app
shinyApp(ui, server)