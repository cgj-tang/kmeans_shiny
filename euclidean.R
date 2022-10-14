#####################################
###CALCULATING EUCLIDEAN DISTANCES###
#####################################

#Calculates euclidean distances between a list of centroids for k clusters and
#a data frame containing two columns for X and Y. Returns the index for the k 
#cluster closest to each datapoint (i.e., row). Meant to be wrapped into an
#lapply function.

euclidean <- function(dataset, k_list) {
        k_list %>%
                lapply(function(x) (x - dataset)^2) %>%
                lapply(function(x) sqrt(sum(x))) %>%
                which.min()
}