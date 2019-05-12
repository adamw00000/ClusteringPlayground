main <- function(tests = -1) 
{
  baseDir <- file.path(getwd(), "benchmark")
  dataFiles <- list.files(baseDir, recursive = TRUE, pattern = ".+\\.data\\.gz")
  labelFiles <- list.files(baseDir, recursive = TRUE, pattern = ".+\\.labels0\\.gz")
  
  randIndices <- numeric(length(dataFiles))
  
  v <- if (tests == -1) 1:length(dataFiles) else tests
  
  for (i in v)#6:6)#length(dataFiles))
  {
    print("Files: ")
    print(dataFiles[i])
    print(labelFiles[i])
    
    data <- read.csv(file.path(baseDir, dataFiles[i]), sep = '', header = FALSE)
    labels <- read.csv(file.path(baseDir, labelFiles[i]), sep = '', header = FALSE)
    
    data <- as.matrix(data)
    expert_labels <- labels[[1]]
    k <- max(expert_labels)
    
    calculate(dataFiles[i], i, data, expert_labels, k)
    calculate(dataFiles[i], i, data, expert_labels, k, standardize = TRUE)
  }
}

Mnn_graph <- function(S) 
{
  G <- GenerateG(S)
  G <- ModifyG(G)
}

Laplacian_eigen <- function(G, k) 
{
  D <- GenerateD(G)
  #print("D: ")
  #print(head(D))
  L <- D - G
  #print("L: ")
  #print(head(L))
  
  GenerateE <- function(L, k)
  {
    #print("GENERATE E ENTRY")
    #print(isSymmetric(L))
    eigenVectors <- eigen(L, symmetric = TRUE)$vectors
    #print("eigenvectors: ")
    #print(head(eigenVectors))
    n <- ncol(eigenVectors)
    eigenVectors[, seq(n - 1, n - k, -1)]
  }
  GenerateE(L, k)
}

spectral_clustering <- function(X, k, M)
{
  S <- Mnn(X, M)
  #print("S: ")
  #print(head(S))
  G <- Mnn_graph(S)
  #print("G: ")
  #print(head(G))
  E <- Laplacian_eigen(G, k)
  #print("E: ")
  #print(head(E))
  
  kmeans(E, k)$cluster
}

calculate <- function(dataFile, i, data, expert_labels, k, standardize = FALSE)
{
  if (standardize)
    data <- scale(data)
  
  attr(expert_labels, "algorithm") <- "expert"
  
  my_spectral_labels_Mk <- spectral_clustering(data, k, k)
  my_spectral_labels_M2 <- spectral_clustering(data, k, 2)
  my_spectral_labels_M5 <- spectral_clustering(data, k, 5)
  my_spectral_labels_M10 <- spectral_clustering(data, k, 10)
  my_spectral_labels_M20 <- spectral_clustering(data, k, 20)
  
  kmeans_labels <- kmeans_clustering(data, k)
  
  genie_default_labels <- genie_clustering(data, k, 0.3)
  genie_0.2_labels <- genie_clustering(data, k, 0.2)
  genie_0.5_labels <- genie_clustering(data, k, 0.5)
  genie_0.8_labels <- genie_clustering(data, k, 0.8)
  
  fuzzy_default_labels <- fuzzy_clustering(data, k, 1.5)
  fuzzy_2_labels <- fuzzy_clustering(data, k, 2)
  fuzzy_5_labels <- fuzzy_clustering(data, k, 5)
  fuzzy_10_labels <- fuzzy_clustering(data, k, 10)
  
  hclust_average_labels <- hclust_average(data, k)
  hclust_centroid_labels <- hclust_centroid(data, k)
  hclust_complete_labels <- hclust_complete(data, k)
  hclust_mcquitty_labels <- hclust_mcquitty(data, k)
  hclust_median_labels <- hclust_median(data, k)
  hclust_single_labels <- hclust_single(data, k)
  hclust_wardD_labels <- hclust_wardD(data, k)
  hclust_wardD2_labels <- hclust_wardD2(data, k)
  
  labels_collection <- list("expert" = expert_labels,
                       "spectral_Mk" = my_spectral_labels_Mk, 
                       "spectral_M2" = my_spectral_labels_M2, 
                       "spectral_M5" = my_spectral_labels_M5,
                       "spectral_M10" = my_spectral_labels_M10,
                       "spectral_M20" = my_spectral_labels_M20,
                       "kmeans" = kmeans_labels,
                       "genie_0.3" = genie_default_labels, 
                       "genie_0.2" = genie_0.2_labels,
                       "genie_0.5" = genie_0.5_labels, 
                       "genie_0.8" = genie_0.8_labels,
                       "fuzzy_default" = fuzzy_default_labels, 
                       "fuzzy_2" = fuzzy_2_labels,
                       "fuzzy_5" = fuzzy_5_labels, 
                       "fuzzy_10" = fuzzy_10_labels,
                       "hclust_average" = hclust_average_labels, 
                       "hclust_centroid" = hclust_centroid_labels,
                       "hclust_complete" = hclust_complete_labels, 
                       "hclust_mcquitty" = hclust_mcquitty_labels,
                       "hclust_median" = hclust_median_labels, 
                       "hclust_single" = hclust_single_labels,
                       "hclust_wardD" = hclust_wardD_labels, 
                       "hclust_wardD2" = hclust_wardD2_labels)
  
  save_results(standardize, labels_collection, dataFile, i)
  display_results(data, dataFile, labels_collection)
}

generate_dir <- function(baseDir, dataFile, i)
{
  if (!dir.exists(baseDir))
    dir.create(baseDir, recursive = TRUE)
  
  write.csv(dataFile, file.path(baseDir, "data_file.csv"), row.names=FALSE)
}

save_results <- function(standardize, labels_collection, dataFile, i)
{
  baseDir <- if (standardize == FALSE)
    file.path(getwd(), "output", i)
  else
    file.path(getwd(), "output", paste(i, "_standardized", sep = ""))
  
  generate_dir(baseDir, dataFile, i)
  
  for (i in 1:length(labels_collection))
  {
    save_result(baseDir, labels_collection[[i]], names(labels_collection)[i])
  }
}

save_result <- function(baseDir, result, name)
{
  name <- paste(name, ".csv", sep = "")
  write.csv(result, file.path(baseDir, name), row.names=FALSE)
}


display_results <- function(data, dataFile, labels_collection)
{
  for (i in 1:length(labels_collection))
  {
    display_clusters(data, dataFile, labels_collection$expert, 
                     labels_collection[[i]], names(labels_collection)[i])
  }
}

kmeans_clustering <- function(data, k)
{
  result <- kmeans(data, k)$cluster
  attr(result, "algorithm") <- "kmeans"
  result
}

genie_clustering <- function(data, k, gini = 0.3)
{
  result <- cutree(genie::hclust2(dist(data), thresholdGini = gini), k)
  attr(result, "algorithm") <- paste("genie_", gini, sep="")
  result
}

fuzzy_clustering <- function(data, k, fuzzyfier = 1.5)
{
  result <- advclust::fuzzy.CM(data, k, m = fuzzyfier, 
                               print.result = FALSE, max.iteration = 250)@hard.label
  attr(result, "algorithm") <- paste("fuzzy_", fuzzyfier, sep="")
  result
}

hclust_complete <- function(data, k)
{
  hclust_default(data, k, "complete")
}

hclust_wardD <- function(data, k)
{
  hclust_default(data, k, "ward.D")
}

hclust_wardD2 <- function(data, k)
{
  hclust_default(data, k, "ward.D2")
}

hclust_single <- function(data, k)
{
  hclust_default(data, k, "single")
}

hclust_average <- function(data, k)
{
  hclust_default(data, k, "average")
}

hclust_mcquitty <- function(data, k)
{
  hclust_default(data, k, "mcquitty")
}

hclust_median <- function(data, k)
{
  hclust_default(data, k, "median")
}

hclust_centroid <- function(data, k)
{
  hclust_default(data, k, "centroid")
}

hclust_default <- function(data, k, m)
{
  result <- cutree(hclust(dist(data), method = m), k)
  attr(result, "algorithm") <- paste("hclust_", m, sep="")
  result
}

display_clusters <- function(data, file, labels, algorithm_labels, algorithm_name)
{
  print(algorithm_name)
  print("Rand index: ")
  randIndex <- round(mclust::adjustedRandIndex(labels, algorithm_labels), 2)
  print(randIndex)
  
  print("FM index: ")
  fmIndex <- as.numeric(round(dendextend::FM_index(labels, algorithm_labels), 2))
  print(fmIndex)
  
  plot(data[,1], data[,2], col=algorithm_labels, 
       main = paste(file, "\n", 
                    algorithm_name, 
                    "\nRand index:", randIndex, ", FM index:", fmIndex))
}