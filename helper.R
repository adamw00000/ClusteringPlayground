load_results <- function(outputDir = "output")
{
  dataBasePath <- file.path(getwd(), outputDir)
  dirs <- list.dirs(dataBasePath)
  
  result_collection <- list()
  standardized_result_collection <- list()
  
  for (i in 2:length(dirs))
  {
    dirFiles <- list.files(dirs[i], pattern = ".*\\.csv")
    absoluteFiles <- file.path(dirs[i], dirFiles)
    
    dirData <- list()
    for (j in 1:length(absoluteFiles))
    {
      fileData <- read.csv(absoluteFiles[j], sep = "")
      
      name <- basename(absoluteFiles[j])
      name <- tools::file_path_sans_ext(name)
      dirData[[name]] <- fileData[[1]]
    }
    
    if (stringi::stri_detect_fixed(dirs[i], "_standardized"))
    {
      standardized_result_collection <- c(standardized_result_collection, list(dirData))
    }
    else
    {
      result_collection <- c(result_collection, list(dirData))
    }
  }
  results <- list(normal = result_collection, standardized = standardized_result_collection)
}

load_results_data <- function(results, benchmarkDir = "benchmark")
{
  load_results_data_helper <- function(resultList)
  {
    for (i in 1:length(resultList))
    {
      baseDir <- file.path(getwd(), benchmarkDir)
      
      dataFile <- resultList[[i]]$data_file[[1]]
      data <- read.csv(file.path(baseDir, dataFile), sep = '', header = FALSE)
      resultList[[i]][["data"]] <- data
    }
    resultList
  }
  results$normal <- load_results_data_helper(results$normal)
  results$standardized <- load_results_data_helper(results$standardized)
  
  results
}

calculate_indices_df <- function(results)
{
  indices <- data.frame()
  
  calculate_indices_helper <- function(resultList, indices, standardized)
  {
    for (i in 1:length(resultList))
    {
      resultSet <- resultList[[i]]
      for (j in 1:length(resultSet))
      {
        name <- names(resultSet)[j]
        if (!stringi::stri_detect_regex(name, "(data_file)|(data)|(expert)"))
        {
          randIndex <- mclust::adjustedRandIndex(resultSet$expert, resultSet[[name]])
          fmIndex <- as.numeric(dendextend::FM_index(resultSet$expert, resultSet[[name]]))
          
          indices <- rbind(indices, data.frame(Algorithm = name, Standardized = standardized,
                                               FileId = i, Index = "rand",
                                               Value = randIndex))
          indices <- rbind(indices, data.frame(Algorithm = name, Standardized = standardized,
                                               FileId = i, Index = "fm",
                                               Value = fmIndex))
        }
      }
    }
    indices
  }
  
  indices <- calculate_indices_helper(results$normal, indices, FALSE)
  indices <- calculate_indices_helper(results$standardized, indices, TRUE)
  indices
}

library(dplyr)
library(kableExtra)

aggregateIndices_df <- function(indices, operation = mean, differences = TRUE)
{
  randNormal <- indices %>%
    filter(Index == "rand" & Standardized == FALSE) %>%
    group_by(Algorithm) %>%
    summarize("RA" = round(operation(Value), 3))
  randSt <- indices %>%
    filter(Index == "rand" & Standardized == TRUE) %>%
    group_by(Algorithm) %>%
    summarize("RAtmp" = round(operation(Value), 3))
  fmNormal <- indices %>%
    filter(Index == "fm" & Standardized == FALSE) %>%
    group_by(Algorithm) %>%
    summarize("FM" = round(operation(Value), 3))
  fmSt <- indices %>%
    filter(Index == "fm" & Standardized == TRUE) %>%
    group_by(Algorithm) %>%
    summarize("FMtmp" = round(operation(Value), 3))
  
  gen_color <- function(diffValue, gap) ifelse(diffValue >= gap, "blue", 
                                               ifelse(diffValue >= gap/2, "cyan",#"cornflowerblue", 
                                                      ifelse(diffValue <= -gap, "red", 
                                                             ifelse(diffValue <= -gap/2, "orange", 
                                                                    "black"))))
  
  resultView <- randNormal %>%
    inner_join(randSt) %>%
    inner_join(fmNormal) %>%
    inner_join(fmSt) %>%
    mutate("RA - difference" = kableExtra::cell_spec(round(RAtmp - RA, 3), "latex", align = "r",
                                                     color = gen_color(round(RAtmp- RA, 3), 0.1))) %>%
    rename("RA - st. data" = RAtmp) %>%
    mutate("FM - difference" = kableExtra::cell_spec(round(FMtmp - FM, 3), "latex", align = "r",
                                                     color = gen_color(round(FMtmp - FM, 3), 0.05))) %>%
    rename("FM - st. data" = FMtmp)
  
  resultView <- if(differences) 
    resultView %>%
    select("Algorithm",
           "RA", "RA - st. data", "RA - difference",
           "FM", "FM - st. data", "FM - difference")
  else
    resultView %>%
    select("Algorithm",
           "RA", "RA - st. data", 
           "FM", "FM - st. data")
  
  resultView %>%
    arrange(-RA)
}

filter_indices <- function(indices, i)
{
  raNormal <- indices %>%
    filter(Index == "rand" & Standardized == FALSE & FileId == i) %>%
    mutate(RA = round(Value, 3)) %>%
    select(Algorithm, RA)
  
  fmNormal <- indices %>%
    filter(Index == "fm" & Standardized == FALSE & FileId == i) %>%
    mutate(FM = round(Value, 3)) %>%
    select(Algorithm, FM)
    
  raNormal %>%
    inner_join(fmNormal) %>%
    arrange(-RA)
}

boxplotIndices <- function(indices)
{
  randNormal <- indices %>%
    filter(Index == "rand" & Standardized == FALSE)
  
  draw_boxplot <- function(df, label)
  {
    bpm <- with(df, reorder(Algorithm, -Value, FUN = median))
    
    par(mar=c(7,5,1,1))
    boxplot(Value ~ bpm, data = df, las = 2, xlab = "", ylab = label)
  }
  
  draw_boxplot(randNormal, "Rand index value")
  
  randSt <- indices %>%
    filter(Index == "rand" & Standardized == TRUE)
  fmNormal <- indices %>%
    filter(Index == "fm" & Standardized == FALSE)
  
  draw_boxplot(fmNormal, "FM index value")
  
  fmSt <- indices %>%
    filter(Index == "fm" & Standardized == TRUE)
  
}


plot_results <- function(resultsCollection, plotId = -1, algorithmRegex = "all") 
{
  options(rgl.printRglwidget = TRUE)
  v <- if(plotId == -1) 1:length(resultsCollection) else plotId
  for (i in v)
  {
    resultSet <- resultsCollection[[i]]
    data <- resultSet$data
    
    create_plot <- function(data, dataFile, name, expertLabels, labels)
    {
      randIndex <- round(mclust::adjustedRandIndex(expertLabels, labels), 3)
      fmIndex <- as.numeric(round(dendextend::FM_index(expertLabels, labels), 3))
      
      #if (length(data) == 2)
      #{
        plot(data[[1]], data[[2]], col=labels,  xlab = "", ylab = "",
           main = paste(dataFile, "\n", 
                        name, 
                        "\nAR:", randIndex, ", FM:", fmIndex))
      #}
      #else if (length(data) == 3)
      #{
      #  rgl::plot3d(data[[1]], data[[2]], data[[3]], 
      #            col = labels, xlab = "", ylab = "", zlab = "")
      #  print(rgl::rglwidget())
      #}
    }
    
    create_plot(data, resultSet$data_file, "expert", resultSet$expert, resultSet$expert)
    
    for (j in 1:length(resultSet))
    {
      name <- names(resultSet)[j]
      
      if (algorithmRegex == "all")
      {
        if (!stringi::stri_detect_regex(name, "(data_file)|(data)|(expert)"))
        {
          create_plot(data, resultSet$data_file, name, resultSet$expert, resultSet[[j]])
        }
      }
      else
      {
        if (stringi::stri_detect_regex(name, algorithmRegex))
        {
          create_plot(data, resultSet$data_file, name, resultSet$expert, resultSet[[j]])
        }
      }
    }
  }
}