create_gaussian_cloud <- function(x, y, n)
{
  x <- rnorm(n, 0, 2) + x
  y <- rnorm(n, 0, 2) + y
  
  df2 <- data.frame(x, y)
}

result <- data.frame()
labels <- data.frame()

n <- 250
df1 <- create_gaussian_cloud(0, 0, n)
df2 <- create_gaussian_cloud(0, 100, n)
df3 <- create_gaussian_cloud(100, 0, n)
df4 <- create_gaussian_cloud(100, 100, n)
result <- rbind(result, df1)
result <- rbind(result, df2)
result <- rbind(result, df3)
result <- rbind(result, df4)
labels <- rbind(labels, data.frame(cluster = rep(1, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(2, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(3, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(4, times = n)))

create_line <- function(x1, y1, x2, y2, n)
{
  x <- seq(x1, x2, length.out = n) + rnorm(n, 0, 0.5)
  y <- seq(y1, y2, length.out = n) + rnorm(n, 0, 0.5)
  
  df2 <- data.frame(x, y)
}

df5 <- create_line(0, 0, 0, 100, n)
df6 <- create_line(0, 100, 100, 0, n)
df7 <- create_line(100, 0, 100, 100, n)
df8 <- create_line(100, 100, 0, 0, n)
df9 <- create_line(100, 0, 0, 0, n)
df10 <- create_line(100, 100, 0, 100, n)

result <- rbind(result, df5)
result <- rbind(result, df6)
result <- rbind(result, df7)
result <- rbind(result, df8)
result <- rbind(result, df9)
result <- rbind(result, df10)
labels <- rbind(labels, data.frame(cluster = rep(5, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(6, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(7, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(8, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(9, times = n)))
labels <- rbind(labels, data.frame(cluster = rep(10, times = n)))

plot(result$x, result$y, col = labels[[1]])

baseDir <- file.path(getwd(), "my_benchmark")
if (!dir.exists(baseDir))
  dir.create(baseDir, recursive = TRUE)

write.table(result, file = file.path(baseDir, "graph.data.csv"), row.names = FALSE, col.names = FALSE)
write.table(labels, file = file.path(baseDir, "graph.labels0.csv"), row.names = FALSE, col.names = FALSE)
