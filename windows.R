n <- 5000
x <- runif(n, 0, 900)
y <- runif(n, 0, 900)

df <- data.frame(x, y)

labirynth_quadrants <- c(
  1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 0, 2, 2, 0, 3, 3, 0, 1,
  1, 0, 2, 2, 0, 3, 3, 0, 1,
  1, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 0, 4, 4, 0, 5, 5, 0, 1,
  1, 0, 4, 4, 0, 5, 5, 0, 1,
  1, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1
)

labirynthCluster <- function(x, y)
{
  labirynth_quadrants[((900 - y) %/% 100) * 9 + (x %/% 100) + 1]
}

result <- data.frame()
labels <- data.frame()
for (i in 1:nrow(df))
{
  cluster <- labirynthCluster(df[i,]$x, df[i,]$y)
  if (cluster != 0)
  {
    result <- rbind(result, df[i,])
    labels <- rbind(labels, data.frame(cluster = cluster))
  }
}

plot(result$x, result$y, col = labels[[1]])

baseDir <- file.path(getwd(), "my_benchmark")
if (!dir.exists(baseDir))
  dir.create(baseDir, recursive = TRUE)

write.table(result, file = file.path(baseDir, "windows.data.csv"), row.names = FALSE, col.names = FALSE)
write.table(labels, file = file.path(baseDir, "windows.labels0.csv"), row.names = FALSE, col.names = FALSE)
