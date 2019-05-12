n <- 5000
x <- runif(n, 0, 700)
y <- runif(n, 0, 700)

df <- data.frame(x, y)

labirynth_quadrants <- c(
  1, 1, 1, 1, 1, 1, 1,
  1, 0, 0, 0, 0, 0, 0,
  1, 0, 2, 2, 2, 2, 2,
  1, 0, 0, 0, 2, 0, 2,
  1, 1, 1, 0, 0, 3, 0,
  1, 0, 0, 0, 4, 0, 4,
  1, 0, 4, 4, 4, 4, 4
)

labirynthCluster <- function(x, y)
{
  labirynth_quadrants[((700 - y) %/% 100) * 7 + (x %/% 100) + 1]
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

n <- 250
x <- rnorm(n, 50, 10) + 100
y <- rnorm(n, 50, 10)

df2 <- data.frame(x, y)
result <- rbind(result, df2)
labels <- rbind(labels, data.frame(cluster = rep(5, times = n)))

n <- 250
x <- rnorm(n, 50, 10) + 600
y <- rnorm(n, 50, 10) + 500

df3 <- data.frame(x, y)
result <- rbind(result, df3)
labels <- rbind(labels, data.frame(cluster = rep(6, times = n)))

plot(result$x, result$y, col = labels[[1]])

baseDir <- file.path(getwd(), "my_benchmark")
if (!dir.exists(baseDir))
  dir.create(baseDir, recursive = TRUE)

write.table(result, file = file.path(baseDir, "labirynth.data.csv"), row.names = FALSE, col.names = FALSE)
write.table(labels, file = file.path(baseDir, "labirynth.labels0.csv"), row.names = FALSE, col.names = FALSE)