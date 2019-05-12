Mnn_graph <- function(S) 
{
  G <- GenerateG(S)
  G <- ModifyG(G)
}

Laplacian_eigen <- function(G, k) 
{
  D <- GenerateD(G)
  L <- D - G
  
  GenerateE <- function(L, k)
  {
    eigenVectors <- eigen(L, symmetric = TRUE)$vectors
    n <- ncol(eigenVectors)
    eigenVectors[, seq(n - 1, n - k, -1)]
  }
  GenerateE(L, k)
}

spectral_clustering <- function(X, k, M)
{
  S <- Mnn(X, M)
  G <- Mnn_graph(S)
  E <- Laplacian_eigen(G, k)
  
  kmeans(E, k)$cluster
}
