#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

#include "kdtree.h"

class MyPoint
{
  NumericVector v;
public:
  static int DIM;
  
  MyPoint(NumericVector v): v(v)
  {
    DIM = v.length();
  }
  
  double& operator[](int i)
  {
    return v[i];
  }
  
  double operator[](int i) const
  {
    return v[i];
  }
};

int MyPoint::DIM = -1;

// [[Rcpp::export]]
NumericMatrix Mnn(NumericMatrix X, int M)
{
  int n = X.nrow();
  std::vector<MyPoint> data;
  
  for (int i = 0; i < n; i++)
  {
    data.push_back(MyPoint(X.row(i)));
  }
  
  kdt::KDTree<MyPoint> kdtree(data);
  NumericMatrix S(n, M);
  
  for (int i = 0; i < n; i++)
  {
    std::vector<int> indices = kdtree.knnSearch(data[i], M + 1);
    for (int j = 0; j < M; j++)
      S(i, j) = indices[j + 1] + 1;
  }
  
  return S;
}

// [[Rcpp::export]]
NumericMatrix GenerateG(NumericMatrix S)
{
  int n = S.nrow();
  int M = S.ncol();
  
  NumericMatrix G(n, n);
  
  for (int i = 0; i < n; i++)
  {
    for (int m = 0; m < M; m++)
    {
      G(i, S(i, m) - 1) = 1;
      G(S(i, m) - 1, i) = 1;
    }
  }
  
  return G;
}

void DFS(NumericMatrix G, int v, std::vector<bool>& visited)
{
  //Rprintf("VISITING: %d", v);
  visited[v] = true;
  
  for (int u = 0; u < G.ncol(); u++)
  {
    if (G(v, u) == 1 && !visited[u])
    {
      DFS(G, u, visited);
    }
  }
  
}

// [[Rcpp::export]]
NumericMatrix ModifyG(NumericMatrix G)
{
  int n = G.nrow();
  
  std::vector<bool> visited(n);
  std::vector<int> componentVertices;
  
  for (int v = 0; v < n; v++)
  {
    if (!visited[v])
    {
      componentVertices.push_back(v);
      DFS(G, v, visited);
    }
  }
  
  //Rprintf("componentVertices.size(): %d", componentVertices.size());
  if (componentVertices.size() == 1)
    return G;
  
  for (int i = 0; i < componentVertices.size() - 1; i++)
  {
    G(componentVertices[i], componentVertices[i + 1]) = 1;
    G(componentVertices[i + 1], componentVertices[i]) = 1;
  }
  
  return G;
}

// [[Rcpp::export]]
NumericMatrix GenerateD(NumericMatrix G)
{
  int n = G.nrow();
  
  NumericMatrix D(n, n);
  for (int i = 0; i < n; i++)
  {
    D(i, i) = 0;
    for (int j = 0; j < n; j++)
    {
      if (G(i, j) == 1)
        D(i, i)++;
    }
  }
  
  return D;
}