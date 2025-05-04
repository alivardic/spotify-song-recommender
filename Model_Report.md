
``` r
library(readr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ purrr     1.0.2
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dbplyr)
```

    ## 
    ## Attaching package: 'dbplyr'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     ident, sql

``` r
df_combined <- read_csv("df_combined.csv")
```

    ## New names:
    ## Rows: 47850 Columns: 16
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (2): artists, track_name dbl (14): ...1, duration_ms, popularity, danceability,
    ## energy, key, loudness...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
# extract song names
song_name <- df_combined[,(2:3)]
# Set all as numeric
songs <- df_combined[c(-1:-3)]
songs[] <- lapply(songs, function(x) as.numeric(as.character(x)))
```

``` r
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 4.4.3

    ## corrplot 0.95 loaded

``` r
corrmatrix <- cor(songs)
corrplot(corrmatrix, method = 'number')
```

![](Models_files/figure-gfm/CORRELATION-1.png)<!-- -->

``` r
which(df_combined$track_name == "Another Love")
```

    ## [1] 34

## MODEL 1: SIMPLE KNN ANALYSIS

``` r
if (!require(FNN)) {
  install.packages("FNN") 
  library(FNN)
}
```

    ## Loading required package: FNN

    ## Warning: package 'FNN' was built under R version 4.4.3

``` r
library(FNN)

# 1. Scale data
songs_scaled <- scale(songs)

# 2. Pick point
query_point <- songs_scaled[3176, , drop = FALSE]

# 3. Find 5 nearest neighbors to point (Report will include the song+5 nearest neighbors so k = 6)
knn_result <- get.knnx(data = songs_scaled, query = query_point, k = 6)

# 4. Inspect the neighbor indices and distances
neighbor_indices <- knn_result$nn.index[]
neighbor_distances <- knn_result$nn.dist[]

# 5. Show actual neighbor rows
neighbors_data <- song_name[neighbor_indices[1, ], ]

# Print everything
cat("Nearest neighbors (indices):\n")
```

    ## Nearest neighbors (indices):

``` r
print(neighbor_indices)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6]
    ## [1,] 3176 2164 3741 5843 3873 2482

``` r
cat("\nEuclidian Distances:\n")
```

    ## 
    ## Euclidian Distances:

``` r
print(neighbor_distances)
```

    ##      [,1]      [,2]      [,3]     [,4]     [,5]     [,6]
    ## [1,]    0 0.9522638 0.9629199 1.137047 1.144283 1.207896

``` r
cat("\nNeighbor data:\n")
```

    ## 
    ## Neighbor data:

``` r
print(neighbors_data)
```

    ## # A tibble: 6 × 2
    ##   artists          track_name                             
    ##   <chr>            <chr>                                  
    ## 1 Hozier           Almost (Sweet Music)                   
    ## 2 Taylor Swift     We Are Never Ever Getting Back Together
    ## 3 Melanie Martinez Mad Hatter                             
    ## 4 Kanye West       Good Morning                           
    ## 5 Cole Swindell    Single Saturday Night                  
    ## 6 BLACKPINK        Ready For Love

## K CLUSTERING DETERMINATION

``` r
library(FNN)
library(cluster)
library(tidyverse)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
set.seed(42)

# 1. Scale data
songs_scaled <- scale(songs)

sampled_df <- songs %>% slice_sample(prop = 0.10)
sample <- scale(sampled_df)

# Two Method K- Cluster Verification 
############################################################
## OPTIMAL K ANALYSIS VIA DERIVIATION AND ELBOW DETECTION ##
############################################################

# Compute WSS valus
wss <- map_dbl(1:15,  function(k){
  model <- kmeans(x = sample, centers = k, nstart = 50, iter.max = 100)
  model$tot.withinss
})

# Emulate Deriviation Logic with a Descrete function using diff()
# Compute first and second derivatives
first_deriv <- diff(wss)
second_deriv <- diff(first_deriv)

# Find where the second derivative is highest
elbow_k <- which.max(abs(second_deriv)) + 1
cat("Optimal k (via second derivative):", elbow_k, "\n")
```

    ## Optimal k (via second derivative): 2

``` r
# Elbow Plot
plot(1:15, wss, type = "b", pch = 19, 
     main = "WSS vs. K with Elbow with Automatic Detection via Deriviation",
     xlab = "Number of Clusters (k)", ylab = "WSS")
abline(v = elbow_k, col = "red", lty = 2)
text(elbow_k, wss[elbow_k], labels = paste0("k=", elbow_k), pos = 4, col = "red")
```

![](Models_files/figure-gfm/K-CLUSTERING-1.png)<!-- -->

``` r
##############################################
## OPTIMAL K ANALYSIS VIA SILHOUETTE METHOD ##
##############################################

silhouette_scores <- c()

for (k in 2:12) {
  km <- kmeans(sample, centers = k, nstart = 50)
  sil <- silhouette(km$cluster, dist(sample))  # FIXED HERE
  avg_sil_width <- mean(sil[, 3])   # average silhouette width
  silhouette_scores <- c(silhouette_scores, avg_sil_width)
}

silhouette_k <- which.max(silhouette_scores) + 1 
cat("Optimal k (via silhoutte analysis):", silhouette_k, "\n")
```

    ## Optimal k (via silhoutte analysis): 2

``` r
plot(2:12, silhouette_scores, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Average silhouette width",
     main = "Choosing k with Silhouette Scores")
points(silhouette_k, silhouette_scores[silhouette_k - 1], col = "red", pch = 19, cex = 1.5)
text(silhouette_k, silhouette_scores[silhouette_k - 1], labels = paste("Best k =", silhouette_k),
     pos = 3, col = "red", cex = 0.9)
```

![](Models_files/figure-gfm/K-CLUSTERING-2.png)<!-- -->

``` r
########################################
## ELBOW METHOD == SILHOUETTE METHOD? ##
########################################
if (elbow_k == silhouette_k) {
  k = silhouette_k
  cat("Elbow method and Silhouette method agree: optimal k =", k, ".\n")
} else {
  k <- silhouette_k
  cat("Warning: Elbow and Silhouette suggest different k values.\n")
}
```

    ## Elbow method and Silhouette method agree: optimal k = 2 .

``` r
#############################################
## K MEANS CLUSTERING WITH OPTIMAL K VALUE ##
#############################################

kmeans_result <- kmeans(songs_scaled, centers = k)

# Reshape Data to long formt via reshape2
centers_df <- as.data.frame(kmeans_result$centers)
centers_df$cluster <- rownames(centers_df)
centers_long <- pivot_longer(centers_df, -cluster, names_to = "feature", values_to = "value")

# Visualize Cluster Difference Analysis
ggplot(centers_long, aes(x = feature, y = value, group = cluster, color = cluster)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  coord_flip() +
  labs(title = "Cluster Centroid Feature Values", y = "Scaled Value")
```

![](Models_files/figure-gfm/K-CLUSTERING-3.png)<!-- -->

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
# Convert to data frame and add cluster
plot_df <- as.data.frame(songs_scaled) %>%
  mutate(cluster = factor(kmeans_result$cluster))

# Select the features you want to include
metrics <- c("popularity", "danceability", "energy", "loudness", "valence")

# Generate the pairplot matrix
ggpairs(
  plot_df,
  columns = which(colnames(plot_df) %in% metrics),
  aes(color = cluster, alpha = 0.6),
  upper = list(continuous = wrap("points", size = 0.7)),
  lower = list(continuous = wrap("points", size = 0.7)),
  diag = list(continuous = wrap("densityDiag"))
) +
  theme_minimal()
```

![](Models_files/figure-gfm/CLUSTERING%20PLOT-1.png)<!-- -->

## MODEL 2: K-Clustering (K determined via Elbow and Silhouette) then cluster focused KNN

``` r
##################################################
## K NEAREST NEIGHBOR WITHIN DETERMINED CLUSTER ##
##################################################

# Pick a Point
query_index <- 3176

# Identify query's cluster and all songs in that cluster
query_cluster <- kmeans_result$cluster[query_index]
cluster_indices <- which(kmeans_result$cluster == query_cluster)

# Subset cluster data
cluster_songs <- songs_scaled[cluster_indices, , drop = FALSE]
cluster_song_names <- song_name[cluster_indices, ]

# Get the query song within the cluster
relative_query_index <- match(query_index, cluster_indices)
query_cluster_point <- cluster_songs[relative_query_index, , drop = FALSE]

# 5 nearest neighbors (Report will include the song+5 nearest neighbors so k = 6)
knn_result <- get.knnx(data = cluster_songs, query = query_cluster_point, k = 6)

# Neighbor Results 
neighbor_indices <- knn_result$nn.index[1, ]
neighbor_distances <- knn_result$nn.dist[1, ]
neighbors_data <- cluster_song_names[neighbor_indices, ]


####################
## RESULTS REPORT ##
####################

cat("Nearest neighbors (indices):\n")
```

    ## Nearest neighbors (indices):

``` r
print(neighbor_indices)
```

    ## [1] 2850 1962 3352 5165 3469 2236

``` r
cat("\nEuclidian Distances:\n")
```

    ## 
    ## Euclidian Distances:

``` r
print(neighbor_distances)
```

    ## [1] 0.0000000 0.9522638 0.9629199 1.1370473 1.1442833 1.2078958

``` r
cat("\nNeighbor data:\n")
```

    ## 
    ## Neighbor data:

``` r
print(neighbors_data)
```

    ## # A tibble: 6 × 2
    ##   artists          track_name                             
    ##   <chr>            <chr>                                  
    ## 1 Hozier           Almost (Sweet Music)                   
    ## 2 Taylor Swift     We Are Never Ever Getting Back Together
    ## 3 Melanie Martinez Mad Hatter                             
    ## 4 Kanye West       Good Morning                           
    ## 5 Cole Swindell    Single Saturday Night                  
    ## 6 BLACKPINK        Ready For Love

## MODEL 3: K-Clustering (K determined via Elbow and Silhouette), PCA Dimensionality reduction, then cluster focused KNN with PCA variables

``` r
##################################
## PCA DIMENSIONALITY REDUCTION ##
##################################
pca_result <- prcomp(songs_scaled, scale. = FALSE)  # Already scaled, so scale.=FALSE

explained_variance <- (pca_result$sdev)^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)

# While I could not find a 100% accepted rule of thumb for acceptable variability thresholds,
# Shmueli et al. (2017) appears to refer back to roughly 90% variance often in Ch. 4 (Dimensionality   
# Reduction, page 108), So I am going to use 90% as the variance threshold
plot(cumulative_variance, type = "b",
     xlab = "Number of Principal Components",
     ylab = "Cumulative Explained Variance",
     main = "Explained Variance by Principal Components")
abline(h = 0.90, col = "red", lty = 2)  
```

![](Models_files/figure-gfm/PCA-1.png)<!-- -->

``` r
num_pcs <- which(cumulative_variance >= 0.90)[1]  
songs_pca <- pca_result$x[, 1:num_pcs]  # Top 5 principal components
```

``` r
#####################################################################
## K NEAREST NEIGHBOR WITHIN DETERMINED CLUSTER WITH PCA REDUCTION ##
#####################################################################

# Pick a Point
query_index <- 3176

# Identify query's cluster and all songs in that cluster
query_cluster <- kmeans_result$cluster[query_index]
cluster_indices <- which(kmeans_result$cluster == query_cluster)

# Subset cluster data
cluster_songs <- songs_pca[cluster_indices, , drop = FALSE]
cluster_song_names <- song_name[cluster_indices, ]

# Get the query song within the cluster
relative_query_index <- match(query_index, cluster_indices)
query_cluster_point <- cluster_songs[relative_query_index, , drop = FALSE]

# 5 nearest neighbors (Report will include the song+5 nearest neighbors so k = 6)
knn_result <- get.knnx(data = cluster_songs, query = query_cluster_point, k = 6)

# Neighbor Results 
neighbor_indices <- knn_result$nn.index[1, ]
neighbor_distances <- knn_result$nn.dist[1, ]
neighbors_data <- cluster_song_names[neighbor_indices, ]


####################
## RESULTS REPORT ##
####################

cat("Nearest neighbors (indices):\n")
```

    ## Nearest neighbors (indices):

``` r
print(neighbor_indices)
```

    ## [1] 2850 3478 3352 4831 3654 2493

``` r
cat("\nEuclidian Distances:\n")
```

    ## 
    ## Euclidian Distances:

``` r
print(neighbor_distances)
```

    ## [1] 0.0000000 0.6339347 0.7510672 0.7799376 0.8463660 0.8491251

``` r
cat("\nNeighbor data:\n")
```

    ## 
    ## Neighbor data:

``` r
print(neighbors_data)
```

    ## # A tibble: 6 × 2
    ##   artists                   track_name                                    
    ##   <chr>                     <chr>                                         
    ## 1 Hozier                    Almost (Sweet Music)                          
    ## 2 Anuel AA, Mariah Angeliq  Bandido                                       
    ## 3 Melanie Martinez          Mad Hatter                                    
    ## 4 Los Amigos Invisibles     La Que Me Gusta                               
    ## 5 2Pac;Talent               Changes                                       
    ## 6 Sia, Diplo, Labrinth, LSD Thunderclouds (feat. Sia, Diplo, and Labrinth)

## SPOTIFY API LOOKUP

``` r
library(stringr)

lookup_list <- neighbors_data %>%
  mutate(combined = str_c(artists, track_name, sep = " "))

liked <- lookup_list[1,]
lookup_list <- lookup_list[-1,]
```

``` r
access_token <- get_spotify_access_token()

lookup_list$spotify_url <- NA

for (i in seq_along(lookup_list$combined)) {
  query <- lookup_list$combined[i]

  result <- tryCatch({
    search_spotify(query, type = "track", limit = 1, market = "US")
  }, error = function(e) NULL)

  if (!is.null(result) && nrow(result) > 0) {
    lookup_list$spotify_url[i] <- result$external_urls.spotify[1]
  }
}

recommendations <- paste0(
  'Try "', lookup_list$track_name, '" by ', lookup_list$artists,
  ' [Spotify link](', lookup_list$spotify_url, ')', "\n \n \n"
)

name <- ""  
name <- paste0(name, liked[1,2])
artist <- ""  
artist <- paste0(artist, liked[1,1])

cat("Since you liked", name, "by", artist, "\n\n",
  recommendations)
```

Since you liked Almost (Sweet Music) by Hozier

Try “Bandido” by Anuel AA, Mariah Angeliq [Spotify
link](https://open.spotify.com/track/5poUJOhLyalGkUedn3ggeX)

Try “Mad Hatter” by Melanie Martinez [Spotify
link](https://open.spotify.com/track/5gWtkdgdyt5bZt9i6n3Kqd)

Try “La Que Me Gusta” by Los Amigos Invisibles [Spotify
link](https://open.spotify.com/track/1q3AYHVLtrry7SDdRHoYnx)

Try “Changes” by 2Pac;Talent [Spotify
link](https://open.spotify.com/track/1ofhfV90EnYhEr7Un2fWiv)

Try “Thunderclouds (feat. Sia, Diplo, and Labrinth)” by Sia, Diplo,
Labrinth, LSD [Spotify
link](https://open.spotify.com/track/4lJNen4SMTIJMahALc3DcB)
