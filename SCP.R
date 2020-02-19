# This is a free implementation of the article mentioned below, the Python code in the article's source code was used to obtain a dataset in csv
# Article: https://towardsdatascience.com/discovering-similarities-across-my-spotify-music-using-data-clustering-and-visualization-52b58e6f547b
# Source Code: https://github.com/juandes/audio-features-unsupervised-learning

# Other resources:
# https://uc-r.github.io/kmeans_clustering
# https://www.datacamp.com/community/tutorials/k-means-clustering-r

library(VIM)
library(ggmap)
library(factoextra)
library(gridExtra)
library(cluster)

# Load the dataset
dataset <- read.csv("audiofeatures.csv")

# Check if there are missing values
aggr(dataset)

# Take a look at the dataset 
head(dataset, n = 20)

    # energy liveness   tempo speechiness acousticness instrumentalness time_signature danceability key duration_ms loudness valence mode           type                                  uri
# 1  0.24900   0.0647  95.675      0.0331       0.9420            0.954              3       0.1780   9      294013  -13.710  0.0386    1 audio_features spotify:track:6M0AiiCV8CX2R0XfKVlchk
# 2  0.10300   0.1070  75.637      0.0439       0.7290            0.915              3       0.1310  10      245507  -22.175  0.0390    1 audio_features spotify:track:5qWFRThiYrdNTFAkvGnlwc
# 3  0.13700   0.2360  79.260      0.0369       0.9590            0.378              4       0.2180   9      116747  -21.585  0.0645    0 audio_features spotify:track:3Bl9iYwrc32eJztVnuljrg
# 4  0.00852   0.0847 138.061      0.0447       0.9800            0.875              4       0.1490   4      242587  -33.468  0.0375    0 audio_features spotify:track:0qCdUhKihO2VN094TYc6Ca
# 5  0.12300   0.0915 129.947      0.0394       0.9440            0.918              3       0.3900   3      158347  -17.706  0.0579    0 audio_features spotify:track:1xRCmlU2GyzGem2vw4glxK
# 6  0.07060   0.1160 176.509      0.0371       0.9470            0.892              4       0.0844   1      213200  -17.245  0.0456    1 audio_features spotify:track:3EoD6CaNuwZ5ZPW43Lm6v7
# 7  0.22300   0.1130  91.569      0.0354       0.9300            0.936              4       0.1060   6      316040  -15.826  0.0496    1 audio_features spotify:track:6zGAgfm8fbkUq51sBg194z
# 8  0.02070   0.0766 108.950      0.0494       0.9950            0.904              4       0.4790   5      282463  -27.397  0.1460    1 audio_features spotify:track:0abOYbhSVScPloC510i5hc
# 9  0.08150   0.1150  74.870      0.0440       0.8420            0.905              3       0.0729   4      149573  -24.860  0.0379    1 audio_features spotify:track:50U6JBQx1Lt7QVVNRO2qCU
# 10 0.23200   0.0834  79.040      0.0344       0.0484            0.709              3       0.2040   9      131587  -14.112  0.0304    0 audio_features spotify:track:3hXqZ6EQaT9zhIanKPoVsY
# 11 0.06320   0.1250  52.333      0.0448       0.8860            0.478              5       0.0592   9      284627  -24.375  0.0341    1 audio_features spotify:track:2eelyXVZq2dLCH0z5VQHsH
# 12 0.14600   0.0882 167.018      0.0391       0.9560            0.978              3       0.0992   0      224351  -19.513  0.0444    1 audio_features spotify:track:6GjbZu7yLBBGhUB9SSvqMb
# 13 0.03970   0.0872  65.532      0.0427       0.9450            0.873              4       0.0653   9      251844  -26.310  0.0366    1 audio_features spotify:track:6ZsmdKXB6wpeIuKa2lCZG4
# 14 0.00712   0.0755  76.401      0.0370       0.9780            0.907              4       0.2070   7      186735  -31.917  0.0558    0 audio_features spotify:track:1Hc9rfFRsUE8nyKDnyXg7f
# 15 0.12300   0.3140  73.572      0.0455       0.9620            0.770              4       0.0946   5      354893  -16.349  0.0375    1 audio_features spotify:track:1HWTRHG5Ve9EDcKroJmYq7
# 16 0.01160   0.1040  77.840      0.0337       0.9830            0.963              4       0.3040   7      245992  -33.190  0.0364    0 audio_features spotify:track:3srcCsCjdbsXQKoWLCXFe5
# 17 0.03890   0.0666 113.500      0.0492       0.9770            0.797              4       0.2020   2      474507  -18.911  0.0361    1 audio_features spotify:track:3x8JiN6t8qRzKjt3pGgsM1
# 18 0.27600   0.1100  68.619      0.0346       0.3350            0.908              4       0.1900   1      176587  -14.830  0.0575    1 audio_features spotify:track:4aW4Dpz3gpb619hBXDrFUa
# 19 0.22200   0.1070 110.877      0.0356       0.5280            0.964              4       0.1690   7      247400  -15.954  0.0397    0 audio_features spotify:track:4VaOecssBTF02dO0n2u8BS
# 20 0.08790   0.0869 126.622      0.0381       0.1550            0.698              4       0.2210   7      275547  -16.996  0.0401    1 audio_features spotify:track:6ZFbXIJkuI1dVNWvzJzown

# Choose features
d <- dataset[,0:6]
head(d)

   # energy liveness   tempo speechiness acousticness instrumentalness
# 1 0.24900   0.0647  95.675      0.0331        0.942            0.954
# 2 0.10300   0.1070  75.637      0.0439        0.729            0.915
# 3 0.13700   0.2360  79.260      0.0369        0.959            0.378
# 4 0.00852   0.0847 138.061      0.0447        0.980            0.875
# 5 0.12300   0.0915 129.947      0.0394        0.944            0.918
# 6 0.07060   0.1160 176.509      0.0371        0.947            0.892

d2 <- dataset[,0:6]
d2$danceability <- dataset$danceability
d2$loudness <- dataset$loudness
d2$valence <- dataset$valence

head(d2)

     # energy liveness    tempo speechiness acousticness instrumentalness danceability loudness valence
# 1 -13.24440 -13.4287  82.1816    -13.4603     -12.5514         -12.5394       0.1780  -13.710  0.0386
# 2 -21.90200 -21.8980  53.6320    -21.9611     -21.2760         -21.0900       0.1310  -22.175  0.0390
# 3 -21.16550 -21.0665  57.9575    -21.2656     -20.3435         -20.9245       0.2180  -21.585  0.0645
# 4 -33.27298 -33.1968 104.7795    -33.2368     -32.3015         -32.4065       0.1490  -33.468  0.0375
# 5 -17.13510 -17.1666 112.6889    -17.2187     -16.3141         -16.3401       0.3900  -17.706  0.0579
# 6 -17.04440 -16.9990 159.3940    -17.0779     -16.1680         -16.2230       0.0844  -17.245  0.0456

set.seed(8616)

# Get the Euclidean distance between samples and visualaize them
euc_dist <- get_dist(d)
fviz_dist(euc_dist)

# Try kmeans with different Ks and visualize the result for the first two features
c1 <- kmeans(d, 1)
c1 <- kmeans(d, 1, nstart = 42)
c2 <- kmeans(d, 2, nstart = 42)
c3 <- kmeans(d, 3, nstart = 42)
c4 <- kmeans(d, 4, nstart = 42)
c5 <- kmeans(d, 5, nstart = 42)

gr1 <- fviz_cluster(c1, data = d) + ggtitle("k = 1")
gr2 <- fviz_cluster(c2, data = d) + ggtitle("k = 2")
gr3 <- fviz_cluster(c3, data = d) + ggtitle("k = 3")
gr4 <- fviz_cluster(c4, data = d) + ggtitle("k = 4")
gr5 <- fviz_cluster(c5, data = d) + ggtitle("k = 5")

grid.arrange(gr1,gr2,gr3,gr4,gr5)

# Try kmedoids with different Ks and visualize the result for the first two features
km1 <- pam(d, 1)
km2 <- pam(d, 2)
km3 <- pam(d, 3)
km4 <- pam(d, 4)
km5 <- pam(d,5)

head(km1)

# $medoids
     # energy liveness  tempo speechiness acousticness instrumentalness
# [1,] 0.0126   0.0861 83.066      0.0424        0.877            0.954

# $id.med
# [1] 161

# $clustering
  # [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [105] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [209] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [313] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [417] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [521] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# $objective
   # build     swap 
# 23.02943 23.02943 

# $isolation
 # 1 
# no 
# Levels: no L L*

# $clusinfo
     # size max_diss  av_diss diameter separation
# [1,]  574 131.0511 23.02943  214.121          0

gr1 <- fviz_cluster(km1, data = d) + ggtitle("k = 1")
gr2 <- fviz_cluster(km2, data = d) + ggtitle("k = 2")
gr3 <- fviz_cluster(km3, data = d) + ggtitle("k = 3")
gr4 <- fviz_cluster(km4, data = d) + ggtitle("k = 4")
gr5 <- fviz_cluster(km5, data = d) + ggtitle("k = 5")

grid.arrange(gr1,gr2,gr3,gr4,gr5)

# Select the best K using Silhouette coefficeint and compare the result se second dataset which has an extra feature
fviz_nbclust(d, pam, method = "silhouette")
fviz_nbclust(d, kmeans, method = "silhouette")
fviz_nbclust(d2, pam, method = "silhouette")
fviz_nbclust(d2, kmeans, method = "silhouette")

# K = 2 is optimal which means that I did a good job at curating my playlist :)

# Future Work:
# Kmeans works better with normalized data, it would be good to compare the performance on normalized and unnormalized data