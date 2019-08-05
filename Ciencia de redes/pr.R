install.packages("lsa")
install.packages("sp")
install.packages("mapproj")
install.packages("maps")
install.packages("purrr")
install.packages("geosphere")
install.packages("rworldmap")
install.packages("ggmap")
install.packages("leaflet")
install.packages("magrittr")
library(magrittr)
library(leaflet)
library(ggmap)
library(igraph)
library(lsa)
library(sp)
library(dplyr)
library(stringr)
library(ggplot2)
library(mapproj)
library(maps)
library(purrr)
library(geosphere)
library(rworldmap)
install.packages("dplyr")
install.packages("stringr")
getwd()
f <- read.csv("datosredescoordenadas.csv", sep = ";")

datosfran <- f %>% 
  mutate(LATITUD=str_replace(LATITUD,"(\\d{2})(.)(\\d{2})(.)(\\d{2})(.)(\\w)","\\1º\\3\\'\\5\\'\\'\\7"),latnum=as.numeric(char2dms(LATITUD,chd = "º",chm = "'",chs = "''")),
         LONGITUD=str_replace(LONGITUD,"(\\d{2})(.)(\\d{2})(.)(\\d{2})(.)(\\w)","\\1º\\3\\'\\5\\'\\'\\7"),lonnum=as.numeric(char2dms(LONGITUD,chd = "º",chm = "'",chs = "''")),
         LATITUD.1=str_replace(LATITUD.1,"(\\d{2})(.)(\\d{2})(.)(\\d{2})(.)(\\w)","\\1º\\3\\'\\5\\'\\'\\7"),latnum.1=as.numeric(char2dms(LATITUD.1,chd = "º",chm = "'",chs = "''")),
         LONGITUD.1=str_replace(LONGITUD.1,"(\\d{2})(.)(\\d{2})(.)(\\d{2})(.)(\\w)","\\1º\\3\\'\\5\\'\\'\\7"),lonnum.1=as.numeric(char2dms(LONGITUD.1,chd = "º",chm = "'",chs = "''")))

str(datosfran)


for (i in 1:390) {
 inter <- as.data.frame(gcIntermediate(c(datosfran[i,]$lonnum, datosfran[i,]$latnum), 
                                        c(datosfran[i,]$lonnum.1, datosfran[i,]$latnum.1), 
                                        n=50, addStartEnd=TRUE))
  print(head(inter, n=2))
}



newmap <- getMap()
newmap <- getMap(resolution = "low")
plot(newmap, asp = 1)
plot(newmap, xlim = c(-70, 59), ylim = c(20, 69), asp = 1)
points(datosfran$lonnum, datosfran$latnum, col = "red", cex = 1)
points(datosfran$lonnum.1, datosfran$latnum.1, col = "red", cex = 1)

map("world", fill=T, col="grey8", bg="grey15")

#create basemap
map("world", fill=T, col="grey8", bg="grey15", xlim = c(-70, 59), ylim = c(20, 69))
points(datosfran$lonnum, datosfran$latnum, pch = 3, col = "chocolate1", cex = 1)

for (i in (1:dim(datosfran)[1])) { 
  inter <- gcIntermediate(c(datosfran$lonnum[1], datosfran$latnum[1]), c(datosfran$lonnum.1[i], datosfran$latnum.1[i]))
  lines(inter, lwd=0.1, col="turquoise2")    
}


#____________________________________

map <- get_map(location = 'Europe', zoom = 4)
mapPoints <- ggmap(map) +
  +   geom_point(aes(x = lonnum, y = latnum, size = sqrt(weight)), data = datosfran, alpha = .5)
?register_google




mydf <- data.frame(data = datosfran, InitialLat = datosfran$latnum,
                   InitialLong = datosfran$lonnum,
                   NewLat = datosfran$latnum.1,
                   NewLong = datosfran$lonnum.1,
                   stringsAsFactors = FALSE)

mydf2 <- data.frame(group = c("A", "B"), lat = c(mydf$InitialLat, mydf$NewLat),
                    long = c(mydf$InitialLong, mydf$NewLong))



leaflet()%>%
addTiles() %>%
addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group)
#______________------------------------
m<-leaflet(data=mydf)%>%addTiles
for (i in 1:nrow(mydf)) 
m<-m%>%addPolylines(lat=c(mydf[i,]$InitialLat,mydf[i,]$NewLat),lng=c(mydf[i,]$InitialLong,mydf[i,]$NewLong))
plot(m)
#___________________________







#___________________________________________
getwd()
g <- read.csv("grafoprueba.csv", header = FALSE)
g2 <- cbind(as.character(g[,1]), as.character(g[,2]),as.numeric(g[,3]))
str(g)
arcos <- graph.edgelist(as.matrix(g2[,1:2]))

E(arcos)$weight <- g2[,3] 
grafo <- graph_from_edgelist(as.matrix(g2[,1:2]))
print(grafo)

str(grafo)

#ploteo el grafo espantoso
plot.igraph(grafo)

#estos son los vertices
grafo$V

length(V(grafo))
#si le doy leght veo los nodos
#estos son los arcos
E(grafo)

length(E(grafo))
#excentricidad de cada uno de los nodos

eccentricity(grafo)
plot(eccentricity(grafo))
?eccentricity()
#la distancia de los nodos, el valor mas alto de las distancias 

edge.betweenness(grafo)
vertex.connectivity(grafo)
closeness(grafo)
plot(closeness(grafo))
radius(grafo, mode = "total")
distance_table(grafo, directed = TRUE )
mean_distance(grafo, directed = TRUE, unconnected = TRUE)
#divide entre la cantidad posible de caminos
degree(grafo)

#distribuciones entre los nodos
degree.distribution(grafo)
plot(degree.distribution(grafo))

plot(degree(gafo))

str(degree(gafo))
table(degree(grafo))
#54 nodos con 1 degree, si se caen dejan de transmitir los recibidos

plot(table(degree(gafo)))
summary(degree(grafo))
barplot(g$V3, xlab = "origen --> destino", ylab = "cantidad de despachos = peso" )
?barplot

#bipartito con estructura de comunidades
degree(grafo)[which(names(degree(grafo))=="UKD001")]
degree(grafo)[which(names(degree(grafo))=="BED001")]
degree(grafo)[which(names(degree(grafo))=="BED003")]


membership(grafo)
communities(grafo)

#bipartito disconexo
tkplot(grafo)
?tkplot

distance_table(grafo, directed = TRUE)
mean_distance(grafo, directed = TRUE, unconnected = TRUE)

#__________________________________________________________________
vcount(gafo)
ecount(gafo)

neighbors(gafo, "BED001", mode = 1)

is.directed(grafo)

are.connected(gafo, "BED001", "UKD001")
get.edge(gafo, id)

bipartite.mapping(gafo)


betweenness(grafo, v = V(grafo), directed = TRUE, weights = g$V3,
            nobigint = TRUE, normalized = TRUE)

plot(betweenness(grafo))

edge_betweenness(grafo, e = E(grafo), directed = TRUE,
                 weights = g$V3)

plot(edge.betweenness(grafo))
?diameter
diameter(grafo, directed = TRUE, unconnected = TRUE, weights = g$V3)
get_diameter(grafo)
a <-hub.score(grafo)
transitivity(grafo,type="global")
transitivity(grafo)
average.path.length(grafo, directed=TRUE)

#Esta función trata de encontrar subgrafos densamente conectados, también llamados comunidades en un gráfico a través de caminatas aleatorias. La idea es que los paseos aleatorios cortos tienden a permanecer en la misma comunidad
#armar las comunidades
c <- cluster_walktrap(grafo)
?modularity

modularity(c)
membership(c)
algorithm(c)

?membership

barplot.default(membership(c))
plot(c, grafo)
barplot(sizes(c))

sizes(c)
algorithm(c)
?merges
merges(c)
plot(merges(c))
crossing(c, grafo)
code_len(c)
is_hierarchical(c)

community.eb(grafo, directed = TRUE)

#Communities in igraph
coords = layout_with_fr(grafo)


plot(grafo, layout = coords, vertex.label= NA, vertex.size= 5)
c <- cluster_walktrap(grafo)
 


# modularity measure
modularity(c)
membership(c)
length(c)
sizes(c)
# crossing edges
crossing(c, gafo)

# plot communities with shaded regions
plot(c, grafo, layout=coords)
# plot communities without shaded regions
tkplot(grafo, vertex.color=membership(c), layout=coords)
plot_dendrogram(c)
?coreness
coreness(grafo, mode = "out")
coreness(grafo, mode = "in")
coreness(grafo, mode ="all")
plot(coreness(grafo, mode = "all"))
hist(coreness(grafo,mode = "all"))
#Hieararchical clustering
A = get.adjacency(gafo, sparse = FALSE)
S =cosine(A)
#Distance matrix
D = 1-S
#distance object
d= as.dist(D)
# average-linkage clustering method
cc = hclust(d, method = "single")
c4 = cluster_optimal(gafo)

boxplot(transitivity(gafo))


head(transitivity(gafo))
