######################################
######### FINAL PROJECT CODE #########
######################################

# Setting up
setwd("/Volumes/JCW/Senior Year/QAC241/finalproject")
#install.packages("tuneR")
library(tuneR)
library(igraph)

# Cleaning up dataset
content <- readMidi("/Volumes/JCW/Senior Year/QAC241/finalproject/september.mid")
df <- getMidiNotes(content)
df$notename <- as.character(df$notename) 
# table(df$channel)

# Boolean to take out each instruments
piano1 <- df$channel == 0
bass_low <- df$channel == 1
ac_guitar <- df$channel == 2
melody <- df$channel == 3
melody2 <- df$channel == 4
top_bass <- df$channel == 5
keys1 <- df$channel == 6
dist_guitar <- df$channel == 7
clean_guitar <- df$channel == 8
piano2 <- df$channel == 9
brass <- df$channel == 10
trumpets <- df$channel == 11
saxophones <- df$channel == 12
trombones <- df$channel == 13
strings <- df$channel == 14

# Create a separate data frame for each instruments
df_piano1 <- df[piano1,]
df_bass_low <- df[bass_low,]
df_ac_guitar <- df[ac_guitar,]
df_melody <- df[melody,]
df_melody2 <- df[melody2,]
df_top_bass <- df[top_bass,]
df_keys1 <- df[keys1,]
df_dist_guitar <- df[dist_guitar,]
df_clean_guitar <- df[clean_guitar,]
df_piano2 <- df[piano2,]
df_brass <- df[brass,]
df_trumpets <- df[trumpets,]
df_saxophones <- df[saxophones,]
df_trombones <- df[trombones,]
df_strings <- df[strings,]

# Helper function to create the graph data frame
graphify_df <- function(dframe){
  from <- character(0)
  to <- character(0)
  curr_time <- dframe$time[1] 
  prev_notes <- character(0)
  curr_notes <- c(character(0), dframe$notename[1])
  for (i in 2:length(dframe$time)) {
    if (dframe$time[i] == curr_time) {
      curr_notes <- c(curr_notes, dframe$notename[i])
    }
    else {
      prev_notes = curr_notes
      curr_note <- dframe$notename[i]
      curr_notes = c(character(0), curr_note)
      for (j in 1:length(prev_notes)) {
        from <- c(from, prev_notes[j])
        to <- c(to, curr_note)
      }
      curr_time = dframe$time[i]
    }
  }
  df <- data.frame(from = from, to = to, stringsAsFactors = F)
  df
}

# Helper function to get a graph and the cluster based on igraph package
make_graph_and_cluster <- function(dframe){
  graph <- graph_from_data_frame(d=graphify_df(dframe), directed=T)
  cluster <- cluster_edge_betweenness(graph)
  gc <- list(graph, cluster)
  gc
}

# Helper function to plot the graph with the corresponding membership and communities.
# The graph is exported as a graphml file to the directory.
# This function also prints out the size of the communities formed.
plot_graph <- function(name, gc){
  graph <- gc[[1]]
  cluster <- gc[[2]]
  cat(length(communities(cluster)))
  E(graph)$weight <- 1
  graph_simp <- simplify(graph, edge.attr.comb=list(weight="sum"), remove.loops = F)
  write_graph(graph_simp, name, "graphml")
  plot(graph_simp, edge.width = abs(log1p(E(graph_simp)$weight)), 
       vertex.color = membership(cluster), mark.groups = communities(cluster))
}

# Plotting the graph for all the different instruments.
plot_graph("piano1.graphml", make_graph_and_cluster(df_piano1))
plot_graph("bass_low.graphml", make_graph_and_cluster(df_bass_low))
plot_graph("ac_guitar.graphml", make_graph_and_cluster(df_ac_guitar))
plot_graph("melody.graphml", make_graph_and_cluster(df_melody))
plot_graph("melody2.graphml", make_graph_and_cluster(df_melody2))
plot_graph("top_bass.graphml", make_graph_and_cluster(df_top_bass))
plot_graph("df_keys1.graphml", make_graph_and_cluster(df_keys1))
plot_graph("dist_guitar.graphml", make_graph_and_cluster(df_dist_guitar))
plot_graph("clean_guitar.graphml", make_graph_and_cluster(df_clean_guitar))
plot_graph("piano2.graphml", make_graph_and_cluster(df_piano2))
plot_graph("brass.graphml", make_graph_and_cluster(df_brass))
plot_graph("trumpets.graphml", make_graph_and_cluster(df_trumpets))
plot_graph("saxophones.graphml", make_graph_and_cluster(df_saxophones))
plot_graph("trombones.graphml", make_graph_and_cluster(df_trombones))
plot_graph("strings.graphml", make_graph_and_cluster(df_strings))