#Mapping bilingual conversations with network science
#Code edited by M.Tiv
#Feb 23, 2020

#Download convenience package first time running script
#devtools::install_github("jasongullifer/convenience")

library(tidyverse)
library(data.table)
library(igraph)
library(rgexf)
library(RColorBrewer)
library(ggraph)
library(Rmisc)
library(psych)
library(gridExtra)

df <- read.csv("bi.conv.networks.csv", stringsAsFactors = F)

# Function to make adjacency matrix (general)
make_adj<-function(data) {
  adj<-outer(as.numeric(data$value),as.numeric(data$value))
  colnames(adj) <- data$topic
  adj <- data.frame(adj)
  adj$topic <- data$topic
  adj <- adj %>% select(topic, everything())
  return(adj)
}


# Part1 - 5 Context Networks ----------------------------------------------
# AIM: compare fluent1/fluent2 language use within each context -> nodes are topics and edge weight is number of languages used
# First, create network for each context, then merge visualizations, and lastly compute descriptive stats and run tests

# HOME CONTEXT
sns.home <- df %>% 
  dplyr::select(subject, contains("home"))

sns.home.subj = sns.home %>% dplyr::select(subject) #subject individual differences will be filled in here.

sns.home <- sns.home %>% 
  gather(measure, value, 2:107) %>% 
  separate(col = measure, into = c("context", "null", "topic", "language"), sep = "_") %>% select(-null) %>% 
  filter(topic != "other") 


sns.home$value[is.na(sns.home$value)] <- 0
sns.home$value[sns.home$value>1] <- 1

# Make adjacency matrix for each subj
adj.home <- sns.home %>% group_by(subject, language) %>% do(make_adj(.)) %>% ungroup()

# Calculate total number of language selected by each subj. We will use this to compute a mean weight
sum.home.languages <- adj.home %>% 
  gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, language) %>% dplyr::summarise(n=any(val==1)) %>% dplyr::summarise(n_languages=sum(n))

# Sum up the adjacency values for each subject for each topic-topic pairing,
# merge with the number of contexts containing responses, and compute mean
sum.adj.home <- adj.home %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, topic,topic2) %>% 
  dplyr::summarise(sum=sum(val)) %>% dplyr::left_join(sum.home.languages) %>% dplyr::mutate(avg=sum/n_languages) %>% ungroup()

#Remove people who have NA's
sum.adj.home = sum.adj.home %>% filter(!is.na(avg))

#Set up network measures
sns.home.subj$home.networkSize <- NA
sns.home.subj$home.meanstrength <- NA
sns.home.subj$home.density <- NA

for (s in unique(sum.adj.home$subject)){
  print(s) 
  g <- graph_from_data_frame(sum.adj.home[sum.adj.home$subject==s & sum.adj.home$sum > 0,] %>% select(-subject), directed=F) #The reason we restricted sum > 1 is bc we want to remove the topics that were not discussed in any sphere (i.e., remove excess nodes)
  g <- simplify(g, remove.loops = T, remove.multiple = F) # remove self-loops in the graph
  
  #Network Measures
  sns.home.subj[sns.home.subj$subject==s, "home.networkSize"] <- vcount(g) #Number of nodes
  sns.home.subj[sns.home.subj$subject==s, "home.meanstrength"] <- mean(strength(g, vids = V(g), loops = F, weights = E(g)$sum)) #Avg strength of edges
  sns.home.subj[sns.home.subj$subject==s, "home.density"] <- mean(edge_density(simplify(g), loops = F)) #network density
}

# compute mean language network across all subjs
home.mean <- sum.adj.home %>% 
  dplyr::select(subject, topic, topic2, sum) %>% 
  dplyr::group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) 

# visualisation of average home network (across all subjs)
home.mean$mean_bin = cut(home.mean$weight, 
                         breaks = c(0, 0.5, 1, 1.5, 2), 
                         labels = 1:4)

g.home <- graph_from_data_frame(home.mean, directed = F) %>%  simplify()
home.mean.simplify <- as.data.frame(get.edgelist(g.home)) %>% 
  dplyr::left_join(home.mean, by = c("V1" = "topic", "V2" = "topic2")) %>% 
  dplyr::select(-mean_bin)

#small worldness measure
qgraph::smallworldness(g.home, B=1000)

set.seed(1515)
grid::grid.newpage()
png("home.plot.png")
dev.control(displaylist="enable")
plot(g.home, vertex.size=5,edge.color = c("pink", "gold", "aquamarine", "lightsteelblue3") [home.mean$mean_bin], vertex.label = NA,
     vertex.color = "white", layout=layout_with_drl(g.work), edge.width=1.3, edge.curved=TRUE)
home.plot = recordPlot()
invisible(dev.off())

# FAMILY CONTEXT
sns.fam <- df %>% 
  dplyr::select(subject, contains("sphere.family_topics"))

sns.fam.subj = sns.fam %>% dplyr::select(subject) #subject individual differences will be filled in here.

sns.fam <- sns.fam %>% 
  gather(measure, value, 2:107) %>% 
  separate(col = measure, into = c("context", "null", "topic", "language"), sep = "_") %>% select(-null) %>% 
  filter(topic != "other") 

sns.fam$value[is.na(sns.fam$value)] <- 0
sns.fam$value[sns.fam$value>1] <- 1

# Make adjacency matrix for each subj
adj.fam <- sns.fam %>% dplyr::group_by(subject, language) %>% do(make_adj(.)) %>% ungroup()

# Calculate total number of language selected by each subj. We will use this to compute a mean weight
sum.fam.languages <- adj.fam %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, language) %>% 
  dplyr::summarise(n = any(val == 1)) %>% dplyr::summarise(n_languages=sum(n))

# Sum up the adjacency values for each subject for each topic-topic pairing,
# merge with the number of contexts containing responses, and compute mean
sum.adj.fam <- adj.fam %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, topic,topic2) %>% 
  dplyr::summarise(sum=sum(val)) %>% dplyr::left_join(sum.fam.languages) %>% dplyr::mutate(avg=sum/n_languages) %>% ungroup()

#Remove people who have NA's
sum.adj.fam = sum.adj.fam %>% filter(!is.na(avg))

#Set up network measures
sns.fam.subj$fam.networkSize <- NA
sns.fam.subj$fam.meanstrength <- NA
sns.fam.subj$fam.density <- NA

for (s in unique(sum.adj.fam$subject)){
  print(s) #current sub
  g <- graph_from_data_frame(sum.adj.fam[sum.adj.fam$subject==s & sum.adj.fam$sum > 0,] %>% select(-subject), directed=F) #The reason we restricted sum > 1 is bc we want to remove the topics that were not discussed in any sphere (i.e., remove excess nodes)
  g <- simplify(g, remove.loops = T, remove.multiple = F) # remove self-loops in the graph
 
  #Network Measures
  sns.fam.subj[sns.fam.subj$subject==s, "fam.networkSize"] <- vcount(g) #Number of nodes
  sns.fam.subj[sns.fam.subj$subject==s, "fam.meanstrength"] <- mean(strength(g, vids = V(g), loops = F, weights = E(g)$sum)) #Avg strength of edges
  sns.fam.subj[sns.fam.subj$subject==s, "fam.density"] <- mean(edge_density(simplify(g), loops =F)) #network density
}

# compute mean language network across all subjs
fam.mean <- sum.adj.fam %>% 
  dplyr::select(subject, topic, topic2, sum) %>% 
  dplyr::group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) 

# visualisation of average family network (across all subjs)
fam.mean$mean_bin = cut(fam.mean$weight, 
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = 1:4)

g.fam <- graph_from_data_frame(fam.mean, directed = F) %>% simplify()
fam.mean.simplify <- as.data.frame(get.edgelist(g.fam)) %>% 
  dplyr::left_join(fam.mean, by = c("V1" = "topic", "V2" = "topic2")) %>% 
  dplyr::select(-mean_bin)

#small worldness measure
qgraph::smallworldness(g.fam, B=1000)

set.seed(1515)
grid::grid.newpage()
png("fam.plot.png")
dev.control(displaylist="enable")
plot(g.fam, vertex.size=5,edge.color =c("pink", "gold", "aquamarine", "lightsteelblue3")  [fam.mean$mean_bin], vertex.label = NA,
     vertex.color = "white", layout=layout_with_drl(g.work), edge.width=1.3, edge.curved=TRUE)
fam.plot = recordPlot()
invisible(dev.off())

# SCHOOL CONTEXT
sns.school <- df %>% 
  dplyr::select(subject, contains("sphere.school_topics"))

sns.school.subj = sns.school %>% dplyr::select(subject) #subject individual differences will be filled in here

sns.school <- sns.school %>% 
  gather(measure, value, 2:107) %>% 
  separate(col = measure, into = c("context", "null", "topic", "language"), sep = "_") %>% select(-null) %>% 
  filter(topic != "other") 

sns.school$value[is.na(sns.school$value)] <- 0
sns.school$value[sns.school$value>1] <- 1

# Make adjacency matrix for each subj
adj.school <- sns.school %>% dplyr::group_by(subject, language) %>% do(make_adj(.)) %>% ungroup()

# Calculate total number of languages selected by each subj. We will use this to compute a mean weight
sum.school.languages <- adj.school %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, language) %>% 
  dplyr::summarise(n = any(val == 1)) %>% dplyr::summarise(n_languages=sum(n))

# Sum up the adjacency values for each subject for each topic-topic pairing,
# merge with the number of contexts containing responses, and compute mean
sum.adj.school <- adj.school %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, topic,topic2) %>% 
  dplyr::summarise(sum=sum(val)) %>% dplyr::left_join(sum.school.languages) %>% dplyr::mutate(avg=sum/n_languages) %>% ungroup()

#Remove people who have NA's
sum.adj.school = sum.adj.school %>% filter(!is.na(avg))

#Set up network measures
sns.school.subj$school.networkSize <- NA
sns.school.subj$school.meanstrength <- NA
sns.school.subj$school.density <- NA

for (s in unique(sum.adj.school$subject)){
  print(s) #current sub
  g <- graph_from_data_frame(sum.adj.school[sum.adj.school$subject==s & sum.adj.school$sum > 0,] %>% select(-subject), directed=F) #The reason we restricted sum > 1 is bc we want to remove the topics that were not discussed in any sphere (i.e., remove excess nodes)
  g <- simplify(g, remove.loops = T, remove.multiple = F) # remove self-loops in the graph

  #Network Measures
  sns.school.subj[sns.school.subj$subject==s, "school.networkSize"] <- vcount(g) #Number of nodes
  sns.school.subj[sns.school.subj$subject==s, "school.meanstrength"] <- mean(strength(g, vids = V(g), loops = F, weights = E(g)$sum)) #Avg strength of edges
  sns.school.subj[sns.school.subj$subject==s, "school.density"] <- mean(edge_density(simplify(g), loops = F)) #network density
}

# compute mean language network across all subjs
school.mean <- sum.adj.school %>% 
  dplyr::select(subject, topic, topic2, sum) %>% 
  dplyr::group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) 

# visualisation of average family network (across all subjs)
school.mean$mean_bin = cut(school.mean$weight, 
                           breaks = c(0, 0.5, 1, 1.5, 2), 
                           labels = 1:4)

g.school <- graph_from_data_frame(school.mean, directed = F) %>%  simplify()
school.mean.simplify <- as.data.frame(get.edgelist(g.school)) %>% 
  dplyr::left_join(school.mean, by = c("V1" = "topic", "V2" = "topic2")) %>% 
  dplyr::select(-mean_bin)

#small worldness measure
qgraph::smallworldness(g.school, B=1000)

set.seed(1515)
grid::grid.newpage()
png("school.plot.png")
dev.control(displaylist="enable")
plot(g.school, vertex.size=5,edge.color = c("pink", "gold", "aquamarine", "lightsteelblue3")[school.mean$mean_bin], vertex.label = NA,
     vertex.color = "white", layout=layout_with_drl(g.work), edge.width=1.3, edge.curved=TRUE)
school.plot = recordPlot()
invisible(dev.off())

# SOCIAL CONTEXT
sns.social <- df %>% 
  dplyr::select(subject, contains("sphere.social")) 

sns.social.subj = sns.social %>% dplyr::select(subject) #subject individual differences will be filled in here.

sns.social <- sns.social %>% 
  gather(measure, value, 2:107) %>% 
  separate(col = measure, into = c("context", "null", "topic", "language"), sep = "_") %>% select(-null) %>% 
  filter(topic != "other") 

sns.social$value[is.na(sns.social$value)] <- 0
sns.social$value[sns.social$value>1] <- 1

# Make adjacency matrix for each subj
adj.social <- sns.social %>% dplyr::group_by(subject, language) %>% do(make_adj(.)) %>% ungroup()

# Calculate total number of language selected by each subj. We will use this to compute a mean weight
sum.social.languages <- adj.social %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, language) %>% 
  dplyr::summarise(n = any(val == 1)) %>% dplyr::summarise(n_languages=sum(n))

# Sum up the adjacency values for each subject for each topic-topic pairing,
# merge with the number of contexts containing responses, and compute mean
sum.adj.social <- adj.social %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, topic,topic2) %>% 
  dplyr::summarise(sum=sum(val)) %>% dplyr::left_join(sum.social.languages) %>% dplyr::mutate(avg=sum/n_languages) %>% ungroup()

#Remove people who have NA's
sum.adj.social = sum.adj.social %>% filter(!is.na(avg))

#Set up network measures
sns.social.subj$social.networkSize <- NA
sns.social.subj$social.meanstrength <- NA
sns.social.subj$social.density <- NA

for (s in unique(sum.adj.social$subject)){
  print(s) #current sub
  g <- graph_from_data_frame(sum.adj.social[sum.adj.social$subject==s & sum.adj.social$sum > 0,] %>% select(-subject), directed=F) #The reason we restricted sum > 1 is bc we want to remove the topics that were not discussed in any sphere (i.e., remove excess nodes)
  g <- simplify(g, remove.loops = T, remove.multiple = F) # remove self-loops in the graph

  #Network Measures
  sns.social.subj[sns.social.subj$subject==s, "social.networkSize"] <- vcount(g) #Number of nodes
  sns.social.subj[sns.social.subj$subject==s, "social.meanstrength"] <- mean(strength(g, vids = V(g), loops = F, weights = E(g)$sum)) #Avg strength of edges
  sns.social.subj[sns.social.subj$subject==s, "social.density"] <- mean(edge_density(simplify(g), loops =F)) #network density
}

# compute mean language network across all subjs
social.mean <- sum.adj.social %>% 
  dplyr::select(subject, topic, topic2, sum) %>% 
  dplyr::group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) 


# visualisation of average social network (across all subjs)
social.mean$mean_bin = cut(social.mean$weight, 
                           breaks = c(0, 0.5, 1, 1.5, 2), 
                           labels = 1:4)

g.social <- graph_from_data_frame(social.mean, directed = F) %>% simplify()
social.mean.simplify <- as.data.frame(get.edgelist(g.social)) %>% 
  dplyr::left_join(social.mean, by = c("V1" = "topic", "V2" = "topic2")) %>% 
  dplyr::select(-mean_bin)

#small worldness measure
qgraph::smallworldness(g.social, B=1000)

set.seed(1515)
grid::grid.newpage()
png("social.plot.png")
dev.control(displaylist="enable")
plot(g.social, vertex.size=5,edge.color = c("pink", "gold", "aquamarine", "lightsteelblue3")[social.mean$mean_bin], vertex.label = NA,
     vertex.color = "white", layout=layout_with_drl(g.work), edge.width=1.3, edge.curved=TRUE)
social.plot = recordPlot()
invisible(dev.off())

# WORK CONTEXT
sns.work <- df %>% 
  dplyr::select(subject, contains("sphere.work"))

sns.work.subj =  sns.work %>% dplyr::select(subject) #subject individual differences will be filled in here

sns.work <- sns.work %>% 
  gather(measure, value, 2:107) %>% 
  separate(col = measure, into = c("context", "null", "topic", "language"), sep = "_") %>% select(-null) %>% 
  filter(topic != "other") 

sns.work$value[is.na(sns.work$value)] <- 0
sns.work$value[sns.work$value>1] <- 1

# Make adjacency matrix for each subj
adj.work <- sns.work %>% dplyr::group_by(subject, language) %>% do(make_adj(.)) %>% ungroup()

# Calculate total number of language selected by each subj. We will use this to compute a mean weight
sum.work.languages <- adj.work %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, language) %>% 
  dplyr::summarise(n = any(val == 1)) %>% dplyr::summarise(n_languages=sum(n))

# Sum up the adjacency values for each subject for each topic-topic pairing,
# merge with the number of contexts containing responses, and compute mean
sum.adj.work <- adj.work %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, topic,topic2) %>% 
  dplyr::summarise(sum=sum(val)) %>% dplyr::left_join(sum.work.languages) %>% dplyr::mutate(avg=sum/n_languages) %>% ungroup()

#Remove people who have NA's
sum.adj.work = sum.adj.work %>% filter(!is.na(avg))

#Set up network measures
sns.work.subj$work.networkSize <- NA
sns.work.subj$work.meanstrength <- NA
sns.work.subj$work.density <- NA

for (s in unique(sum.adj.work$subject)){
  print(s) #current sub
  g <- graph_from_data_frame(sum.adj.work[sum.adj.work$subject==s & sum.adj.work$sum > 0,] %>% select(-subject), directed=F) #The reason we restricted sum > 1 is bc we want to remove the topics that were not discussed in any sphere (i.e., remove excess nodes)
  g <- simplify(g, remove.loops = T, remove.multiple = F) # remove self-loops in the graph

  #Network Measures
  sns.work.subj[sns.work.subj$subject==s, "work.networkSize"] <- vcount(g) #Number of nodes
  sns.work.subj[sns.work.subj$subject==s, "work.meanstrength"] <- mean(strength(g, vids = V(g), loops = F, weights = E(g)$sum)) #Avg strength of edges
  sns.work.subj[sns.work.subj$subject==s, "work.density"] <- mean(edge_density(simplify(g), loops = F)) #network density
}

# compute mean language network across all subjs
work.mean <- sum.adj.work %>% 
  dplyr::select(subject, topic, topic2, sum) %>% 
  dplyr::group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum))

# visualisation of average work network (across all subjs)
work.mean$mean_bin = cut(work.mean$weight, 
                         breaks = c(0, 0.5, 1, 1.5, 2), 
                         labels = 1:4)

g.work <- graph_from_data_frame(work.mean, directed = F) %>% simplify()
work.mean.simplify <- as.data.frame(get.edgelist(g.work)) %>% 
  dplyr::left_join(work.mean, by = c("V1" = "topic", "V2" = "topic2")) %>% 
  dplyr::select(-mean_bin)

#small worldness measure
qgraph::smallworldness(g.work, B=1000)

set.seed(1515)
png("work.plot.png")
dev.control(displaylist="enable")
plot(g.work, vertex.size=5,edge.color = c("pink", "gold", "aquamarine", "lightsteelblue3") [work.mean$mean_bin], vertex.label = NA,
     vertex.color = "white", layout=layout_with_drl(g.work), edge.width=1.3, edge.curved=TRUE)
work.plot = recordPlot()
invisible(dev.off())


##Context network figures & stats

#NETWORK WEIGHT (i.e., how many languages are used to speak about each topic-topic pair in each network?)
contexts.wide <- sum.adj.work %>% 
  dplyr::full_join(sum.adj.home, by = c("subject", "topic", "topic2")) %>% 
  dplyr::full_join(sum.adj.fam, by = c("subject", "topic", "topic2")) %>% 
  dplyr::full_join(sum.adj.school, by = c("subject", "topic", "topic2")) %>% 
  dplyr::full_join(sum.adj.social, by = c("subject", "topic", "topic2")) %>% select(-contains("n_lang"))  %>% select(-contains("avg"))
names(contexts.wide) <- c("subject","topic", "topic2", "work", "home", "fam", "school", "social")

contexts.long <- gather(contexts.wide, context, weight, work:social)
contexts.long$context = factor(contexts.long$context, levels = c("work", "school", "home", "fam", "social"), labels = c("Work", "School", "Home", "Family", "Social"))

contexts.long = contexts.long %>% filter(!is.na(weight))
c.weight.summary = convenience::sem(contexts.long, dv = weight, id = subject, context)

context_weight_plot <- ggplot(c.weight.summary, aes(x= context, y=mean_weight)) +
  expand_limits(y = c(0.2, 1.5)) +
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  labs(x = "", y= "Weight (# languages)") +
  theme_light(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

contexts.aov <- aov(weight ~ context, data = contexts.long)
summary.aov(contexts.aov) # sig***
TukeyHSD(contexts.aov) # social sig higher than all other contexts and work sig lower than all other

#NETWORKS MEASURES 
contexts.mean <- sns.work.subj %>% 
  dplyr::full_join(sns.fam.subj, by = "subject") %>% 
  dplyr::full_join(sns.home.subj, by = "subject") %>% 
  dplyr::full_join(sns.school.subj, by = "subject") %>% 
  dplyr::full_join(sns.social.subj, by = "subject")

context.mean.des <- describe(contexts.mean, skew=FALSE, IQR=FALSE, na.rm = F) #just for quick eye ball 

#NETWORK SIZE (i.e., how many topics are used in each network?)
c.size.long <- contexts.mean %>% 
  select(subject, contains("networkSize")) %>% 
  gather(context, network.size, contains("networkSize"))
c.size.long$context = factor(c.size.long$context, levels = c("work.networkSize", "school.networkSize", "home.networkSize", "fam.networkSize", "social.networkSize"), labels = c("Work", "School", "Home", "Family", "Social"))
c.size.long = c.size.long %>% filter(!is.na(network.size))
c.size.summary = convenience::sem(c.size.long, dv = network.size, id = subject, context)
#write.csv(c.size.summary, "../Tables(forMT)/c.size.summary.csv")

context_size_plot <- ggplot(c.size.summary, aes(x= context, y=mean_network.size)) +
  expand_limits(y = c(10, 22)) +
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  labs(x = "", y= "Network Size") +
  theme_light(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

c.size.aov <- aov(network.size ~ context, data = c.size.long)
summary.aov(c.size.aov) # sig*
TukeyHSD(c.size.aov)

#DENSITY (i.e., how interconnected are the topics used in each network?)
c.density.long <- contexts.mean %>% 
  select(subject, contains("density")) %>% 
  gather(context, network.density, contains("density"))
c.density.long$context = factor(c.density.long$context, levels = c("work.density", "school.density", "home.density", "fam.density", "social.density"), labels = c("Work", "School", "Home", "Family", "Social"))
c.density.long = c.density.long %>% filter(!is.na(network.density))
c.density.summary = convenience::sem(c.density.long, dv = network.density, id = subject, context)
#write.csv(c.density.summary, "../Tables(forMT)/c.density.summary.csv")

context_density_plot <- ggplot(c.density.summary, aes(x= context, y=mean_network.density)) +
  expand_limits(y = c(.9, 1)) +
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  labs(x = "", y= "Network Density") +
  theme_light(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

c.density.aov <- aov(network.density ~ context, data = c.density.long)
summary.aov(c.density.aov) # sig*
TukeyHSD(c.density.aov)


#MEAN STRENGTH (i.e., how likely are all topics to be used in more languages together?)
c.strength.long <- contexts.mean %>% 
  select(subject, contains("strength")) %>% 
  gather(context, strength, contains("strength"))

c.strength.long$context = factor(c.strength.long$context, levels = c("work.meanstrength", "school.meanstrength", "home.meanstrength", "fam.meanstrength", "social.meanstrength"), labels = c("Work", "School", "Home", "Family", "Social"))
c.strength.long = c.strength.long %>% filter(!is.na(strength))
c.strength.summary = convenience::sem(c.strength.long, dv = strength, id = subject, context)
#write.csv(c.strength.summary, "../Tables(forMT)/c.strength.summary.csv")

context_strength_plot <- ggplot(c.strength.summary, aes(x= context, y=mean_strength)) +
  expand_limits(y = c(30, 60)) +
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  labs(x = "", y= "Network Strength") +
  theme_light(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

c.strength.aov <- aov(strength ~ context, data = c.strength.long)
summary.aov(c.strength.aov) # sig***
TukeyHSD(c.strength.aov) #sig*: school-fam, work-fam, work-home, social-school, work-social

merged_context_plots= grid.arrange(context_weight_plot, context_size_plot, context_strength_plot, context_density_plot, nrow=2)
#ggsave("../Tables(forMT)/merged_context_plots.png", merged_context_plots, width = 10)


# Part2 - 2 Language Networks ---------------------------------------------

#DOMINANT LANGUAGE
sns.dom = df %>% dplyr::select(subject, contains("l1"), -fluent.l1)

sns.dom.subj = sns.dom %>% dplyr::select(subject) #subject individual differences will be filled in here

sns.dom = sns.dom %>% gather(measure, value, 2:106) %>% 
  separate(col = measure, into = c("context", "null", "topic", "language"), sep = "_") %>% select(-null) 

sns.dom$value[is.na(sns.dom$value)] <- 0
sns.dom$value[sns.dom$value>1] <- 1

#Make adjacency matrix for each subj
adj.dom <- sns.dom %>% group_by(subject, context) %>% do(make_adj(.)) %>% ungroup()

#Calculate total number of contexts selected by each subj. We will use this to compute a mean weight
sum.dom.context <- adj.dom %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, context) %>% 
  dplyr::summarise(n = any(val == 1)) %>% dplyr::summarise(n_context=sum(n))

# Sum up the adjacency values for each subject for each topic-topic pairing,
# merge with the number of contexts containing responses, and compute mean
sum.adj.dom <- adj.dom %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, topic,topic2) %>% 
  dplyr::summarise(sum=sum(val)) %>% dplyr::left_join(sum.dom.context) %>% dplyr::mutate(avg=sum/n_context) %>% ungroup()

#Remove people who have NA's
sum.adj.dom = sum.adj.dom %>% filter(!is.na(avg))

#Set up network measures
sns.dom.subj$dom.networkSize <- NA
sns.dom.subj$dom.meanstrength <- NA
sns.dom.subj$dom.density <- NA

for (s in unique(sum.adj.dom$subject)){
  print(s) 
  g <- graph_from_data_frame(sum.adj.dom[sum.adj.dom$subject==s & sum.adj.dom$sum > 0,] %>% select(-subject), directed=F) #The reason we restricted sum > 1 is bc we want to remove the topics that were not discussed in any sphere (i.e., remove excess nodes)
  g <- simplify(g, remove.loops = T, remove.multiple = F) # remove self-loops in the graph
  
  #Network Measures
  sns.dom.subj[sns.dom.subj$subject==s, "dom.networkSize"] <- vcount(g) #Number of nodes
  sns.dom.subj[sns.dom.subj$subject==s, "dom.meanstrength"] <- mean(strength(g, vids = V(g), loops = F, weights = E(g)$sum)) #Avg strength of edges
  sns.dom.subj[sns.dom.subj$subject==s, "dom.density"] <- mean(edge_density(simplify(g), loops = F)) #network density
}

# compute mean context network across all subjs
dom.mean <- sum.adj.dom %>% 
  dplyr::select(subject, topic, topic2, sum) %>% 
  dplyr::group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) 

# visualisation of average dominant language network (across all subjs)
dom.mean$mean_bin = cut(dom.mean$weight, 
                        breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), 
                        labels = 1:7)

g.dom <- graph_from_data_frame(dom.mean, directed = F) %>%  simplify()
dom.mean.simplify <- as.data.frame(get.edgelist(g.dom)) %>% 
  dplyr::left_join(dom.mean, by = c("V1" = "topic", "V2" = "topic2")) %>% 
  dplyr::select(-mean_bin)

#small worldness measure
qgraph::smallworldness(g.dom, B=1000)
qgraph::smallworldness(dom.mean.simplify, B=1000)


grid::grid.newpage()
png("dom.plot.png")
dev.control(displaylist="enable")
plot(g.dom, vertex.size=5,edge.color = c("pink", "gold", "aquamarine", "lightsteelblue3", "dodgerblue3", "darkviolet", "maroon2") [dom.mean$mean_bin], vertex.label = NA,
     vertex.color = "white", layout=layout_with_drl(g.dom), edge.width=1.3, edge.curved=TRUE)
dom.plot = recordPlot()
invisible(dev.off())


#NON-DOMINANT LANGUAGE
sns.non = df %>% dplyr::select(subject, contains("l2"), -fluent.l2)

sns.non.subj = sns.non %>% dplyr::select(subject) #subject individual differences will be filled in here

sns.non = sns.non %>% gather(measure, value, 2:106) %>% 
  separate(col = measure, into = c("context", "null", "topic", "language"), sep = "_") %>% select(-null) 

sns.non$value[is.na(sns.non$value)] <- 0
sns.non$value[sns.non$value>1] <- 1

#Make adjacency matrix for each subj
adj.non <- sns.non %>% group_by(subject, context) %>% do(make_adj(.)) %>% ungroup()

#Calculate total number of contexts selected by each subj. We will use this to compute a mean weight
sum.non.context <- adj.non %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, context) %>% 
  dplyr::summarise(n = any(val == 1)) %>% dplyr::summarise(n_context=sum(n))

# Sum up the adjacency values for each subject for each topic-topic pairing,
# merge with the number of contexts containing responses, and compute mean
sum.adj.non <- adj.non %>% gather(topic2, val, chitchat:gossip) %>% dplyr::group_by(subject, topic,topic2) %>% 
  dplyr::summarise(sum=sum(val)) %>% dplyr::left_join(sum.non.context) %>% dplyr::mutate(avg=sum/n_context) %>% ungroup()

#Remove people who have NA's
sum.adj.non = sum.adj.non %>% filter(!is.na(avg))

#Set up network measures
sns.non.subj$non.networkSize <- NA
sns.non.subj$non.meanstrength <- NA
sns.non.subj$non.density <- NA

for (s in unique(sum.adj.non$subject)){
  print(s) 
  g <- graph_from_data_frame(sum.adj.non[sum.adj.non$subject==s & sum.adj.non$sum > 0,] %>% select(-subject), directed=F) #The reason we restricted sum > 1 is bc we want to remove the topics that were not discussed in any sphere (i.e., remove excess nodes)
  g <- simplify(g, remove.loops = T, remove.multiple = F) # remove self-loops in the graph
  
  #Network Measures
  sns.non.subj[sns.non.subj$subject==s, "non.networkSize"] <- vcount(g) #Number of nodes
  sns.non.subj[sns.non.subj$subject==s, "non.meanstrength"] <- mean(strength(g, vids = V(g), loops = F, weights = E(g)$sum)) #Avg strength of edges
  sns.non.subj[sns.non.subj$subject==s, "non.density"] <- mean(edge_density(simplify(g), loops = F)) #network density
}

# compute mean context network across all subjs
non.mean <- sum.adj.non %>% 
  dplyr::select(subject, topic, topic2, sum) %>% 
  dplyr::group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) 

# visualisation of average dominant language network (across all subjs)
non.mean$mean_bin = cut(non.mean$weight, 
                        breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), 
                        labels = 1:7)

g.non <- graph_from_data_frame(non.mean, directed = F) %>%  simplify()
non.mean.simplify <- as.data.frame(get.edgelist(g.non)) %>% 
  dplyr::left_join(non.mean, by = c("V1" = "topic", "V2" = "topic2")) %>% 
  dplyr::select(-mean_bin)

#small worldness measure
qgraph::smallworldness(g.non, B=1000)
qgraph::smallworldness(non.mean.simplify, B=1000)


grid::grid.newpage()
png("non.plot.png")
dev.control(displaylist="enable")
plot(g.non, vertex.size=5,edge.color = c("pink", "gold", "aquamarine", "lightsteelblue3", "dodgerblue3", "darkviolet", "maroon2") [non.mean$mean_bin], vertex.label = NA,
     vertex.color = "white", layout=layout_with_drl(g.non), edge.width=1.3, edge.curved=TRUE)
non.plot = recordPlot()
invisible(dev.off())

##Language network stats

#NETWORK WEIGHT (i.e., how many contexts is each topic-topic pair used in this language?)
language.wide <- sum.adj.dom %>% 
  dplyr::full_join(sum.adj.non, by = c("subject", "topic", "topic2")) %>% select(-contains("n_context"))  %>% select(-contains("avg"))

names(language.wide) <- c("subject","topic", "topic2", "Dominant Language", "Non-dominant Language")

language.long <- gather(language.wide, language, weight, "Dominant Language":"Non-dominant Language")

language.long = language.long %>% filter(!is.na(weight))
l.weight.summary = convenience::sem(language.long, dv = weight, id = subject, language)

language.aov <- aov(weight ~ language, data = language.long)
summary.aov(language.aov) # sig***

#NETWORKS MEASURES 
languages.mean <- sns.dom.subj %>% 
  dplyr::full_join(sns.non.subj, by = "subject")

#language.mean.des <- describe(languages.mean, skew=FALSE, IQR=FALSE, na.rm = F)
#write.csv(languages.mean, "../../participant plot and table/subjs.manuscript.csv") #This will help me get LHQ data for only ppl in the paper


#NETWORK SIZE (i.e., how many topics are used in each network?)
l.size.long <- languages.mean %>% 
  select(subject, contains("networkSize")) %>% 
  gather(language, network.size, contains("networkSize"))

l.size.long = l.size.long %>% filter(!is.na(network.size))
l.size.summary = convenience::sem(l.size.long, dv = network.size, id = subject, language)
#write.csv(l.size.summary, "../Tables(forMT)/l.size.summary.csv")

l.size.aov <- aov(network.size ~ language, data = l.size.long)
summary.aov(l.size.aov) # sig*

#DENSITY (i.e., how interconnected are the topics used in each network?)
l.density.long <- languages.mean %>% 
  select(subject, contains("density")) %>% 
  gather(language, network.density, contains("density"))

l.density.long = l.density.long %>% filter(!is.na(network.density))
l.density.summary = convenience::sem(l.density.long, dv = network.density, id = subject, language)
#write.csv(l.density.summary, "../Tables(forMT)/l.density.summary.csv")

l.density.aov <- aov(network.density ~ language, data = l.density.long)
summary.aov(l.density.aov) # sig*

#MEAN STRENGTH (i.e., how likely are all topics to be used in more languages together?)
l.strength.long <- languages.mean %>% 
  select(subject, contains("strength")) %>% 
  gather(language, strength, contains("strength"))

l.strength.long = l.strength.long %>% filter(!is.na(strength))
l.strength.summary = convenience::sem(l.strength.long, dv = strength, id = subject, language)
#write.csv(l.strength.summary, "../Tables(forMT)/l.strength.summary.csv")

l.strength.aov <- aov(strength ~ language, data = l.strength.long)
summary.aov(l.strength.aov) # sig***

#Merging dominant and non-dominant
language.network.measures = left_join(sns.dom.subj, sns.non.subj, by = "subject")
write.csv(language.network.measures, "../Tables(forMT)/language.network.measures.csv", row.names = F)


# Part3 - Community Detection for Language Networks -----------------------

#AIM: across all subjects, how do conversational topics cluster in each language?
#DOMINANT LANGUAGE
dom.edgelist = sum.adj.dom %>% select(-c(n_context, avg))
dom.edgelist$topic <- gsub(x = dom.edgelist$topic, pattern = "\\.", replacement = "\n") 
dom.edgelist$topic2 <- gsub(x = dom.edgelist$topic2, pattern = "\\.", replacement = "\n") 

dom.edgelist$sum <- as.numeric(dom.edgelist$sum)
dom.nodelist <- unique(dom.edgelist$topic)

# weight thresholding to reduce number of edges in weighted networks
# otherwise too dense for the application of standard graph-theoretical methods (Yan et al., 2018)
# 2-step threshold: first, use subject-level weight-based thresholding
# take off edges with weight of 1 or 2 (i.e., now their weight is 0)
dom.weighted <- dom.edgelist %>% 
  mutate(sum = replace(sum, sum < 3, 0))

# check if weights are properly removed
dom.weighted$sum <- as.numeric(as.character(dom.weighted$sum))
unique(dom.weighted$sum) #yes

# 2nd weight threshold: network-level proportional threshold (keep top 75% of edges)
dom.mean.edgelist <- dom.weighted %>% 
  group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) %>%
  subset(weight > quantile(weight, prob = 1 - 75/100))

unique(dom.mean.edgelist$topic)
unique(dom.mean.edgelist$topic2)

# create igraph object
g.dom <- graph.data.frame(dom.mean.edgelist, directed=F)
g.dom <- simplify(g.dom)

# make mean simplified edgelist for stats
dom.mean <- dom.edgelist %>% 
  group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum))

g.dom.mean <- graph.data.frame(dom.mean, directed = F) %>% simplify()

dom.mean.simplify <- as.data.frame(get.edgelist(g.dom.mean)) %>% 
  left_join(dom.mean, by = c("V1" = "topic", "V2" = "topic2")) 


# Many community detection algorithms possible:

#dom.fastgreedy <- fastgreedy.community(g) #fastgreedy method: greedy optimization of modularity
#dendPlot(dom.fastgreedy)
#dom.fastgreedy
#dom.ceb <- edge.betweenness.community(g) #based on edge betweenness (Newman-Girvan)
#dendPlot(dom.ceb)
#dom.clp <- label.propagation.community(g) #based on propagating labels
#plot(dom.clp, g)
#dom.walktrap <- walktrap.community(g) #walktrap method: finds communities through a series of short random walks (4 steps)
#plot(dom.walktrap, g)
#dendPlot(dom.walktrap)
#dom.infomap <- infomap.community(g) #infomap method: map the flow of information in a network
#plot(dom.infomap, g)

# The one we picked:
# Louvain/multilevel algorithm (Blondel et al., 2008): initially assigns each node to its own community
# nodes are then sequentially assigned to the community that increases modularity (if any) so that communities are merged
dom.louvain <- cluster_louvain(g.dom) 
plot(dom.louvain, g.dom)
dom.louvain #topics in each cluster
dom.louvain$memberships
membership(dom.louvain)

# visualisation
dom.mean.edgelist$mean_bin = cut(dom.mean.edgelist$weight, 
                                breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), 
                                labels = 1:7)


set.seed(222)
plot(dom.louvain, mark.border= "gray", mark.col = c(adjustcolor("gray95",alpha.f = 0.5), adjustcolor("antiquewhite1", alpha.f = 0.5)),
     g.dom, vertex.size=25,edge.color = c("pink", "gold", "aquamarine", "lightsteelblue3", "dodgerblue3", "darkviolet", "maroon2")[dom.mean.edgelist$mean_bin],
     col = c("white", "antiquewhite3")[membership(dom.louvain)], vertex.label.color="black", 
     vertex.label.family="Arial", vertex.label.font=1, vertex.label.cex = 0.8, edge.width=1.5, edge.curved=TRUE)

#NON-DOMINANT LANGUAGE
non.edgelist = sum.adj.non %>% select(-c(n_context, avg))
non.edgelist$topic <- gsub(x = non.edgelist$topic, pattern = "\\.", replacement = "\n") 
non.edgelist$topic2 <- gsub(x = non.edgelist$topic2, pattern = "\\.", replacement = "\n") 

non.edgelist$sum <- as.numeric(non.edgelist$sum)
non.nodelist <- unique(non.edgelist$topic)

# weight thresholding to reduce number of edges in weighted networks
# otherwise too dense for the application of standard graph-theoretical methods (Yan et al., 2018)
# 2-step threshold: first, use subject-level weight-based thresholding
# take off edges with weight of 1 or 2 (i.e., now their weight is 0)
non.weighted <- non.edgelist %>% 
  mutate(sum = replace(sum, sum < 3, 0))

# check if weights are properly removed
non.weighted$sum <- as.numeric(as.character(non.weighted$sum))
unique(non.weighted$sum) #yes

# 2nd weight threshold: network-level proportional threshold (keep top 75% of edges)
non.mean.edgelist <- non.weighted %>% 
  group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum)) %>%
  subset(weight > quantile(weight, prob = 1 - 75/100))

unique(non.mean.edgelist$topic)
unique(non.mean.edgelist$topic2)

# create igraph object for figure
g.non <- graph.data.frame(non.mean.edgelist, directed=F)
g.non <- simplify(g.non)

# make mean simplified edgelist for stats
non.mean <- non.edgelist %>% 
  group_by(topic, topic2) %>% 
  dplyr::summarise(weight = mean(sum))

g.non.mean <- graph.data.frame(non.mean, directed = F) %>% simplify()

non.mean.simplify <- as.data.frame(get.edgelist(g.non.mean)) %>% 
  left_join(non.mean, by = c("V1" = "topic", "V2" = "topic2")) 


# Many community detection algorithms possible:

#non.fastgreedy <- fastgreedy.community(g) #fastgreedy method: greedy optimization of modularity
#dendPlot(non.fastgreedy)
#non.fastgreedy
#non.ceb <- edge.betweenness.community(g) #based on edge betweenness (Newman-Girvan)
#dendPlot(non.ceb)
#non.clp <- label.propagation.community(g) #based on propagating labels
#plot(non.clp, g)
#non.walktrap <- walktrap.community(g) #walktrap method: finds communities through a series of short rannon walks (4 steps)
#plot(non.walktrap, g)
#dendPlot(non.walktrap)
#non.infomap <- infomap.community(g) #infomap method: map the flow of information in a network
#plot(non.infomap, g)

# The one we picked:
# Louvain/multilevel algorithm (Blondel et al., 2008): initially assigns each node to its own community
# nodes are then sequentially assigned to the community that increases modularity (if any) so that communities are merged
non.louvain <- cluster_louvain(g.non) 
plot(non.louvain, g.non)
non.louvain #topics in each cluster
non.louvain$memberships
membership(non.louvain)

# visualisation
non.mean.edgelist$mean_bin = cut(non.mean.edgelist$weight, 
                                 breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), 
                                 labels = 1:7)


set.seed(303)
plot(non.louvain, mark.border= "gray", mark.col = c(adjustcolor("gray95",alpha.f = 0.5), adjustcolor("antiquewhite1", alpha.f = 0.5), adjustcolor("honeydew", alpha.f = 0.5)),
     g.non, vertex.size=25,edge.color =c("gold", "aquamarine", "lightsteelblue3", "dodgerblue3", "darkviolet", "orangered3")[non.mean.edgelist$mean_bin],
     col = c("white", "antiquewhite3", "honeydew3")[membership(non.louvain)], vertex.label.color= "black", 
     vertex.label.family="Arial", vertex.label.font=1, vertex.label.cex = 0.9, edge.width=1.5, edge.curved=TRUE)


