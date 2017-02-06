#######
# Using social network analysis methods to look at Clinton's Emails


# Data source: Kaggle competiton
# https://www.kaggle.com/kaggle/hillary-clinton-emails
#######
library(dplyr)
library(igraph)
install.packages('stringr')
library(stringr)
library(readr)
emails = read_csv("data/Emails.csv")
persons = read_csv("data/Persons.csv")
receivers = read_csv("data/EmailReceivers.csv")
# rename the Personid variable in receivers dataset as ReceiverID
receivers = rename(receivers,  ReceiverId = PersonId)

# merge with the original email dataset (you can also use sqldf)
# notice that it has more observations than original emails dataset, why?
emails_joined = left_join(emails, receivers, by = c("Id" = "EmailId"))
rm(emails) # remove original emails from enviroment

# create the edge list
emails_edge_list = select(emails_joined, SenderPersonId, ReceiverId)
emails_edge_list = emails_edge_list[complete.cases(emails_edge_list),]
email_graph = graph.data.frame(emails_edge_list,directed=FALSE)
V(email_graph)$id = V(email_graph)$name

V(email_graph)$name = persons$Name[match(V(email_graph)$name, as.character(persons$Id))]

V(email_graph)$name
V(email_graph)$id
# visualization
plot(email_graph, layout=layout.fruchterman.reingold(email_graph),
     vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

# descriptive stats
vcount(email_graph)
sort(degree(email_graph), decreasing = T)[1:10]
cliques(email_graph, min = 6)
maximal.cliques(email_graph, min = 6)


# President Clinton's 2-degree egocentric network
bill_egocentric = graph.neighborhood(email_graph, order = 2,
                    nodes = which(V(email_graph)$name == "Bill Clinton"))[[1]]
plot(bill_egocentric)

# remove Hillary from the network
hillary_egocentric = delete.vertices(email_graph,
                                     which(V(email_graph)$name == "Hillary Clinton"))
hillary_egocentric = simplify(hillary_egocentric)
vcount(hillary_egocentric)
plot(hillary_egocentric, layout=layout.fruchterman.reingold(email_graph),
     vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)
sort(degree(hillary_egocentric), decreasing = T)[1:10]

# communities in the giant component
components(hillary_egocentric)
hillary_egocentric_GC = delete.vertices(hillary_egocentric,
                        which(components(hillary_egocentric)$membership  != 1)) 
community = fastgreedy.community(hillary_egocentric_GC)

community$membership
plot(hillary_egocentric_GC,
     vertex.color = community$membership, vertex.size = log(degree(hillary_egocentric) + 1),
     mark.groups = by(seq_along(community$membership), community$membership, invisible),
     layout=layout.fruchterman.reingold)

######### Closeness Centrality and Election-related Email ###########
# create a new variable "election_related" if election is mentioned in subject or text
emails_joined$election_related =
  str_detect(tolower(emails_joined$ExtractedBodyText), "election") |
  str_detect(tolower(emails_joined$MetadataSubject), "election")

# tag a node's election attribute as TRUE if it is involved in election discussion
election_nodes =
  unique(unlist(filter(emails_joined, election_related == TRUE) %>% select(ReceiverId, SenderPersonId)))
election_names = persons$Name[match(election_nodes, as.character(persons$Id))]

V(hillary_egocentric)$election = V(hillary_egocentric)$name %in% election_names
V(hillary_egocentric)[V(hillary_egocentric)$election == TRUE]$color = "red"
plot(hillary_egocentric, layout=layout.fruchterman.reingold(hillary_egocentric),
     vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

closeness_centrality = closeness(hillary_egocentric, normalized = TRUE)

# Fit a logistic regression model
logit_df = data.frame(V(hillary_egocentric)$election, closeness_centrality)
names(logit_df) = c("Election", "Closeness")
summary(glm(Election ~ Closeness, data = logit_df, family = "binomial"))

# note that we cannot make causal claim here
# high centrality -> discuss election, or discuss election -> high centrality?

######### Average (log) Length of Received Emails and Degree Centrality ########
emails_joined$email_length = log(nchar(emails_joined$ExtractedBodyText)+1)
avg_length = emails_joined %>% group_by(ReceiverId) %>% summarise(avg_sent_length = mean(email_length))
hist(emails_joined$email_length)
V(hillary_egocentric)$avg_length =
  avg_length$avg_sent_length[match(V(hillary_egocentric)$id, as.character(avg_length$ReceiverId))]

closeness_centrality = closeness(hillary_egocentric)

# Fit a regression model
reg_df = data.frame(V(hillary_egocentric)$avg_length, closeness_centrality)
names(reg_df) = c("Length", "Closeness")
summary(lm(Length ~ Closeness, data = reg_df))
cor(reg_df,  use = "complete.obs")
