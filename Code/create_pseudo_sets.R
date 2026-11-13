# Code to create pseudo sets given a set of locations and a corresponding hlf/clf file
# Read pseudo set file -----------------------------------------------------------------
sets.pseudo <- read_csv(seine.pseudo.name) %>%
  arrange(datetime) %>% 
  # Create new cluster/haul numbers
  mutate(cluster = max(clf.ns$cluster) + as.numeric(as.factor(id)),
         haul    = max(hlf.ns$haul) + as.numeric(as.factor(id)),
         sample.type = "Pseudo set") %>%
  project_df(to = crs.proj) 

# Match pseudo sets to nearest trawl haul and cluster ----------------------------------
set.match <- sets.pseudo %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

# Get cluster/haul data to match with nearest pseudo set
## Only include trawl data to avoid matching with the problematic data
clf.match <- super.clusters.ns %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  filter(sample.type == "Trawl")

hlf.match <- super.hauls.ns %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  filter(sample.type == "Trawl")

# Find nearest cluster ----------------------------
# Returns a vector of nearest clusters
nearest.pseudo.cluster <- st_nearest_feature(set.match, clf.match)
nearest.pseudo.haul    <- st_nearest_feature(set.match, hlf.match)

# Expand clf to match nasc ------------------------
pseudo.cluster.sp <- clf.match[nearest.pseudo.cluster, ] %>% 
  select(geometry) %>% 
  as_Spatial()

pseudo.haul.sp <- hlf.match[nearest.pseudo.haul, ] %>% 
  select(geometry) %>% 
  as_Spatial()

# Make pseudo sets sp
# Removing the other data (i.e., only retaining the geometry) decreases the size 50-fold
set.match.sp <- set.match %>% 
  select(geometry) %>% 
  as_Spatial()

# Compute distances with {geosphere}
## Must be done in WGS84 projection
set.match <- set.match %>% 
  mutate(pseudo.cluster = clf.match$cluster[nearest.pseudo.cluster],
         pseudo.cluster.distance = distGeo(set.match.sp, pseudo.cluster.sp)*0.000539957,
         pseudo.haul = hlf.match$haul[nearest.pseudo.haul],
         pseudo.haul.distance = distGeo(set.match.sp, pseudo.haul.sp)*0.000539957)  

# Remove geometry
set.match <- st_set_geometry(set.match, NULL) 

# Add clusters and cluster distances to nasc
sets.pseudo <- sets.pseudo %>% 
  bind_cols(select(set.match, pseudo.cluster, pseudo.cluster.distance, 
                   pseudo.haul, pseudo.haul.distance))

# Add cluster and haul info to each pseudo set
clf.ns.pseudo <- sets.pseudo %>% 
  select(cluster, sample.type, long, lat, X, Y, pseudo.cluster) %>% 
  left_join(select(clf.ns, -(sample.type:Y)), by = c("pseudo.cluster" = "cluster")) 

hlf.ns.pseudo <- sets.pseudo %>% 
  select(haul, sample.type, long, lat, X, Y, pseudo.haul) %>% 
  left_join(select(hlf.ns, -(sample.type:Y)), by = c("pseudo.haul" = "haul")) 

# Combine clf, hlf, and pseudo sets 
clf.ns <- clf.ns %>%
  bind_rows(clf.ns.pseudo) %>% 
  select(-pseudo.cluster)

hlf.ns <- hlf.ns %>%
  bind_rows(hlf.ns.pseudo) %>% 
  select(-pseudo.haul) 

# Visually inspect matching, species proportions, etc.
clf.ns.sf <- st_as_sf(clf.ns, coords = c("long","lat"), crs = 4326)
hlf.ns.sf <- st_as_sf(hlf.ns, coords = c("long","lat"), crs = 4326)
# mapview(clf.ns.sf, zcol = "sample.type")
# mapview(hlf.ns.sf, zcol = "sample.type")

# Filter lf.final to only include clusters in the pseudo set list
# Then, update cluster numbers to use the pseudo sets
# load(here("Output/cluster_length_frequency_tables.Rdata"))

lf.pseudo.ns <- lf.final %>% 
  filter(cluster %in% unique(sets.pseudo$pseudo.cluster)) %>% 
  rename(pseudo.cluster = cluster, pseudo.haul = haul) %>% 
  left_join(select(sets.pseudo, pseudo.cluster, cluster)) %>% 
  select(-pseudo.cluster, -pseudo.haul) %>% 
  arrange(scientificName, cluster, lf.labels) %>%
  mutate(sample.type = "Pseudo set")

# Add pseudo set lf to lf.final.ns
lf.final.ns <- lf.final.ns %>% 
  bind_rows(lf.pseudo.ns) 

# Combine super clusters from the core and nearshore regions
super.pseudo.clusters <- sets.pseudo %>% 
  select(cluster, sample.type, long, lat, X, Y)

super.pseudo.hauls <- sets.pseudo %>% 
  select(haul, sample.type, long, lat, X, Y)

super.clusters.ns <- bind_rows(super.clusters.ns, super.pseudo.clusters)

super.hauls.ns    <- bind_rows(super.hauls.ns, super.pseudo.hauls) 

# ggplot(super.clusters.ns, aes(long, lat, color = sample.type)) + 
#   geom_point() + coord_map()

# ggplot(super.hauls.ns, aes(long, lat, color = sample.type)) + 
#   geom_point() + coord_map()

# Save super clusters and hauls
save(super.clusters.ns, super.hauls.ns,
     file = here("Output/super_clusters_hauls_ns.Rdata"))

# Calculate length frequencies for each species and pseudo set cluster --------------------
# Create a list for storing final species-specific cluster results
cluster.final.pseudo <-  list() #clf.ns.pseudo
haul.final.pseudo    <-  list() #hlf.ns.pseudo
lf.final.pseudo      <-  data.frame() #lf.pseudo.ns

# Subset lengths to include only species present in pseudo sets
lengths.pseudo <- lengths %>% 
  filter(haul %in% sets.pseudo$pseudo.haul)

# For each species, calculate length frequencies, combine with clf, and write final file
for (i in unique(lengths.pseudo$scientificName)) {
  # Create a data frame for results
  lf.df.pseudo <- data.frame()
  
  for (ii in unique(sets.pseudo$haul)) {
    # i = "Engraulis mordax"
    #ii = 168
    
    # Get haul corresponding to pseudo.hull[ii]
    haul.pseudo <- sets.pseudo$pseudo.haul[sets.pseudo$haul == ii]
    # Subset specimen data by species and trawl cluster
    lengths.sub <- droplevels(filter(lengths.pseudo, scientificName == i & haul == haul.pseudo))
    # Define length bins
    lf.breaks <- seq(length.min - 0.5, length.max$sl[length.max$species == i] + 0.5, 1)
    lf.labels <- seq(length.min, length.max$sl[length.max$species == i], 1)
    # Calculate the proportion (histogram density) of individuals in each size class
    # Setting right = F includes X.5 in the lower class (e.g., 14.5 = 14, 14.6 = 15)
    # Length inputs are native lengths (i.e., SL for sardine and anchovy, FL for herring and mackerels)
    if (i %in% c("Sardinops sagax", "Engraulis mordax")) {
      f <- hist(lengths.sub$standardLength_mm/10, breaks = lf.breaks, plot = FALSE, right = FALSE)$density      
    } else if (i %in% c("Scomber japonicus", "Trachurus symmetricus", "Clupea pallasii", 
                        "Etrumeus acuminatus", "Allosmerus elongatus")) {
      f <- hist(lengths.sub$forkLength_mm/10, breaks = lf.breaks, plot = FALSE, right = FALSE)$density        
    }
    # Create a vector of size class
    lf.bin <- lf.breaks[1:length(lf.breaks) - 1]
    # Create a vector of cluster number
    # haul <- rep(ii, length(f))
    # Add results to data frame
    lf.df.pseudo <- rbind(lf.df.pseudo, data.frame(haul = ii, lf.bin, lf.labels, f))
  }
  
  # Replace NaN values with zeros
  lf.df.pseudo$f[is.nan(lf.df.pseudo$f)] <- 0
  
  # Get estimated numbers of individuals for each haul
  n.summ.haul.sub <- data.frame()
  
  for (jj in sets.pseudo$haul){
    # jj = 168
    
    n.summ.haul.jj <- filter(n.summ.haul, as.character(scientificName) == i, 
                             haul == sets.pseudo$pseudo.haul[sets.pseudo$haul == jj]) %>% 
      mutate(haul = jj,
             cluster = sets.pseudo$cluster[sets.pseudo$haul == jj])
    
    n.summ.haul.sub <- rbind(n.summ.haul.sub, n.summ.haul.jj)
  }
  
  # Add estimated total number of individuals in each cluster (based on subsample weight) 
  # to the length frequency data frame
  lf.df.pseudo <- left_join(lf.df.pseudo, n.summ.haul.sub) %>% 
    rename(spp.num = num) %>% 
    replace_na(list(spp.num = 0)) %>%
    mutate(counts = f * spp.num) 
  
  # Summarise lengths by cluster, to filter subsequent data frames
  lf.df.pseudo.summ <- lf.df.pseudo %>% 
    group_by(cluster, haul) %>% 
    summarise(nIndiv = sum(counts)) %>% 
    filter(nIndiv > 0)
  
  # Calculate the estimated number of individuals in each size class
  lf.df.pseudo <- lf.df.pseudo %>% 
    mutate(counts = f * spp.num) %>% 
    filter(is.na(cluster) == FALSE)
  
  # Combine length frequency data for plotting later
  lf.final.pseudo <- bind_rows(lf.final.pseudo, lf.df.pseudo)
  
  # Reshape data frame by cluster for adding to clf
  lf.table <- pivot_wider(lf.df.pseudo, id_cols = cluster, names_from = lf.labels, 
                          values_from = counts, values_fn = sum) %>% 
    rowwise(cluster) %>%
    mutate('(all)' = sum(c_across(all_of(lf.labels)))) %>% 
    arrange(cluster)
  
  lf.table.h <- pivot_wider(lf.df.pseudo, id_cols = haul, names_from = lf.labels, 
                            values_from = counts, values_fn = sum) %>% 
    rowwise(haul) %>%
    mutate('(all)' = sum(c_across(all_of(lf.labels)))) %>% 
    arrange(haul)
  
  # Convert counts to relative frequencies
  lf.table[ ,2:ncol(lf.table)] <- lf.table[ , 2:ncol(lf.table)]/lf.table$`(all)`
  lf.table.h[ ,2:ncol(lf.table.h)] <- lf.table.h[ , 2:ncol(lf.table.h)]/lf.table.h$`(all)`
  
  # Rename columns
  names(lf.table)[2:ncol(lf.table)] <- paste0("L",names(lf.table)[2:ncol(lf.table)])
  # lf.table <- left_join(select(cluster.mid, cluster), lf.table)
  
  names(lf.table.h)[2:ncol(lf.table.h)] <- paste0("L",names(lf.table.h)[2:ncol(lf.table.h)])
  # lf.table.h <- left_join(select(haul.mid, haul), lf.table.h) %>% 
  #   ungroup() %>% 
  #   select(-cluster)
  
  # Replace NA values with zeros
  lf.table[is.na(lf.table)] <- 0
  lf.table.h[is.na(lf.table.h)] <- 0
  
  # Add length frequencies for each species to clf for final analysis
  clf.pseudo.final <- left_join(clf.ns.pseudo, lf.table, by = 'cluster')
  hlf.pseudo.final <- left_join(hlf.ns.pseudo, lf.table.h, by = "haul")
  
  # Add the species name to clf (for plotting)
  clf.pseudo.final$species <- i
  hlf.pseudo.final$species <- i
  
  # Write final data frame to .csv for each species
  write_csv(clf.pseudo.final,
            file = paste0(here("Output/cluster_length_frequency_pseudo_"), 
                          i, "_",survey.name, ".csv"))
  write_csv(hlf.pseudo.final,
            file = paste0(here("Output/haul_length_frequency_pseudo_"), 
                          i, "_",survey.name, ".csv"))
  
  # Add species-specific results to list of results
  cluster.final.pseudo[[i]] <- clf.pseudo.final
  haul.final.pseudo[[i]]    <- hlf.pseudo.final
  
  # Plot length frequencies by species and cluster
  lf.plot.pseudo <- ggplot(lf.df.pseudo, aes(lf.labels, counts)) + geom_bar(stat = 'identity') + 
    facet_wrap(~cluster,scales = 'free_y') + theme_bw() + 
    xlab("Standard length (cm)") + ylab("Counts") + ggtitle(i)
  
  lf.plot.haul.pseudo <- ggplot(lf.df.pseudo, aes(lf.labels, counts)) + geom_bar(stat = 'identity') + 
    facet_wrap(~haul,scales = 'free_y') + theme_bw() + 
    xlab("Standard length (cm)") + ylab("Counts") + ggtitle(i)
  
  # Save plot
  ggsave(lf.plot.pseudo, 
         filename = paste0(here("Figs/fig_cluster_length_frequency_pseudo_"), i, ".png"),
         height = 7, width = 10)
  
  ggsave(lf.plot.haul.pseudo, 
         filename = paste0(here("Figs/fig_haul_length_frequency_pseudo_"), i, ".png"),
         height = 7, width = 10)
}

# Calculate cluster length frequencies for purse seine data ---------------
# Calculate numeric abundance by species and set
n.summ.pseudo <- n.summ.haul %>%
  filter(haul %in% sets.pseudo$pseudo.haul) %>% 
  ungroup() %>% 
  rename(pseudo.haul = haul) %>% 
  select(-cluster) %>% # Remove cluster for now
  left_join(select(sets.pseudo, pseudo.haul, haul)) %>%
  left_join(select(sets.pseudo, haul, cluster)) %>%
  select(scientificName, cluster, haul, num) %>% 
  arrange(scientificName, haul)

# Summarise weights and sigmas by haul
l.summ.pseudo <- l.summ.haul %>%
  filter(haul %in% sets.pseudo$pseudo.haul) %>% 
  # left_join(n.summ.pseudo) %>%
  ungroup() %>% 
  rename(pseudo.haul = haul) %>% 
  select(-cluster) %>% # Remove cluster for now
  left_join(select(sets.pseudo, pseudo.haul, haul)) %>%
  left_join(select(sets.pseudo, haul, cluster)) %>%
  select(-pseudo.haul) %>% 
  select(scientificName, cluster, haul, everything()) %>% 
  arrange(scientificName, haul)
  
# Save updated objects
save(n.summ.pseudo, l.summ.pseudo,
     file = here("Output/seine_summaries_pseudo.Rdata"))
save(cluster.final.pseudo,
     file = here("Output/cluster_length_frequency_all_pseudo.Rdata"))
save(haul.final.pseudo,
     file = here("Output/haul_length_frequency_all_pseudo.Rdata"))
save(lf.final.pseudo,
     file = here("Output/cluster_length_frequency_tables_pseudo.Rdata"))

# Save super clusters and hauls
save(super.clusters.ns, super.hauls.ns,
     file = here("Output/super_clusters_hauls_ns.Rdata"))
