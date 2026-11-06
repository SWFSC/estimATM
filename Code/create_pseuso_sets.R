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

clf.ns.pseudo <- sets.pseudo %>% 
  select(cluster, sample.type, long, lat, X, Y, pseudo.cluster) %>% 
  left_join(select(clf.ns, -(sample.type:Y)), by = c("pseudo.cluster" = "cluster")) #%>% 
  # select(-pseudo.cluster)

hlf.ns.pseudo <- sets.pseudo %>% 
  select(haul, sample.type, long, lat, X, Y, pseudo.haul) %>% 
  left_join(select(hlf.ns, -(sample.type:Y)), by = c("pseudo.haul" = "haul")) #%>% 
  # select(-pseudo.haul)

# Combine clf, hlf, and pseudo sets 
clf.ns <- clf.ns %>%
  bind_rows(clf.ns.pseudo) 

hlf.ns <- hlf.ns %>%
  bind_rows(hlf.ns.pseudo) 

# Visually inspect matching, species proportions, etc.
# clf.ns <- st_as_sf(clf.ns, coords = c("long","lat"), crs = 4326)
# hlf.ns <- st_as_sf(hlf.ns, coords = c("long","lat"), crs = 4326)
# mapview(clf.ns, zcol = "sample.type")
# mapview(hlf.ns, zcol = "sample.type")

# RESUME HERE -----------------
# Filter lf.final to only include clusters in the pseudo set list
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

# Save super clusters and hauls
save(super.clusters.ns, super.hauls.ns,
     file = here("Output/super_clusters_hauls_ns.Rdata"))
