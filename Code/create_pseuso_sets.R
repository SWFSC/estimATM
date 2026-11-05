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

clf.match <- super.clusters.ns %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) 

# Need to remove purse seine data to ensure matching with closest trawl

hlf.match <- super.hauls.ns %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

cluster.match.ns <- super.clusters.ns %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  filter(sample.type %in% catch.source.ns)

haul.match.ns <- super.hauls.ns %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  filter(sample.type %in% catch.source.ns)

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

# Make nasc sp
# Removing the other data (i.e., only retaining the geometry) decreases the size of nasc from 50 to 1 MB
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
  left_join(select(clf.ns, -(sample.type:Y)), by = c("pseudo.cluster" = "cluster")) %>% 
  select(-pseudo.cluster)

hlf.ns.pseudo <- sets.pseudo %>% 
  select(haul, sample.type, long, lat, X, Y, pseudo.haul) %>% 
  left_join(select(hlf.ns, -(sample.type:Y)), by = c("pseudo.haul" = "haul")) %>% 
  select(-pseudo.haul)

# Combine clf, hlf, and pseudo sets 
clf.ns <- clf.ns %>%
  bind_rows(clf.ns.pseudo) 

hlf.ns <- hlf.ns %>%
  bind_rows(hlf.ns.pseudo) 

# RESUME HERE -----------------
lf.pseudo.ns <- lf.final %>% 
  filter(cluster %in% unique(sets.pseudo$pseudo.cluster))


lf.final.ns <- lf.final %>% 
  bind_rows(lf.final.seine) %>% 
  filter(sample.type %in% catch.source.ns)

# Combine super clusters from the core and nearshore regions
super.hauls.ns    <- bind_rows(super.hauls, super.clusters.ns) %>% 
  filter(sample.type %in% catch.source.ns)

super.clusters.ns <- bind_rows(super.clusters, super.clusters.ns) %>% 
  filter(sample.type %in% catch.source.ns)

# Save super clusters and hauls
save(super.clusters.ns, super.hauls.ns,
     file = here("Output/super_clusters_hauls_ns.Rdata"))
