# Matching taxonomy between GZTD and SeaLifeBase
# 2026-09-10
# P. Pata

library(tidyverse)
library(rfishbase)

`%notin%` <- Negate(`%in%`)


# only count species in the trait dabase with records
taxonomy.GZTD <- read.csv("../Zooplankton_trait_database/data_input/Trait_dataset_level1/trait_dataset_level1-2023-08-15.csv") %>% 
  bind_rows(trait.table <- read.csv("../Zooplankton_trait_database/data_input/Trait_dataset_level1/trait_dataset_level1_excluded-2023-08-15.csv")) %>% 
  group_by(taxonID, scientificName, acceptedNameUsageID, acceptedNameUsage,
           taxonRank, kingdom, phylum, class, order, family, genus, majorgroup) %>% 
  summarise(nrecords = n(), .groups = "drop")

# # GZTD taxonomy
# Note that a taxonID can have multiple rows if verbatim names are different
taxonomy.GZTD2 <- read.csv("../Zooplankton_trait_database/data_input/taxonomy_table_20230628.csv") %>% filter(kingdom == "Animalia") %>%
  distinct(taxonID, .keep_all = T)
# rm(trait.table)

# Get SLB taxonomy
taxonomy.SLB <- rfishbase::load_taxa(server = "sealifebase") 

# What are the GZTD phyla not in SLB? Chaetognatha
taxonomy.GZTD %>% 
  distinct(phylum) %>% 
  left_join(taxonomy.SLB %>% 
              distinct(Phylum, Kingdom), by = join_by("phylum" == "Phylum"))

# Note that in SLB "Copepoda" is not in phylum "Arthropoda" or in kingdom "Animalia"
taxonomy.GZTD %>% 
  distinct(class) %>% 
  left_join(taxonomy.SLB %>% 
              distinct(Phylum, Class), by = join_by("class" == "Class"))

# Basic species name match (based on species name since SpecCode is not AphiaID)
species.match <- taxonomy.GZTD %>% 
  # filter(scientificName %in% taxonomy.SLB$Species) %>% 
  left_join(taxonomy.SLB %>% select(Species, SpecCode),
            by = join_by("scientificName" == "Species")) %>% 
  rename(GZTD_taxonID = taxonID, GZTD_nrecords = nrecords)

write.csv(species.match, row.names = F, na = "",
          file = "data/taxonomy_list_GZTD_20260910.csv")

species.match %>% 
  filter(!is.na(SpecCode)) %>% 
  nrow()

species.match %>% 
  filter(is.na(SpecCode)) %>% 
  group_by(taxonRank) %>% 
  count()
