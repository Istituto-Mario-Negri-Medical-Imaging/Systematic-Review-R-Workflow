# Cell Species ####

# 1) Clean + separa eventuali multiple Cell_Species (es. "A; B; C")
cell_species_long <- Cell_All %>%
  filter(!is.na(Cell_Species), Cell_Species != "") %>%
  separate_rows(Cell_Species, sep = ";") %>%
  mutate(Cell_Species = str_trim(Cell_Species)) %>%
  distinct(ID_Article, ID_Cell, Organ_cell, Cell_Species)

# 2) Totali articoli unici per organo (denominatore % within organ)
denom_by_organ <- cell_species_long %>%
  distinct(ID_Article, ID_Cell, Organ_cell) %>%
  count(Organ_cell, name = "Total_Cells_Organ")

denom_overall <- cell_species_long %>%
  distinct(ID_Article, ID_Cell, Organ_cell) %>%
  nrow()

# 3) Distribuzione per organo (conteggi cellule) + % within organ
dist_cell_species_organ <- cell_species_long %>%
  count(Organ_cell, Cell_Species, name = "Cells") %>%
  left_join(denom_by_organ, by = "Organ_cell") %>%
  mutate(Percent_within_organ = round(Cells / Total_Cells_Organ * 100, 2))

# 4) Overall (celle uniche per specie su tutte le coppie organo)
dist_cell_species_overall <- cell_species_long %>%
  count(Cell_Species, name = "Overall_Cells") %>%
  mutate(Percent_Overall = round(Overall_Cells / denom_overall * 100, 2))

# 5) Merge overall nel main
dist_cell_species <- dist_cell_species_organ %>%
  left_join(dist_cell_species_overall, by = "Cell_Species")

wide_counts <- dist_cell_species %>%
  select(Organ_cell, Cell_Species, Cells, Overall_Cells) %>%
  pivot_wider(names_from = Organ_cell, values_from = Cells, values_fill = 0) %>%
  mutate(Overall = Overall_Cells) %>%
  select(-Overall_Cells) %>%
  arrange(desc(Overall)) %>%
  select(Cell_Species, all_of(organi), Overall)

wide_percent <- dist_cell_species %>%
  select(Organ_cell, Cell_Species, Percent_within_organ, Percent_Overall) %>%
  pivot_wider(names_from = Organ_cell, values_from = Percent_within_organ, values_fill = 0) %>%
  mutate(Overall = Percent_Overall) %>%
  select(-Percent_Overall) %>%
  arrange(desc(Overall)) %>%
  select(Cell_Species, all_of(organi), Overall)

## Export Excel ####
write_xlsx(
  list(
    Wide_Counts = wide_counts,
    Wide_Percentages = wide_percent
  ),
  "02_CellSpecies_By_Organ.xlsx"
)

