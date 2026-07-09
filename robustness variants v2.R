# ============================================================================
#  ROBUSTNESS VARIANTS v2 — saubere Neufassung
#  ----------------------------------------------------------------------------
#  Berechnet ALLE vier Mapping-Varianten (A, B, C, D) dynamisch aus denselben
#  bereinigten Rohdaten und schätzt sie mit fixest::feols inkl. geclusterter
#  Standardfehler.
#
#  Verbesserungen gegenüber "robustness variants.R" (v1):
#   [ 1] left_join + coalesce statt Base-R match() — lesbarer, sicherer
#   [2] Berichtslücken werden NICHT mehr stillschweigend als 0 gewertet:
#              (Land, Aktivität, Jahr)-Zellen, die INNERHALB der aktiven
#              Berichtsspanne eines Paares fehlen, werden identifiziert und
#              aus der Regression ausgeschlossen (11 Zellen, 0,22 %).
#              Echte Nicht-Betroffenheit (Sektor nie im ETS bzw. Land hat
#              die Aktivität gar nicht) bleibt korrekt bei Kosten = 0.
#   [3] fixest::feols statt lm(): schneller, und Standardfehler werden
#              sowohl klassisch (iid) als auch geclustert (Land + Sektor)
#              berichtet. Clustering ändert nur SE/p-Werte, nie den Koeffizienten.
#   [4] Kein Hardcoding mehr: auch Varianten A und B werden aus den
#              aktuellen Daten berechnet. Ändert sich die Bereinigung, ändern
#              sich alle vier Varianten konsistent mit.
#
#  Ausführen aus dem Projekt-Hauptordner. Benötigt: readxl, dplyr, stringr,
#  tidyr, igraph, fixest, writexl.
# ============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(igraph)
  library(fixest)
  library(writexl)
})

normalize_nace_label <- function(x) {
  x %>%
    str_remove("\\s*\\(en\\)\\s*$") %>%
    str_squish() %>%
    str_to_lower()
}

# ===========================================================================
# 1. ROHDATEN LADEN & BEREINIGEN — einmal, für alle Varianten identisch
# ===========================================================================

mapping <- read_excel(
  "data/raw/mapping_ets_activity_nace_level3.xlsx", sheet = "Mapping"
) %>%
  filter(!is.na(ets_activity_code), !is.na(nace_level_3_label)) %>%
  mutate(
    ets_activity_code = as.character(ets_activity_code),
    nace_join_key = normalize_nace_label(nace_level_3_label)
  ) %>%
  distinct(ets_activity_code, nace_join_key, nace_level_3_label)

turnover <- read_excel(
  "data/turnover_by_nace_2.0_level_3_coun_year_2013_24.xlsx"
) %>%
  mutate(
    nace_join_key = normalize_nace_label(nace_category_name),
    # HIER IST DIE KORREKTUR: as.numeric() zwingt R, aus Text Zahlen zu machen
    turnover_eur = as.numeric(turnover_million_eur) * 1e6
  ) %>%
  filter(!is.na(turnover_eur),
         turnover_eur > 0,                
         country_code != "EU27_2020") %>%
  select(country_code, year, nace_category_name, nace_join_key, turnover_eur)
net_cost <- read_excel(
  "data/ets_net_cost_by_year_coun_acti_labeled.xlsx"
) %>%
  mutate(ets_activity_code = as.character(ets_activity_code)) %>%
  filter(!is.na(ets_net_cost_eur), year >= 2013, year <= 2024) %>%
  select(country_code, year, ets_activity_code, ets_net_cost_eur)

# ===========================================================================
# 2. BERICHTSLÜCKEN identifizieren
#    Eine "Lücke" = ein Jahr fehlt INNERHALB der Spanne zwischen erstem und
#    letztem berichteten Jahr eines (Land, Aktivität)-Paares. Solche Zellen
#    bedeuten "unbekannt", nicht "0", und werden später ausgeschlossen.
#    Jahre VOR dem ersten / NACH dem letzten Auftreten gelten weiterhin als
#    echte Nicht-Aktivität (Kosten = 0 über die Kontrollgruppen-Logik).
# ===========================================================================

coverage <- net_cost %>%
  group_by(country_code, ets_activity_code) %>%
  summarise(erstes_jahr = min(year), letztes_jahr = max(year), .groups = "drop")

grid_expected <- coverage %>%
  rowwise() %>%
  mutate(year = list(seq(erstes_jahr, letztes_jahr))) %>%
  unnest(year) %>%
  select(country_code, ets_activity_code, year)

reporting_gaps <- grid_expected %>%
  anti_join(net_cost, by = c("country_code", "ets_activity_code", "year"))

cat("Berichtslücken (Land-Aktivität-Jahr-Zellen, die 'unbekannt' statt '0' sind):",
    nrow(reporting_gaps), "\n")

# Lücken auf NACE-Ebene übersetzen (welche Land-Jahr-Sektor-Kombis betrifft das?)
gap_nace <- reporting_gaps %>%
  inner_join(mapping, by = "ets_activity_code",
             relationship = "many-to-many") %>%
  distinct(country_code, year, nace_join_key) %>%
  mutate(has_reporting_gap = TRUE)

# ===========================================================================
# 3. GRAPH-KOMPONENTEN (für Varianten C und D)
# ===========================================================================

edges <- mapping %>%
  transmute(ets_node = paste0("ETS:", ets_activity_code),
            nace_node = paste0("NACE:", nace_join_key))

g <- graph_from_data_frame(edges, directed = FALSE)
comp <- components(g)
comp_df <- tibble(node = names(comp$membership), comp_id = comp$membership) %>%
  left_join(tibble(comp_id = seq_len(comp$no), comp_size = comp$csize),
            by = "comp_id")

ets_component <- comp_df %>%
  filter(str_starts(node, "ETS:")) %>%
  transmute(ets_activity_code = str_remove(node, "ETS:"), comp_id, comp_size)

nace_component <- comp_df %>%
  filter(str_starts(node, "NACE:")) %>%
  transmute(nace_join_key = str_remove(node, "NACE:"), comp_id, comp_size)

comp_labels <- mapping %>%
  left_join(nace_component %>% select(nace_join_key, comp_id), by = "nace_join_key") %>%
  group_by(comp_id) %>%
  summarise(combined_sector_name = paste(sort(unique(nace_level_3_label)),
                                         collapse = " + "),
            .groups = "drop")

# ===========================================================================
# 4. DIE VIER VARIANTEN — alle dynamisch aus denselben Daten 
# ===========================================================================

# Hilfsfunktion: Lücken-Zellen entfernen und in Mio. EUR umrechnen
finalize <- function(df, sector_col) {
  df %>%
    filter(!coalesce(has_reporting_gap, FALSE)) %>%   # [Kritik 2]
    mutate(ets_net_cost_eur_mio = coalesce(ets_net_cost_eur, 0) / 1e6,
           turnover_eur_mio = turnover_eur / 1e6) %>%
    rename(sector = all_of(sector_col))
}

#  Variante A: naiver Join (Referenz!) 
cost_A <- net_cost %>%
  left_join(mapping, by = "ets_activity_code",
            relationship = "many-to-many")            

data_A <- turnover %>%
  left_join(cost_A %>% select(country_code, year, nace_join_key, ets_net_cost_eur),
            by = c("country_code", "year", "nace_join_key"),
            relationship = "many-to-many") %>%
  left_join(gap_nace, by = c("country_code", "year", "nace_join_key")) %>%
  finalize("nace_category_name")

#  Variante B: Gleichgewichtung (Kosten / n gemappte Sektoren) 
mapping_B <- mapping %>%
  add_count(ets_activity_code, name = "n_mapped") %>%
  mutate(cost_weight = 1 / n_mapped)

cost_B <- net_cost %>%
  left_join(mapping_B, by = "ets_activity_code",
            relationship = "many-to-many") %>%
  mutate(ets_net_cost_eur = ets_net_cost_eur * cost_weight)

data_B <- turnover %>%
  left_join(cost_B %>% select(country_code, year, nace_join_key, ets_net_cost_eur),
            by = c("country_code", "year", "nace_join_key"),
            relationship = "many-to-many") %>%
  left_join(gap_nace, by = c("country_code", "year", "nace_join_key")) %>%
  finalize("nace_category_name")

#  Variante C: Aggregation über Graph-Komponenten  
turnover_C <- turnover %>%
  left_join(nace_component %>% select(nace_join_key, comp_id), by = "nace_join_key") %>%
  left_join(comp_labels, by = "comp_id") %>%
  mutate(modeling_sector = coalesce(combined_sector_name, nace_category_name)) %>%
  left_join(gap_nace, by = c("country_code", "year", "nace_join_key")) %>%
  group_by(country_code, year, modeling_sector) %>%
  summarise(turnover_eur = sum(turnover_eur),
            has_reporting_gap = any(coalesce(has_reporting_gap, FALSE)),
            .groups = "drop")

cost_C <- net_cost %>%
  inner_join(ets_component %>% select(ets_activity_code, comp_id), by = "ets_activity_code") %>%
  left_join(comp_labels, by = "comp_id") %>%
  group_by(country_code, year, modeling_sector = combined_sector_name) %>%
  summarise(ets_net_cost_eur = sum(ets_net_cost_eur), .groups = "drop")

data_C <- turnover_C %>%
  left_join(cost_C, by = c("country_code", "year", "modeling_sector")) %>%
  finalize("modeling_sector")

# --- Variante D: nur echte 1:1-Paare (2-Knoten-Komponenten) ----------------
ambiguous_nace <- nace_component %>% filter(comp_size > 2) %>% pull(nace_join_key)
ambiguous_ets  <- ets_component  %>% filter(comp_size > 2) %>% pull(ets_activity_code)

cost_D <- net_cost %>%
  filter(!(ets_activity_code %in% ambiguous_ets)) %>%
  left_join(mapping %>% distinct(ets_activity_code, nace_join_key),
            by = "ets_activity_code") %>%
  filter(!is.na(nace_join_key))

data_D <- turnover %>%
  filter(!(nace_join_key %in% ambiguous_nace)) %>%
  left_join(cost_D %>% select(country_code, year, nace_join_key, ets_net_cost_eur),
            by = c("country_code", "year", "nace_join_key")) %>%
  left_join(gap_nace, by = c("country_code", "year", "nace_join_key")) %>%
  finalize("nace_category_name")

# ===========================================================================
# 5. SCHÄTZUNG mit fixest [3]
#    Identisches Modell je Variante. Standardfehler werden auf VIER Ebenen
#    ausgewiesen: klassisch (iid), geclustert nach Sektor, nach Land und
#    zweifach (Land + Sektor). Zweiwege-Clustering kann bei bestimmten
#    Datenstrukturen numerisch scheitern (nicht-positive Varianzmatrix) —
#    das wird abgefangen und als NA ausgewiesen statt das Skript zu stoppen.
# ===========================================================================

p_of <- function(m, clus = NULL) {
  ct <- tryCatch({
    if (is.null(clus)) summary(m, se = "iid")$coeftable
    else summary(m, cluster = clus)$coeftable
  }, error = function(e) NULL)
  if (is.null(ct) || !"ets_net_cost_eur_mio" %in% rownames(ct)) return(c(NA, NA))
  out <- ct["ets_net_cost_eur_mio", c("Std. Error", "Pr(>|t|)")]
  if (any(!is.finite(out))) return(c(NA, NA))
  unname(out)
}

fit_variant <- function(df, label) {
  m <- feols(log(turnover_eur_mio) ~ ets_net_cost_eur_mio |
               sector + country_code + year,
             data = df)

  iid  <- p_of(m)
  sek  <- p_of(m, ~ sector)
  land <- p_of(m, ~ country_code)
  zwei <- p_of(m, ~ country_code + sector)

  tibble(
    variante     = label,
    beta1        = unname(coef(m)["ets_net_cost_eur_mio"]),
    p_iid        = iid[2],
    p_cl_sektor  = sek[2],
    p_cl_land    = land[2],
    p_cl_zweiweg = zwei[2],
    n            = nobs(m),
    r2           = r2(m, type = "r2")
  )
}

vergleich <- bind_rows(
  fit_variant(data_A, "A: Original (Doppelzählung, Referenz)"),
  fit_variant(data_B, "B: Gleichgewichtung (1/n)"),
  fit_variant(data_C, "C: Aggregation (Graph-Komponenten)"),
  fit_variant(data_D, "D: Nur echte 1:1-Paare")
)

cat("\n=== VERGLEICH ALLER VARIANTEN (iid vs. geclusterte SE) ===\n\n")
print(as.data.frame(vergleich), digits = 3, row.names = FALSE)

cat("\nLesehilfe:\n")
cat("- beta1 ist je Variante identisch, egal welche SE — Clustering ändert nur SE/p.\n")
cat("- Die p-Werte können sich je Cluster-Ebene STARK unterscheiden (auch in beide\n")
cat("  Richtungen). Das ist selbst ein Befund: die Inferenz ist nicht stabil.\n")
cat("- Konservative Praxis für die Arbeit: die Ebene mit dem GRÖSSTEN p-Wert\n")
cat("  berichten (bzw. mehrere offenlegen) und die Wahl begründen.\n")
cat("- NA bei p_cl_zweiweg = Zweiwege-Clustering numerisch nicht berechenbar\n")
cat("  (bekanntes Problem, kein Fehler im Skript).\n")

# ===========================================================================
# 6. EXPORT
# ===========================================================================
write_xlsx(
  list(
    vergleich       = vergleich,
    berichtsluecken = reporting_gaps,
    komponenten     = comp_df %>% arrange(comp_id, node),
    daten_variante_C = data_C
  ),
  "data/robustness_variants_v2.xlsx"
)
cat("\nGespeichert: data/robustness_variants_v2.xlsx\n")
