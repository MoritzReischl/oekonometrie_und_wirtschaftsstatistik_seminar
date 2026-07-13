library(dplyr)
library(readxl)
library(stringr)
library(openxlsx)

mapping_file <- normalizePath("data/1_raw/mapping ets nace/mapping_ets_nace_all_levels.xlsx", mustWork = TRUE)

# NACE codes are hierarchy identifiers, not numbers. Padding is necessary because
# Excel/readxl can otherwise turn, for example, "01" into "1".
normalise_nace_code <- function(code, level) {
  code <- str_trim(as.character(code))
  code[is.na(code) | code == ""] <- NA_character_
  numeric_code <- !is.na(code) & str_detect(code, "^[0-9]+$")
  width <- case_when(
    level == 2 ~ 2L,
    level == 3 ~ 3L,
    level == 4 ~ 4L,
    TRUE ~ NA_integer_
  )
  code[numeric_code & !is.na(width)] <- str_pad(
    code[numeric_code & !is.na(width)],
    width = width[numeric_code & !is.na(width)],
    pad = "0"
  )
  code
}

nace_tree <- read_excel(mapping_file, sheet = "NACE All Levels") %>%
  transmute(
    nace_level = as.integer(level),
    nace_code = normalise_nace_code(ID, nace_level),
    nace_label = name_clean,
    parent_code = str_trim(as.character(parent_code))
  ) %>%
  filter(!is.na(nace_code)) %>%
  distinct(nace_code, .keep_all = TRUE)

mappingetsnace <- read_excel(mapping_file, sheet = "Mapping")

mapped_nodes <- mappingetsnace %>%
  filter(!is.na(ets_activity_code), str_trim(as.character(ets_activity_code)) != "") %>%
  transmute(
    nace_level = as.integer(nace_level),
    nace_code = normalise_nace_code(nace_code, nace_level)
  ) %>%
  distinct() %>%
  inner_join(nace_tree, by = c("nace_level", "nace_code"))

parent_of <- setNames(nace_tree$parent_code, nace_tree$nace_code)

ancestors_of <- function(code) {
  result <- character()
  parent <- unname(parent_of[code])
  while (!is.null(parent) && !is.na(parent) && parent != "" &&
         !parent %in% result) {
    result <- c(result, parent)
    parent <- unname(parent_of[parent])
  }
  result
}

mapped_codes <- unique(mapped_nodes$nace_code)
mapped_ancestors <- unique(unlist(lapply(mapped_codes, ancestors_of)))

section_of <- function(code) {
  lineage <- c(code, ancestors_of(code))
  section <- lineage[str_detect(lineage, "^[A-Z]$")]
  if (length(section) == 0) NA_character_ else section[[1]]
}

# A candidate is hierarchy-safe only if it is neither a mapped node, an
# ancestor of one, nor a descendant of one.
has_mapped_ancestor <- function(code) {
  any(ancestors_of(code) %in% mapped_codes)
}

is_hierarchy_safe <- function(code) {
  !code %in% mapped_codes &&
    !code %in% mapped_ancestors &&
    !has_mapped_ancestor(code)
}

# Case 1: a mapped node fixes the granularity for its level-1 section. Append
# every other activity at that same NACE level in the section. For example, a
# mapping to division 11 in section C also selects divisions 10, 12, 13, ...,
# 33. Restricting the candidates to the mapped level keeps the resulting
# frontier mutually exclusive; selecting the whole section makes it exhaustive.
nodes_with_section <- nace_tree %>%
  mutate(section_code = vapply(nace_code, section_of, character(1)))

mapped_section_levels <- mapped_nodes %>%
  transmute(
    section_code = vapply(nace_code, section_of, character(1)),
    nace_level
  ) %>%
  filter(!is.na(section_code)) %>%
  distinct()

case_1_codes <- mapped_nodes %>%
  transmute(
    section_code = vapply(nace_code, section_of, character(1)),
    nace_level
  ) %>%
  distinct() %>%
  inner_join(nodes_with_section, by = c("section_code", "nace_level")) %>%
  filter(vapply(nace_code, is_hierarchy_safe, logical(1))) %>%
  pull(nace_code) %>%
  unique()

# Case 2: if an entire level-1 section contains no mapped node at any level
# (2, 3 or 4), represent that untouched section by all of its level-2
# activities only. That produces exhaustive coverage for the unmapped section
# without introducing parent/child overlap.
mapped_sections <- unique(vapply(mapped_codes, section_of, character(1)))
mapped_sections <- mapped_sections[!is.na(mapped_sections)]

unmapped_top_level_sections <- nace_tree %>%
  filter(nace_level == 2) %>%
  pull(parent_code) %>%
  unique() %>%
  setdiff(mapped_sections)

case_2_codes <- nace_tree %>%
  filter(
    nace_level == 2,
    parent_code %in% unmapped_top_level_sections,
    vapply(nace_code, is_hierarchy_safe, logical(1))
  ) %>%
  pull(nace_code) %>%
  unique()

append_nodes <- nace_tree %>%
  filter(nace_code %in% union(case_1_codes, case_2_codes)) %>%
  mutate(
    selection_case = if_else(nace_code %in% case_1_codes, 1L, 2L)
  ) %>%
  arrange(nace_level, nace_code)

has_hierarchy_overlap <- function(codes) {
  any(vapply(codes, function(code) {
    any(ancestors_of(code) %in% setdiff(codes, code))
  }, logical(1)))
}

# Multiple mapped levels in one section cannot simultaneously define one
# mutually exclusive and exhaustive frontier without changing existing rows.
ambiguous_sections <- mapped_section_levels %>%
  count(section_code, name = "mapped_level_count") %>%
  filter(mapped_level_count > 1L) %>%
  pull(section_code)

if (length(ambiguous_sections) > 0L) {
  stop(
    "Cannot construct a unique NACE frontier: these level-1 sections contain ",
    "mapped activities at multiple NACE levels: ",
    paste(ambiguous_sections, collapse = ", "),
    ". Harmonise their mapped levels first."
  )
}

if (has_hierarchy_overlap(mapped_codes)) {
  stop("The existing mapped NACE activities contain a parent/child overlap.")
}

# Build rows in exactly the Mapping sheet's existing column order. ETS fields
# deliberately remain empty because these are NACE-only coverage rows.
mapping_rows_to_append <- append_nodes %>%
  transmute(
    nace_level,
    nace_code,
    nace_label_original = nace_label,
    nace_code_agg = nace_code,
    nace_label_agg = nace_label,
    mapped_uniq_ets_cnt = 0L,
    cleaning_action = "Append unmapped NACE activity",
    ets_activity_code = NA_character_,
    ets_activity_name_original = NA_character_,
    ets_activity_code_agg = NA_character_,
    ets_activity_name_agg = NA_character_,
    mapped_uniq_nace_cnt = 1L,
    mapping_quality = "unmapped NACE-only",
    exclusivity_note = if_else(
      selection_case == 1L,
      "Unmapped sibling at the same level as a mapped NACE activity",
      "Level-2 activity from a section without any mapped NACE activity"
    )
  )

# Idempotence: rerunning the script does not duplicate a NACE-only row already
# present in the Mapping sheet.
existing_keys <- mappingetsnace %>%
  transmute(
    nace_level = as.integer(nace_level),
    nace_code = normalise_nace_code(nace_code, nace_level)
  ) %>%
  distinct()

mapping_rows_to_append <- mapping_rows_to_append %>%
  anti_join(existing_keys, by = c("nace_level", "nace_code"))

# Fail loudly if a future workbook change violates mutual exclusivity.
stopifnot(
  all(vapply(mapping_rows_to_append$nace_code, is_hierarchy_safe, logical(1))),
  all(mapping_rows_to_append$nace_level[mapping_rows_to_append$exclusivity_note ==
    "Level-2 activity from a section without any mapped NACE activity"] == 2L),
  !anyDuplicated(mapping_rows_to_append[c("nace_level", "nace_code")]),
  !has_hierarchy_overlap(c(mapped_codes, mapping_rows_to_append$nace_code))
)

mappingetsnace_extended <- bind_rows(mappingetsnace, mapping_rows_to_append)

message(
  "Prepared ", nrow(mapping_rows_to_append), " mutually exclusive NACE-only rows (",
  sum(append_nodes$nace_code %in% case_1_codes), " from Case 1; ",
  sum(append_nodes$nace_code %in% case_2_codes &
        !append_nodes$nace_code %in% case_1_codes), " from Case 2)."
)

# Load the original workbook rather than rebuilding it. The existing Mapping
# worksheet is never edited; the complete extended mapping is stored separately.
mapping_workbook <- openxlsx::loadWorkbook(mapping_file)
extended_sheet <- "Mapping All Nace Activities"
header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  border = "Bottom"
)

if (extended_sheet %in% names(mapping_workbook)) {
  openxlsx::removeWorksheet(mapping_workbook, extended_sheet)
}

openxlsx::addWorksheet(mapping_workbook, extended_sheet, gridLines = FALSE)

openxlsx::writeData(
  wb = mapping_workbook,
  sheet = extended_sheet,
  x = mappingetsnace_extended %>% select(all_of(names(mappingetsnace))),
  startCol = 1,
  startRow = 1,
  colNames = TRUE,
  rowNames = FALSE,
  keepNA = FALSE,
  withFilter = TRUE,
  headerStyle = header_style
)

openxlsx::freezePane(mapping_workbook, extended_sheet, firstActiveRow = 2)
openxlsx::setColWidths(
  mapping_workbook,
  extended_sheet,
  cols = seq_along(mappingetsnace_extended),
  widths = "auto"
)

openxlsx::saveWorkbook(
  wb = mapping_workbook,
  file = mapping_file,
  overwrite = TRUE
)

message(
  "Added/replaced worksheet '", extended_sheet, "' in: ", mapping_file,
  ". The original Mapping worksheet remains unchanged."
)
