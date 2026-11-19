
# ============================================================
# PACHIJAL / MIRA ARR LEAKAGE COMPARISON
#
# Six registries:
# Verra VM0047/VMD0054, SOCIALCARBON, Isometric,
# Gold Standard, Equitable Earth, CAR
#
# ============================================================

library(tidyverse)
library(sf)
library(terra)

# ============================================================
# 0. CONFIGURATION
# ============================================================

kml_path <- "PATH TO PROJ KML"

esa_crop_path <- "PATH TO ESA DATA"
  
esa_tiles <- file.path(
  "/Users/alexthompson/Downloads",
  c("PATH TO FIRST ESA TILE", "PATH TO SECOND ESA TILE"
  )
)

output_dir <- "outputs"
dir.create(output_dir, recursive = TRUE,
           showWarnings = FALSE)

buffer_km <- c(5, 10, 20)
selected_buffer_km <- 5

# ESA AGB in Mg dry biomass/ha
carbon_fraction <- 0.47

# IPCC 2019 tropical rainforest / South America
# Conditional ecological classification
root_shoot_ratio <- 0.221

# ============================================================
# 1. PROJECT DATA
# ============================================================

project <- list(
  name = "Pachijal and Mira Watersheds",
  country = "Ecuador",
  start_year = 2011,
  area_ha = 383,
  grazing_area_ha = 157.5,
  stocking_head_ha = 1,
  exempt_area_ha = 50,
  original_leakage_tco2e = 0,
  project_removals_20yr = 179034.2,
  baseline_removals_20yr = 9812,
  net_removals_20yr = 169222
)

receiving_areas <- tibble(
  site = c("Santa Rosa", "La Yumbada"),
  available_pasture_ha = c(100, 60),
  excluded_planting_ha = c(0, 14.3),
  capacity_head_ha = c(1, 1),
  evidence = "2011 PDD landowner interviews"
) |>
  mutate(
    net_pasture_ha = pmax(
      available_pasture_ha - excluded_planting_ha,
      0
    ),
    capacity_heads = net_pasture_ha *
      capacity_head_ha
  )

# ============================================================
# 2. ORIGINAL LEAKAGE RECONSTRUCTION
# ============================================================

total_displaced_heads <-
  project$grazing_area_ha *
  project$stocking_head_ha

assessed_area_ha <- max(
  project$grazing_area_ha - project$exempt_area_ha,
  0
)

required_heads <-
  assessed_area_ha * project$stocking_head_ha

available_heads <- sum(receiving_areas$capacity_heads)

original <- data.frame(
  project = project$name,
  displaced_heads = total_displaced_heads,
  assessed_area_ha = assessed_area_ha,
  required_heads = required_heads,
  available_heads = available_heads,
  capacity_margin = available_heads - required_heads,
  original_screen_passes =
    available_heads >= required_heads,
  reported_leakage_tco2e =
    project$original_leakage_tco2e
)

print(original)

stopifnot(
  isTRUE(all.equal(assessed_area_ha, 107.5)),
  original$original_screen_passes
)

write_csv(
  original,
  file.path(output_dir, "original_reconstruction.csv")
)

# ============================================================
# 3. REFERENCE CARBON EVIDENCE
# ============================================================

# Ecuador National Forest Evaluation.
# These are regional forest carbon references,
# not measured leakage-conversion emission factors.

forest_reference <- tibble(
  forest_type = c(
    "Andean montane evergreen",
    "Andean foothills evergreen",
    "Andean high-mountain evergreen"
  ),
  biomass_tc_ha = c(123.11, 122.77, 105.10),
  tree_tc_ha = c(80.89, 72.94, 61.22),
  root_tc_ha = c(19.41, 17.51, 14.69)
) |>
  mutate(
    tree_root_tc_ha = tree_tc_ha + root_tc_ha
  )

soc_reference <- crossing(
  climate = c("Tropical montane", "Tropical wet"),
  soil = c(
    "Volcanic",
    "High activity clay",
    "Low activity clay"
  )
) |>
  mutate(
    soc_ref_tc_ha = case_when(
      climate == "Tropical montane" &
        soil == "Volcanic" ~ 96,
      climate == "Tropical montane" &
        soil == "High activity clay" ~ 51,
      climate == "Tropical montane" &
        soil == "Low activity clay" ~ 44,
      climate == "Tropical wet" &
        soil == "Volcanic" ~ 77,
      climate == "Tropical wet" &
        soil == "High activity clay" ~ 60,
      TRUE ~ 52
    )
  )

soc_stock_loss <- function(ref, fLU, fMG, fIN) {
  ref * (1 - fLU * fMG * fIN)
}

# ============================================================
# 4. ISOMETRIC HISTORICAL GROWTH EVIDENCE
# ============================================================

# Ecuador national cattle and pasture data.
# Source: SIPAE (2007), MAG/INEC/SICA series.
#
# IMPORTANT:
# Cattle/ha is a stocking-density proxy, not
# verified cattle-product output per hectare.
#
# The historical window is 1990–2005 (15 years),
# preceding the 2011 project start.

historical_yield <- tibble(
  year = c(1990L, 2005L),
  cattle_head = c(4359000, 5283109),
  pasture_ha = c(4921200, 5480500)
) |>
  mutate(
    stocking_head_ha = cattle_head / pasture_ha
  )

calculate_iso_GR <- function(
    yield_recent,
    yield_historical,
    year_recent,
    year_historical) {
  
  years <- year_recent - year_historical
  
  stopifnot(
    years >= 10,
    yield_recent > 0,
    yield_historical > 0
  )
  
  max(
    (yield_recent / yield_historical) ^
      (1 / years) - 1,
    0
  )
}

iso_GR_proxy <- calculate_iso_GR(
  yield_recent = historical_yield$stocking_head_ha[2],
  yield_historical = historical_yield$stocking_head_ha[1],
  year_recent = 2005,
  year_historical = 1990
)

message(
  sprintf(
    "Isometric GR proxy: %.3f%% annually",
    100 * iso_GR_proxy
  )
)

write_csv(
  historical_yield,
  file.path(
    output_dir,
    "ecuador_historical_stocking_1990_2005.csv"
  )
)

# ============================================================
# 5. METHODOLOGY ASSUMPTIONS
# ============================================================

assumptions <- list(
  # Regional carbon references
  biomass_tc_ha = 123.11,
  tree_root_tc_ha = 100.30,
  
  # Conditional SOC assumptions
  soc_ref_tc_ha = 96,
  fLU = 1,
  fMG = 1,
  fIN = 1,
  
  # Receiving grazing capacity
  replacement_yield_head_ha = 1,
  
  # Verra / SOCIALCARBON
  yield_growth = iso_GR_proxy,
  agricultural_IS = 0.75,
  agricultural_NL = 0.40,
  
  # Isometric Appendix A
  iso_supply_elasticity = 0.40,
  iso_demand_elasticity = -0.40,
  iso_IS = 0.50,
  
  # Brazil livestock proxy, NOT Ecuador default
  iso_NL = 0.89,
  
  # Ecuador historical stocking proxy
  iso_GR = iso_GR_proxy,
  
  # CAR
  au_per_head = 1,
  grazing_months = 12,
  canopy_cover_pct = 75,
  car_commercial_verified = FALSE,
  
  # Mitigation emissions
  mitigation_emissions_tco2e = 0
)

assumptions$soc_loss_tc_ha <- with(
  assumptions,
  soc_stock_loss(soc_ref_tc_ha, fLU, fMG, fIN)
)

# ============================================================
# 6. READ PROJECT KML
# ============================================================

read_project_kml <- function(path) {
  
  if (!file.exists(path)) {
    stop("KML not found: ", path)
  }
  
  layers <- st_layers(path)$name
  print(layers)
  
  out <- lapply(layers, function(layer_name) {
    
    x <- tryCatch(
      st_read(path, layer = layer_name, quiet = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(x) || nrow(x) == 0) {
      return(NULL)
    }
    
    x <- st_make_valid(x)
    
    types <- as.character(st_geometry_type(x))
    
    keep <- types %in% c(
      "POLYGON",
      "MULTIPOLYGON",
      "GEOMETRYCOLLECTION"
    )
    
    x <- x[keep, ]
    
    if (nrow(x) == 0) {
      message("Skipping nonpolygon layer: ", layer_name)
      return(NULL)
    }
    
    is_collection <-
      as.character(st_geometry_type(x)) ==
      "GEOMETRYCOLLECTION"
    
    if (any(is_collection)) {
      
      x_collection <- suppressWarnings(
        st_collection_extract(
          x[is_collection, ],
          "POLYGON"
        )
      )
      
      x <- rbind(
        x[!is_collection, ],
        x_collection
      )
    }
    
    x <- x[!st_is_empty(x), ]
    
    if (!nrow(x)) return(NULL)
    
    feature_name <- if ("Name" %in% names(x)) {
      as.character(x$Name)
    } else {
      rep(NA_character_, nrow(x))
    }
    
    st_sf(
      source_layer = rep(layer_name, nrow(x)),
      placemark_name = feature_name,
      geometry = st_geometry(x)
    )
  })
  
  out <- Filter(Negate(is.null), out)
  
  if (!length(out)) {
    stop("No polygon features found in KML.")
  }
  
  bind_rows(out) |>
    st_transform(6933) |>
    st_make_valid()
}

polygons <- read_project_kml(kml_path)

# ============================================================
# 7. RECONCILE PROJECT POLYGONS
# ============================================================

polygons <- polygons |>
  mutate(
    area_ha = as.numeric(st_area(geometry)) / 10000,
    search_label = paste(source_layer, placemark_name),
    candidate_planting = !str_detect(
      str_to_lower(search_label),
      "expansion|noeleg|ineligible|\\(#2\\)"
    )
  )

layer_summary <- polygons |>
  st_drop_geometry() |>
  group_by(source_layer) |>
  summarise(
    n_features = n(),
    summed_area_ha = sum(area_ha),
    candidate = any(candidate_planting),
    .groups = "drop"
  )

planting_candidates <- polygons |>
  filter(candidate_planting)

project_geometry <- planting_candidates |>
  summarise(
    geometry = st_union(geometry)
  ) |>
  st_make_valid()

candidate_area_ha <- as.numeric(
  st_area(project_geometry)
) / 10000

area_reconciliation <- tibble(
  pdd_area_ha = project$area_ha,
  kml_area_ha = candidate_area_ha,
  missing_area_ha = project$area_ha - candidate_area_ha
)

print(area_reconciliation)

write_csv(
  layer_summary,
  file.path(output_dir, "kml_layer_reconciliation.csv")
)

write_csv(
  area_reconciliation,
  file.path(output_dir, "kml_area_reconciliation.csv")
)

gpkg_path <- file.path(
  output_dir, "pachijal_geometry.gpkg"
)

if (file.exists(gpkg_path)) {
  file.remove(gpkg_path)
}

st_write(
  polygons,
  gpkg_path,
  layer = "all_polygons",
  quiet = TRUE
)

st_write(
  project_geometry,
  gpkg_path,
  layer = "candidate_planting",
  quiet = TRUE
)

# ============================================================
# 8. MAP PROJECT
# ============================================================

p_boundary <- ggplot() +
  geom_sf(
    data = planting_candidates,
    aes(fill = source_layer),
    color = "grey25",
    linewidth = 0.2,
    alpha = 0.75
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Pachijal ARR project",
    subtitle = paste0(
      "KML: ", round(candidate_area_ha, 1),
      " ha | PDD: 383 ha"
    ),
    fill = "Polygon group"
  ) +
  theme_minimal(base_size = 12)

print(p_boundary)

ggsave(
  file.path(output_dir, "project_boundary.png"),
  p_boundary,
  width = 9,
  height = 7,
  dpi = 300
)

# ============================================================
# 9. EXPLORATORY BUFFER GEOGRAPHIES
# ============================================================

parcel_groups <- planting_candidates |>
  group_by(source_layer) |>
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  st_transform(32617) |>
  st_make_valid()

project_utm <- st_transform(
  project_geometry, 32617
)

buffers <- map_dfr(buffer_km, function(km) {
  
  b <- st_buffer(
    parcel_groups,
    dist = km * 1000
  )
  
  b <- st_difference(
    b,
    st_union(st_geometry(project_utm))
  )
  
  b |>
    mutate(
      buffer_km = km,
      buffer_id = paste0(source_layer, "_", km, "km")
    )
})

st_write(
  buffers,
  gpkg_path,
  layer = "exploratory_buffers",
  quiet = TRUE
)

# ============================================================
# 10. LOAD ESA 2010 AGB
# ============================================================

# Use processed crop if available.
# Otherwise crop and mosaic the two original tiles.

if (!file.exists(esa_crop_path) &&
    all(file.exists(esa_tiles))) {
  
  extent_geom <- st_transform(buffers, 4326)
  bb <- st_bbox(extent_geom)
  
  crop_extent <- terra::ext(
    unname(bb["xmin"]) - 0.01,
    unname(bb["xmax"]) + 0.01,
    unname(bb["ymin"]) - 0.01,
    unname(bb["ymax"]) + 0.01
  )
  
  pieces <- lapply(esa_tiles, function(path) {
    
    r <- rast(path)
    
    terra::crop(
      r,
      crop_extent,
      snap = "out"
    )
  })
  
  agb_mosaic <- do.call(
    terra::mosaic,
    pieces
  )
  
  writeRaster(
    agb_mosaic,
    esa_crop_path,
    overwrite = TRUE,
    NAflag = 65535
  )
}

spatial_carbon <- NULL
pooled_carbon <- NULL

if (file.exists(esa_crop_path)) {
  
  agb <- rast(esa_crop_path)
  
  if (nlyr(agb) != 1) {
    stop("Expected a single ESA AGB layer.")
  }
  
  # Exclude ESA missing-data sentinel.
  agb <- terra::ifel(
    agb >= 65535 | agb < 0,
    NA,
    agb
  )
  
  names(agb) <- "agb_mg_dm_ha"
  
  # Biomass -> carbon
  agb_carbon <- agb * carbon_fraction
  
  bgb_carbon <- agb_carbon * root_shoot_ratio
  
  living_carbon_tc <- agb_carbon + bgb_carbon
  
  living_carbon_co2 <-
    living_carbon_tc * (44 / 12)
  
  names(living_carbon_co2) <-
    "living_carbon_tco2_ha"
  
  writeRaster(
    living_carbon_co2,
    file.path(
      output_dir,
      "esa_living_carbon_2010.tif"
    ),
    overwrite = TRUE
  )
  
  # ==========================================================
  # 11. CARBON EXTRACTION HELPER
  # ==========================================================
  
  extract_stats <- function(r, geom) {
    
    g <- st_transform(
      geom,
      crs(r)
    )
    
    values <- terra::extract(
      r,
      vect(g),
      touches = TRUE
    )[[2]]
    
    values <- values[is.finite(values)]
    
    if (!length(values)) {
      return(tibble(
        n_cells = 0L,
        mean = NA_real_,
        median = NA_real_,
        p75 = NA_real_,
        p90 = NA_real_,
        p95 = NA_real_
      ))
    }
    
    tibble(
      n_cells = length(values),
      mean = mean(values),
      median = median(values),
      p75 = as.numeric(quantile(values, 0.75)),
      p90 = as.numeric(quantile(values, 0.90)),
      p95 = as.numeric(quantile(values, 0.95))
    )
  }
  
  # ==========================================================
  # 12. CARBON BY PARCEL GROUP
  # ==========================================================
  
  spatial_carbon <- map_dfr(
    seq_len(nrow(buffers)),
    function(i) {
      
      stats <- extract_stats(
        living_carbon_co2,
        buffers[i, ]
      )
      
      tibble(
        buffer_id = buffers$buffer_id[i],
        source_layer = buffers$source_layer[i],
        buffer_km = buffers$buffer_km[i]
      ) |>
        bind_cols(stats)
    }
  )
  
  spatial_carbon <- spatial_carbon |>
    rename(p95_carbon_tco2_ha = p95)
  
  write_csv(
    spatial_carbon,
    file.path(output_dir, "esa_carbon_by_buffer.csv")
  )
  
  # ==========================================================
  # 13. POOLED CARBON DISTRIBUTION
  # ==========================================================
  
  pooled_buffers <- buffers |>
    group_by(buffer_km) |>
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    )
  
  pooled_carbon <- map_dfr(
    seq_len(nrow(pooled_buffers)),
    function(i) {
      
      stats <- extract_stats(
        living_carbon_co2,
        pooled_buffers[i, ]
      )
      
      tibble(
        buffer_km = pooled_buffers$buffer_km[i]
      ) |>
        bind_cols(stats)
    }
  ) |>
    rename(p95_carbon_tco2_ha = p95)
  
  print(pooled_carbon)
  
  write_csv(
    pooled_carbon,
    file.path(
      output_dir,
      "esa_carbon_pooled_buffers.csv"
    )
  )
  
  # Biomass map
  png(
    file.path(output_dir, "esa_agb_map.png"),
    width = 1800,
    height = 1400,
    res = 180
  )
  
  plot(
    agb,
    main = "Pachijal: ESA 2010 aboveground biomass"
  )
  
  plot(
    vect(st_transform(
      planting_candidates,
      crs(agb)
    )),
    add = TRUE,
    border = "black",
    lwd = 1
  )
  
  dev.off()
  
} else {
  
  message(
    "ESA raster not found. Spatial leakage unavailable."
  )
}

# ============================================================
# 14. SHARED COMMODITY FUNCTIONS
# ============================================================

p <- list(
  grazing_area_ha = project$grazing_area_ha,
  stocking_head_ha = project$stocking_head_ha,
  alternative_pasture_ha =
    sum(receiving_areas$net_pasture_ha)
)

baseline_heads <- function(p) {
  p$grazing_area_ha * p$stocking_head_ha
}

qualified_mitigation <- function(
    p,
    formal = FALSE) {
  
  if (!formal) return(0)
  
  # Conditional additional production.
  # NOT verified as qualifying mitigation.
  min(
    p$alternative_pasture_ha *
      p$stocking_head_ha,
    baseline_heads(p)
  )
}

# ============================================================
# 15. VERRA VM0047 / VMD0054 v1.1
# ============================================================

verra_vmd0054 <- function(
    p, a, formal = FALSE, year = 5) {
  
  BP <- baseline_heads(p) *
    (1 + a$yield_growth)^year
  
  LM <- qualified_mitigation(p, formal)
  
  displaced <- max(BP - LM, 0)
  
  conversion_area <- displaced *
    a$agricultural_IS *
    a$agricultural_NL /
    a$replacement_yield_head_ha
  
  carbon_loss <- a$biomass_tc_ha +
    a$soc_loss_tc_ha
  
  tibble(
    registry = "Verra",
    methodology = "VM0047 / VMD0054 v1.1",
    baseline_production = BP,
    mitigation_production = LM,
    displaced_production = displaced,
    conversion_area_ha = conversion_area,
    carbon_loss_tc_ha = carbon_loss,
    leakage_tco2e =
      conversion_area * carbon_loss * 44 / 12,
    status = "Simplified single-commodity screening"
  )
}

# ============================================================
# 16. SOCIALCARBON SCD0002 v1.0
# ============================================================

socialcarbon_scd0002 <- function(
    p, a, formal = FALSE, year = 5) {
  
  stopifnot(year %in% 1:5)
  
  # Equations 1-2
  BP <- baseline_heads(p) *
    (1 + a$yield_growth)^year
  
  FP <- BP
  
  # Equations 3-5
  LM <- qualified_mitigation(p, formal)
  
  l <- max(FP - LM, 0)
  
  # Equations 6-7
  AL <- l *
    a$agricultural_IS *
    a$agricultural_NL /
    a$replacement_yield_head_ha
  
  # Equations 8-10
  CS <- a$biomass_tc_ha +
    a$soc_loss_tc_ha
  
  LK <- AL * CS * 44 / 12
  
  tibble(
    registry = "SOCIALCARBON",
    methodology = "SCM0009 / SCD0002 v1.0",
    baseline_production = BP,
    mitigation_production = LM,
    displaced_production = l,
    conversion_area_ha = AL,
    carbon_loss_tc_ha = CS,
    leakage_tco2e =
      LK + a$mitigation_emissions_tco2e,
    status = "Core equations; conditional mitigation"
  )
}

# ============================================================
# 17. ISOMETRIC REFORESTATION v1.2
# ============================================================

# Uses:
# IS = 0.50, South American livestock
# NL = 0.83, Brazilian livestock proxy
# GR = historical Ecuador stocking-density proxy

isometric_market <- function(
    p, a, formal = FALSE) {
  
  PPP <- baseline_heads(p)
  
  MAP <- qualified_mitigation(p, formal)
  
  uPPP <- max(PPP - MAP, 0)
  
  # Isometric Eq. 6
  aPPP <- uPPP * (1 + a$iso_GR)
  
  # Induced land conversion
  conversion_area <- aPPP *
    a$iso_IS *
    a$iso_NL /
    a$replacement_yield_head_ha
  
  EF <- a$biomass_tc_ha * 44 / 12
  
  tibble(
    registry = "Isometric",
    methodology = "Reforestation v1.2",
    baseline_production = PPP,
    mitigation_production = MAP,
    displaced_production = aPPP,
    conversion_area_ha = conversion_area,
    carbon_loss_tc_ha = a$biomass_tc_ha,
    leakage_tco2e = conversion_area * EF,
    status = paste(
      "South America IS; Brazil NL proxy;",
      "Ecuador historical stocking GR proxy"
    )
  )
}

# ============================================================
# 18. CLIMATE ACTION RESERVE
# ============================================================

car_leakage <- function(
    project,
    a,
    grazing_months = a$grazing_months) {
  
  stopifnot(
    grazing_months > 0,
    grazing_months <= 12
  )
  
  # AUM per acre
  aum_acre <-
    project$stocking_head_ha *
    a$au_per_head *
    grazing_months / 2.47105381
  
  if (aum_acre <= 0.5) {
    
    risk <- 0
    status <- "Below grazing threshold"
    
  } else if (!a$car_commercial_verified) {
    
    risk <- NA_real_
    status <- "Commercial viability unresolved"
    
  } else {
    
    canopy <- a$canopy_cover_pct
    
    risk <- case_when(
      canopy < 30 ~ 0,
      canopy < 40 ~ 0.10,
      canopy < 50 ~ 0.20,
      canopy < 60 ~ 0.30,
      canopy < 70 ~ 0.40,
      TRUE ~ 0.50
    )
    
    status <- "Conditional commercial grazing scenario"
  }
  
  # Provisional use of PDD net removals.
  # CAR-specific eligible carbon stock accounting
  # still needs reconciliation.
  
  leakage <- risk * project$net_removals_20yr
  
  tibble(
    registry = "CAR",
    methodology = "U.S. Forest Protocol v5.1",
    baseline_production = NA_real_,
    mitigation_production = NA_real_,
    displaced_production = NA_real_,
    conversion_area_ha = NA_real_,
    carbon_loss_tc_ha = NA_real_,
    leakage_tco2e = leakage,
    aum_per_acre = aum_acre,
    leakage_risk_factor = risk,
    status = status
  )
}

# ============================================================
# 19. GOLD STANDARD A/R v2.1
# ============================================================

# Single lower-bound capacity scenario:
#
# 157.5 head displaced
# 145.7 head conditionally accommodated
# 11.8 head potentially affecting forest
#
# Assumes 1 head/ha receiving carrying capacity.
# This is NOT a Gold Standard prescribed default.

gold_standard <- function(
    p,
    a,
    accommodation_heads = 145.7,
    receiving_yield_head_ha =
      a$replacement_yield_head_ha,
    residual_forest_impact_fraction = 1) {
  
  stopifnot(
    receiving_yield_head_ha > 0,
    accommodation_heads >= 0,
    residual_forest_impact_fraction >= 0,
    residual_forest_impact_fraction <= 1
  )
  
  total_heads <- baseline_heads(p)
  
  accommodated <- min(
    total_heads,
    accommodation_heads
  )
  
  residual_heads <- max(
    total_heads - accommodated,
    0
  )
  
  affected_heads <- residual_heads *
    residual_forest_impact_fraction
  
  affected_area <- affected_heads /
    receiving_yield_head_ha
  
  leakage <- affected_area *
    a$tree_root_tc_ha * 44 / 12
  
  tibble(
    registry = "Gold Standard",
    methodology = "A/R v2.1",
    baseline_production = total_heads,
    mitigation_production = accommodated,
    displaced_production = affected_heads,
    conversion_area_ha = affected_area,
    carbon_loss_tc_ha = a$tree_root_tc_ha,
    leakage_tco2e = leakage,
    status = paste(
      "PDD lower-bound accommodation;",
      "all residual cattle assumed forest-affecting"
    )
  )
}

# ============================================================
# 20. EQUITABLE EARTH M001 v1.2
# ============================================================

# Unknown hosting-area exploratory pathway.
# P95 is prescribed by M001 v1.2.
# ESA is a substitute for the protocol's
# designated geospatial biomass source.

equitable_earth <- function(
    project,
    p95_tco2_ha = NA_real_,
    displaced_fraction = 1) {
  
  affected_area <- project$grazing_area_ha *
    displaced_fraction
  
  tibble(
    registry = "Equitable Earth",
    methodology = "M001 v1.2",
    baseline_production = NA_real_,
    mitigation_production = NA_real_,
    displaced_production = NA_real_,
    conversion_area_ha = affected_area,
    carbon_loss_tc_ha =
      p95_tco2_ha * 12 / 44,
    leakage_tco2e =
      affected_area * p95_tco2_ha,
    status = if_else(
      is.na(p95_tco2_ha),
      "Spatial estimate unavailable",
      "Exploratory ESA P95; not official leakage belt"
    )
  )
}

# ============================================================
# 21. EQUITABLE EARTH SPATIAL INPUT
# ============================================================

ee_p95 <- NA_real_

if (!is.null(pooled_carbon)) {
  
  selected <- pooled_carbon |>
    filter(buffer_km == selected_buffer_km)
  
  if (
    nrow(selected) > 0 &&
    is.finite(selected$p95_carbon_tco2_ha[1])
  ) {
    ee_p95 <- selected$p95_carbon_tco2_ha[1]
  }
}

message(
  "Equitable Earth exploratory P95: ",
  round(ee_p95, 2),
  " tCO2e/ha"
)

# ============================================================
# 22. RUN REGISTRY MODELS
# ============================================================

scenarios <- tibble(
  scenario = c(
    "No qualified mitigation",
    "Conditional verified accommodation"
  ),
  formal = c(FALSE, TRUE)
)

run_models <- function(s) {
  
  bind_rows(
    verra_vmd0054(
      p, assumptions, formal = s$formal
    ),
    
    socialcarbon_scd0002(
      p, assumptions, formal = s$formal
    ),
    
    isometric_market(
      p, assumptions, formal = s$formal
    ),
    
    car_leakage(
      project, assumptions
    ),
    
    # Gold Standard: single lower-bound
    # pasture-accommodation calculation.
    gold_standard(
      p, assumptions,
      accommodation_heads =
        sum(receiving_areas$capacity_heads),
      receiving_yield_head_ha =
        assumptions$replacement_yield_head_ha,
      residual_forest_impact_fraction = 1
    ),
    
    equitable_earth(
      project,
      p95_tco2_ha = ee_p95
    )
  ) |>
    mutate(
      scenario = s$scenario,
      .before = 1
    )
}

results <- map_dfr(
  seq_len(nrow(scenarios)),
  function(i) {
    run_models(as.list(scenarios[i, ]))
  }
) |>
  mutate(
    deduction_pct_net_removals =
      100 * leakage_tco2e /
      project$net_removals_20yr
  )

print(results, n = Inf, width = Inf)

write_csv(
  results,
  file.path(
    output_dir,
    "six_registry_comparison.csv"
  )
)

# ============================================================
# 23. PRESENTATION TABLE
# ============================================================

results_display <- results |>
  mutate(
    leakage_display = if_else(
      is.na(leakage_tco2e),
      "Unresolved",
      scales::comma(round(leakage_tco2e))
    ),
    conversion_area_display = if_else(
      is.na(conversion_area_ha),
      "N/A",
      sprintf("%.1f ha", conversion_area_ha)
    ),
    deduction_display = if_else(
      is.na(deduction_pct_net_removals),
      "Unresolved",
      sprintf("%.1f%%", deduction_pct_net_removals)
    )
  ) |>
  select(
    scenario,
    registry,
    leakage_display,
    conversion_area_display,
    deduction_display,
    status
  )

print(results_display, n = Inf, width = Inf)

# ============================================================
# 24. FIGURE 1 — LEAKAGE ESTIMATES
# ============================================================

res_raw_lkg <- results |>
  filter(
    scenario == "No qualified mitigation"
  )
p_leakage <- res_raw_lkg |>
  filter(!is.na(leakage_tco2e)) |>
  ggplot(
    aes(
      x = reorder(registry, -leakage_tco2e),
      y = leakage_tco2e
    )
  ) +
  geom_col(
    fill = "darkgreen",
    width = 0.65
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    x = NULL,
    y = "Ex-ante leakage estimate (tCO2e)",
    title = "ARR leakage accounting across registries",
    subtitle = paste(
      "Pachijal, Ecuador |",
      "Original reported leakage: 0 tCO2e"
    ),
    caption = paste(
      "*Equitable Earth uses ESA biomass map"
    )
  ) +
  theme_classic(base_size = 13)


print(p_leakage)

ggsave(
  file.path(
    output_dir,
    "figure1_leakage.png"
  ),
  p_leakage,
  width = 10,
  height = 6,
  dpi = 300
)



# ============================================================
# 25. FIGURE 2 — WHY LEAKAGE ESTIMATES DIFFER
# ============================================================

# Methodology interpretation table.
# Separate registry rules from project-specific assumptions.

methodology_drivers <- tribble(
  ~registry,
  ~leakage_mechanism,
  ~key_methodological_driver,
  ~pachijal_implementation,
  ~evidence_limitation,
  
  "Verra",
  "Market-mediated land conversion",
  "75% supply replacement; 40% new-land share",
  "Five-year production growth; regional forest carbon",
  "No qualifying mitigation assumed",
  
  "SOCIALCARBON",
  "Market-mediated land conversion",
  "75% supply replacement; 40% new-land share",
  "Five-year production growth; regional forest carbon",
  "Similar structure and defaults to Verra",
  
  "Isometric",
  "Commodity-specific market displacement",
  "50% supply replacement; 83% new-land proxy",
  "Ecuador GR proxy; Brazilian livestock NL proxy",
  "Ecuador-specific NL and yield evidence unresolved",
  
  "Gold Standard",
  "Activity shifting affecting tree biomass",
  "Only livestock expected to affect external trees",
  "145.7 accommodated; 11.8 residual cattle",
  "Receiving capacity and forest exposure conditional",
  
  "Equitable Earth",
  "Spatial carbon exposure from displaced activity",
  "Unknown hosting area uses leakage-belt P95",
  "157.5 ha displaced; ESA-derived spatial P95",
  "Unknown destination and full displacement assumed"
)

# Join actual results from the primary scenario.

figure2 <- results |>
  filter(scenario == "No qualified mitigation") |>
  left_join(
    methodology_drivers,
    by = "registry"
  ) |>
  mutate(
    leakage_tco2e = round(leakage_tco2e),
    
    leakage_label = if_else(
      is.na(leakage_tco2e),
      "Unresolved",
      scales::comma(leakage_tco2e)
    ),
    
    deduction_label = if_else(
      is.na(deduction_pct_net_removals),
      "Unresolved",
      sprintf(
        "%.1f%%",
        deduction_pct_net_removals
      )
    )
  ) |>
  arrange(
    is.na(leakage_tco2e),
    leakage_tco2e
  ) |>
  select(
    registry,
    leakage_label,
    deduction_label,
    leakage_mechanism,
    key_methodological_driver,
    pachijal_implementation,
    evidence_limitation
  )

print(
  figure2,
  n = Inf,
  width = Inf
)

write_csv(
  figure2,
  file.path(
    output_dir,
    "figure2_why_leakage_differs.csv"
  )
)

# Optional publication-style table
# install.packages("gt") if needed

if (requireNamespace("gt", quietly = TRUE)) {
  
  figure2_gt <- figure2 |>
    filter(registry != "CAR") |>
    gt::gt() |>
    gt::cols_label(
      registry = "Registry",
      leakage_label = "Leakage (tCO2e)",
      deduction_label = "% of removals",
      leakage_mechanism = "Leakage mechanism",
      key_methodological_driver = "Methodology driver",
      pachijal_implementation = "Pachijal assumption",
      evidence_limitation = "Evidence limitation"
    ) |>
    gt::tab_header(
      title = "Why do ARR leakage estimates differ?",
      subtitle = paste(
        "Pachijal, Ecuador —",
        "counterfactual methodology comparison"
      )
    ) 
  
  print(figure2_gt)
  
  gt::gtsave(
    figure2_gt,
    file.path(
      output_dir,
      "figure2_why_leakage_differs.html"
    )
  )
}



# ============================================================
# 26. FIGURE 3 — EVIDENCE REQUIREMENTS
# ============================================================

# Provisional analytical comparison.
# Not formal registry-defined evidence tiers.

evidence_matrix <- tribble(
  ~registry,
  ~historical_production,
  ~market_parameters,
  ~hosting_area,
  ~spatial_monitoring,
  ~mitigation_evidence,
  
  "Verra",
  "Required",
  "Defaults or evidence",
  "Conditional",
  "Method-dependent",
  "Formal production evidence",
  
  "SOCIALCARBON",
  "Required",
  "Defaults or evidence",
  "Mitigation areas",
  "Production monitoring",
  "Contracts and production evidence",
  
  "Isometric",
  "Required",
  "Elasticities and land conversion",
  "Conditional",
  "Activity-shifting monitoring",
  "Formal production evidence",
  
  "Gold Standard",
  "Credible estimate or survey",
  "Not central",
  "Conditional",
  "No continuing parameter",
  "Evidence of tree-biomass exposure",
  
  "Equitable Earth",
  "Displaced activity",
  "No separate market model",
  "Known or unknown",
  "Spatial carbon monitoring",
  "Location and displacement evidence",
  
  "CAR",
  "Historical land use",
  "Prescribed risk factors",
  "Not required",
  "No dedicated leakage belt",
  "Grazing intensity and viability"
)

write_csv(
  evidence_matrix,
  file.path(
    output_dir,
    "figure3_evidence_requirements.csv"
  )
)

# ============================================================
# 27. SAVE INPUT EVIDENCE
# ============================================================

write_csv(
  forest_reference,
  file.path(
    output_dir,
    "forest_carbon_evidence.csv"
  )
)

write_csv(
  soc_reference,
  file.path(
    output_dir,
    "soc_reference_values.csv"
  )
)

write_csv(
  receiving_areas,
  file.path(
    output_dir,
    "receiving_pasture_evidence.csv"
  )
)

# ============================================================
# 28. DIAGNOSTICS
# ============================================================

message(
  "Gold Standard residual cattle: ",
  round(
    max(
      baseline_heads(p) -
        sum(receiving_areas$capacity_heads),
      0
    ),
    2
  )
)

message(
  "Isometric inputs: GR = ",
  signif(assumptions$iso_GR, 4),
  "; IS = ",
  assumptions$iso_IS,
  "; NL = ",
  assumptions$iso_NL
)

message("Analysis complete.")
message("Outputs: ", normalizePath(output_dir))
