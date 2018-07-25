

prop_cut <-
  tibble::tribble(
    ~unit_number, ~house_number, ~street_name, ~street_type, ~street_suffix,               ~suburb, ~postcode,          ~address_use_type,    ~ward_name,       ~property_description,     ~northing,     ~easting,
    NA,          "27",      "AARON",     "Avenue",             NA,           "HAWTHORNE",     4171L, "Uprn (Official Location)", "MORNINGSIDE",  "L.4 RP.55905 PAR BULIMBA",  "6961834.49",   "505697.5",
    NA,          "28",      "AARON",     "Avenue",             NA,           "HAWTHORNE",     4171L, "Uprn (Official Location)", "MORNINGSIDE",  "L.2 RP.87085 PAR BULIMBA",   "6961816.9",  "505648.29",
    NA,          "30",      "AARON",     "Avenue",             NA,           "HAWTHORNE",     4171L, "Uprn (Official Location)", "MORNINGSIDE",  "L.1 RP.87085 PAR BULIMBA",   "6961833.2",  "505644.55",
    NA,          "33",      "AARON",     "Avenue",             NA,           "HAWTHORNE",     4171L, "Uprn (Official Location)", "MORNINGSIDE",  "L.6 RP.55905 PAR BULIMBA", "6961879.153", "505678.232",
    NA,           "7",     "ABARTH",     "Street",             NA,           "CHERMSIDE",     4032L, "Uprn (Official Location)",    "MARCHANT",  "L.5 RP.118611 PAR KEDRON",  "6970343.22",  "503006.95",
    NA,           "9",     "ABARTH",     "Street",             NA,           "CHERMSIDE",     4032L, "Uprn (Official Location)",    "MARCHANT",  "L.6 RP.118611 PAR KEDRON",  "6970339.95",   "502986.9",
    NA,          "11",     "ABARTH",     "Street",             NA,           "CHERMSIDE",     4032L, "Uprn (Official Location)",    "MARCHANT",  "L.7 RP.118611 PAR KEDRON",  "6970318.72",  "502982.43",
    NA,          "16",  "HILLGROVE",     "Street",             NA, "UPPER MOUNT GRAVATT",     4122L,                "Alternate",   "MACGREGOR", "L.44 RP.87380 PAR BULIMBA", "6953349.984", "508671.277",
    NA,           "2",  "ABBEVILLE",     "Street",             NA, "UPPER MOUNT GRAVATT",     4122L, "Uprn (Official Location)",   "MACGREGOR", "L.44 RP.87380 PAR BULIMBA", "6953349.984", "508671.277",
    NA,          "22",  "HILLGROVE",     "Street",             NA, "UPPER MOUNT GRAVATT",     4122L,                "Alternate",   "MACGREGOR", "L.42 RP.87380 PAR BULIMBA",  "6953307.34",   "508664.2"
  )

bne_addresses <- as.data.frame(prop_cut)
save(bne_addresses, file = "inst/extdata/bne_addresses.rda")
