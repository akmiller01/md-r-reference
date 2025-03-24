list.of.packages = c("jsonlite", "sf", "rlang", "httr", "dplyr")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))


create_query_string = function(named_list) {
  if (length(named_list) == 0) {
    return("")
  }
  
  encoded_pairs = sapply(names(named_list), function(name) {
    value = named_list[[name]]
    paste0(URLencode(name), "=", URLencode(as.character(value)))
  })
  
  return(paste(encoded_pairs, collapse = "&"))
}


md_find_address_candidates = function(
    SingleLine = NULL,
    Address = NULL,
    Address2 = NULL,
    Address3 = NULL,
    Neighborhood = NULL,
    City = NULL,
    Subregion = NULL,
    Region = NULL,
    Postal = NULL,
    PostalExt = NULL,
    CountryCode = NULL,
    maxLocations = NULL
){
  if(is.null(SingleLine) & is.null(Address)){
    stop("Either SingleLine or Address must be specified.")
  }
  url_base = "https://geodata.md.gov/imap/rest/services/GeocodeServices/MD_CompositeLocator/GeocodeServer/findAddressCandidates?f=json&"
  query_string = create_query_string(match.call()[-1])
  json_response = fromJSON(paste(url_base, query_string, sep="&"))
  latest_crs = json_response$spatialReference$latestWkid
  response_df = jsonlite::flatten(json_response$candidates)
  response_geo = st_as_sf(response_df, coords = c("location.x", "location.y"), crs = latest_crs)
  return(
    response_geo
  )
}


md_reverse_geocode = function(
    Location,
    Distance = NULL
){
  url_base = "https://geodata.md.gov/imap/rest/services/GeocodeServices/MD_CompositeLocator/GeocodeServer/reverseGeocode?f=json&"
  md_crs = 3857
  if(st_crs(Location) != md_crs){
    Location = st_transform(Location, crs = md_crs)
  }
  Location = paste(st_coordinates(Location), collapse=",")
  query_string = paste0(
    "Location=", Location,
    "&Distance=", Distance
  )
  json_response = fromJSON(paste(url_base, query_string, sep="&"))
  address = json_response$address
  address = address[!sapply(address, is.null)]
  return(data.frame(address))
}


address_to_json = function(object_ids, addresses) {
  if (length(object_ids) != length(addresses)) {
    stop("All input vectors must have the same length.")
  }
  records = lapply(addresses, function(address) {
    list(attributes = list(singleLine = address))
  })
  records = lapply(seq_along(addresses), function(i) {
    list(attributes = list(
      ObjectID = object_ids[i],
      singleLine = addresses[i]
    ))
  })
  
  result = list(records = records)
  json_string = toJSON(result, auto_unbox = TRUE)
  return(json_string)
}

address_to_json_multifield = function(ObjectIDs, Address, Address2, City, Postal) {
  if (length(Address) != length(Address2) || 
      length(Address) != length(City) || 
      length(Address) != length(Postal) ||
      length(Address) != length(ObjectIDs)) {
    stop("All input vectors must have the same length.")
  }
  
  records = lapply(seq_along(Address), function(i) {
    list(attributes = list(
      ObjectID = ObjectIDs[i],
      Address = Address[i],
      Address2 = Address2[i],
      City = City[i],
      Postal = Postal[i]
    ))
  })
  
  result = list(records = records)
  json_string = toJSON(result, auto_unbox = TRUE)
  return(json_string)
}


md_geocode_singleline = function(
    .data,
    address_col,
    full_response = FALSE,
    batch_size = 1000,
    timeout = 60
){
  url_base = "https://geodata.md.gov/imap/rest/services/GeocodeServices/MD_CompositeLocator/GeocodeServer/geocodeAddresses?f=json"
  .data$ObjectID = 1:nrow(.data)
  object_id_vector = .data$ObjectID
  address_col = enquo(address_col)
  address_vector = pull(.data, !!address_col)
  num_batches = ceiling(length(address_vector) / batch_size)
  pb = txtProgressBar(max=length(address_vector), style=3)
  geocoded_list = list()
  geocoded_index = 1
  for (i in 1:num_batches) {
    start_index = (i - 1) * batch_size + 1
    end_index = min(i * batch_size, length(address_vector))
    object_id_batch = object_id_vector[start_index:end_index]
    address_batch = address_vector[start_index:end_index]
    batch_json = address_to_json(object_id_batch, address_batch)
    
    batch_response = httr::POST(
      url = url_base,
      body = list(addresses = as.character(batch_json)),
      encode = "form"
    )
    if(httr::status_code(batch_response) != 200){
      message("Error fetching batch ", i,". Skipping...")
      Sys.sleep(timeout)
      next
    }
    
    tryCatch({
      batch_content = httr::content(batch_response, "text", encoding = "UTF-8")
      batch_response_json = fromJSON(batch_content)
      batch_df = jsonlite::flatten(batch_response_json$locations)
    }, error = function(e) {
      message("Error flattening JSON for batch ", i, ": ", e$message)
      message("Skipping this batch and continuing...")
      Sys.sleep(timeout)
      next
    })

    if(!full_response){
      batch_df = batch_df[,c("address", "score", "location.x", "location.y", "attributes.ResultID")]
    }
    names(batch_df) = paste0("md_geocode_",names(batch_df))
    geocoded_list[[geocoded_index]] = batch_df
    geocoded_index = geocoded_index + 1
    setTxtProgressBar(pb, end_index)
    if(i < num_batches){
      Sys.sleep(timeout)
    }
  }
  close(pb)
  geocoded_data = rbindlist(geocoded_list)
  .data = merge(.data, geocoded_data, by.x="ObjectID", by.y="md_geocode_attributes.ResultID")
  return(.data)
}

md_geocode = function(
    .data,
    Address_col,
    Address2_col,
    City_col,
    Postal_col,
    full_response = FALSE,
    batch_size = 1000,
    timeout = 60
){
  url_base = "https://geodata.md.gov/imap/rest/services/GeocodeServices/MD_CompositeLocator/GeocodeServer/geocodeAddresses?f=json"
  .data$ObjectID = 1:nrow(.data)
  object_id_vector = .data$ObjectID
  Address_col = enquo(Address_col)
  Address_vector = pull(.data, !!Address_col)
  Address2_col = enquo(Address2_col)
  Address2_vector = pull(.data, !!Address2_col)
  City_col = enquo(City_col)
  City_vector = pull(.data, !!City_col)
  Postal_col = enquo(Postal_col)
  Postal_vector = pull(.data, !!Postal_col)
  num_batches = ceiling(length(Address_vector) / batch_size)
  pb = txtProgressBar(max=length(Address_vector), style=3)
  geocoded_list = list()
  geocoded_index = 1
  for (i in 1:num_batches) {
    start_index = (i - 1) * batch_size + 1
    end_index = min(i * batch_size, length(Address_vector))
    object_id_batch = object_id_vector[start_index:end_index]
    Address_batch = Address_vector[start_index:end_index]
    Address2_batch = Address2_vector[start_index:end_index]
    City_batch = City_vector[start_index:end_index]
    Postal_batch = Postal_vector[start_index:end_index]
    batch_json = address_to_json_multifield(
      object_id_batch,
      Address_batch,
      Address2_batch,
      City_batch,
      Postal_batch
    )
    
    batch_response = httr::POST(
      url = url_base,
      body = list(addresses = as.character(batch_json)),
      encode = "form"
    )
    if(httr::status_code(batch_response) != 200){
      message("Error fetching batch ", i,". Skipping...")
      Sys.sleep(timeout)
      next
    }
    
    tryCatch({
      batch_content = httr::content(batch_response, "text", encoding = "UTF-8")
      batch_response_json = fromJSON(batch_content)
      batch_df = jsonlite::flatten(batch_response_json$locations)
    }, error = function(e) {
      message("Error flattening JSON for batch ", i, ": ", e$message)
      message("Skipping this batch and continuing...")
      Sys.sleep(timeout)
      next
    })
      
    
    if(!full_response){
      batch_df = batch_df[,c("address", "score", "location.x", "location.y", "attributes.ResultID")]
    }
    names(batch_df) = paste0("md_geocode_",names(batch_df))
    geocoded_list[[geocoded_index]] = batch_df
    geocoded_index = geocoded_index + 1
    setTxtProgressBar(pb, end_index)
    if(i < num_batches){
      Sys.sleep(timeout)
    }
  }
  close(pb)
  geocoded_data = rbindlist(geocoded_list)
  .data = merge(.data, geocoded_data, by.x="ObjectID", by.y="md_geocode_attributes.ResultID")
  return(.data)
}
