# A function that removes all NA columns and NA rows from the first ones in a DF
remove_na_columns = function(df) {
  df = df[, colSums(is.na(df)) < nrow(df)]  # Remove columns with all NA
  df = df[rowSums(is.na(df)) < ncol(df), ]  # Remove rows with all NA
  return(df)
}

# Function to remove rows with all NA values and remove all rows that comes
# after the first row with all NA values
remove_na = function(df) {
  first_all_na = which(rowSums(is.na(df)) == ncol(df))[1]
  if (!is.na(first_all_na)) {
    df = df[1:(first_all_na - 1), ]  # Keep rows before the first all NA row
  }
  df = df[rowSums(is.na(df)) < ncol(df), ]  # Remove rows with all NA
  df = df[, colSums(is.na(df)) < nrow(df)]  # Remove columns with all NA
  return(df)
}

# Parse "depth" column from the summary table
find_depth_column = function(summary_df) {
  depth_col = grep('depth', names(summary_df), ignore.case = TRUE)
  if (length(depth_col) == 0) {
    depth_col = grep('глубина', names(summary_df), ignore.case = TRUE)
  }
  if (length(depth_col) == 0) {
    stop("No depth column found in the summary table.")
  } else if (length(depth_col) == 1) {
    depth_col = names(summary_df)[depth_col]
  } else if (length(depth_col) > 1) {
    depth_col = names(summary_df)[depth_col[1]]
    warning("Multiple depth columns found, using the first one: ",
            depth_col)
  }
  return(select(summary_df, all_of(depth_col)) |> 
           rename(depth = !!depth_col) |> 
           mutate(depth = round(as.numeric(depth), 2)))
}

parse_variable = function(summary_df, variable_name) {
  # Find columns matching pattern
  variable_cols = grep(
    variable_name,
    names(summary_df),
    value = TRUE,
    ignore.case = TRUE)
  
  # Handle no matches
  if (length(variable_cols) == 0) {
    stop("No variables with name '", variable_name, "' found")
  }
  
  # Handle single match
  if (length(variable_cols) == 1) {
    message("Found single column: ", variable_cols)
    variable = summary_df |>
      select(variable_cols[1]) |>
      mutate(find_depth_column(summary_df))
    return(variable)
  }
  
  # Handle multiple matches
  message("Multiple columns found:\n")
  print_options = sapply(seq_along(variable_cols), function(i) {
    cat(i, ":", variable_cols[i], "\n")
  })
  
  # Get user input with validation
  choice = as.integer(readline("Enter variable number: "))
  
  # Validate input
  if (is.na(choice)) {
    stop("Invalid input: Please enter a number")
  }
  if (choice < 1 | choice > length(variable_cols)) {
    stop("Invalid selection: Number must be between 1 and ",
         length(variable_cols))
  }
  
  message("\nSelected column: ", variable_cols[choice])
  
  # Select parsed variable with depth column to ensure that variable values
  # match depths
  variable = summary_df |>
    select(variable_cols[choice]) |>
    mutate(find_depth_column(summary_df))
  return(variable)
}

parse_name = function(summary_df, variable_name) {
  # Find columns matching pattern
  variable_cols = grep(
    variable_name,
    names(summary_df),
    value = TRUE,
    ignore.case = TRUE)
  
  # Handle no matches
  if (length(variable_cols) == 0) {
    stop("No variables with name '", variable_name, "' found")
  }
  
  # Handle single match
  if (length(variable_cols) == 1) {
    message("Found single column: ", variable_cols)
    return(variable_cols[1])
  }
  
  # Handle multiple matches
  message("Multiple columns found:\n")
  print_options = sapply(seq_along(variable_cols), function(i) {
    cat(i, ":", variable_cols[i], "\n")
  })
  
  # Get user input with validation
  choice = as.integer(readline("Enter variable number: "))
  
  # Validate input
  if (is.na(choice)) {
    stop("Invalid input: Please enter a number")
  }
  if (choice < 1 | choice > length(variable_cols)) {
    stop("Invalid selection: Number must be between 1 and ",
         length(variable_cols))
  }
  
  message("\nSelected column: ", variable_cols[choice])
  # Select parsed variable with depth column to ensure that variable values
  # match depths
  return(variable_cols[choice])
}

standardize_columns = function(gran) {
  # Define mapping patterns for each category
  category_map = list(
    depth = c('глубина', 'depth'),
    clay = c('мелкая глина', "\\<2\\.0"),
    very_fine_silt = c('крупная глина', "2\\.0-4\\.0"),
    fine_silt = c('мелкий алеврит', "4\\.0-8\\.0"),
    medium_silt = c('средний алеврит', "8\\.0-16\\.0"),
    coarse_silt = c('крупный алеврит', "16\\.0-31\\.0"),
    very_coarse_silt = c('грубый алеврит', "31\\.0-63\\.0"),
    very_fine_sand = c('тонкий песок', "63\\.0-125\\.0"),
    fine_sand = c('мелкий песок', "125\\.0-250\\.0"),
    medium_sand = c('средний песок', "250\\.0-500\\.0"),
    coarse_sand = c('крупный песок', "500\\.0-1000\\.0"),
    very_coarse_sand = c('грубый песок', "1000\\.0-2000\\.0"),
    gravel = c('гравий', "\\>2000")
  )
  
  # Initialize result dataframe
  result = data.frame(matrix(nrow = nrow(gran), ncol = 0))
  colnames(result) = character(0)
  
  # Process each category
  for (cat_name in names(category_map)) {
    # Create regex pattern combining all alternatives
    regex_pattern = paste0("(",
                           paste(category_map[[cat_name]], collapse = "|"),
                           ")")
    
    # Find matching columns (case-insensitive)
    matches = str_detect(names(gran),
                         regex(regex_pattern,
                               ignore_case = T))
    matched_cols = names(gran)[matches]
    
    # Select first match if exists
    if (length(matched_cols) > 0) {
      # Use first match if multiple found
      if (length(matched_cols) > 1) {
        message(
          sprintf(
            "Multiple matches for '%s': %s. Using first: %s",
            cat_name,
            paste(matched_cols, collapse = ", "),
            matched_cols[1]
          )
        )
      }
      
      # Add to result with standardized name
      result[[cat_name]] = gran[[matched_cols[1]]]
    } else {
      warning(sprintf("No columns found for category: %s", cat_name))
    }
  }
  
  return(result)
}

igras_specific_CNHS_loading = function(files) {
  cnhs_match = grep('CNHS', files, ignore.case = TRUE)
  
  if (length(cnhs_match) == 0) {
    stop("No CNHS file found in the specified directory.")
  } else if (length(cnhs_match) > 1) {
    warning("Multiple CNHS files found, using the first one: ",
            files[cnhs_match[1]])
    cnhs_match = cnhs_match[1]
  }
  cnhs = readxl::read_excel(files[cnhs_match], 
                            sheet = "ведомость", skip = 3) |> 
    select('Шифр на пробирке', 'Глубина отбора, м')
  colnames(cnhs) = c('Name', 'depth')
  cnhs = arrange(cnhs, depth)
  
  cnhs2 = readxl::read_excel(files[cnhs_match], 
                             sheet = "CN data", skip = 1)
  colnames(cnhs2) = c('Name', 'C', 'N', 'H', 'S_cnhs', 'C/N', 'CNatm')
  
  cnhs = cnhs |> 
    left_join(cnhs2, by = 'Name') |> 
    select(depth, C, N, H, S_cnhs, `C/N`)
  return(cnhs)
}
