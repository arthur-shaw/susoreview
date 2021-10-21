
#' Extract attributes from variables in microdata
#' 
#' @param df Data frame. Household-level microdata that contains the attribute to extract.
#' @param var Bare name. Variable whose value to extract.
#' @param attrib_name Character. Name to give the attribute in the issues data file
#' @param attrib_vars Character. Regular expression that identifies the variable in `var`.
#' 
#' @return Data frame of attributes with the following columns:
#' - `interview__id`
#' - `interview__key`
#' - `attrib_name`
#' - `attrib_val`
#' - `attrib_vars`
#' 
#' @importFrom dplyr `%>%` mutate select
#' @importFrom rlang .data
#' 
#' @export 
extract_attribute <- function(
    df,
    var,
    attrib_name,
    attrib_vars
) {

    df_attribs <- df %>%
        dplyr::mutate(
            attrib_val = {{var}},
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

}

#' Create attribute from one or more variable in microdata
#' 
#' @param df Data frame. Household-level microdata that contains the attribute to extract.
#' @param condition Logical expression
#' @param attrib_name Character. Name to give the attribute in the issues data file
#' @param attrib_vars Character. Regular expression that identifies the variable in `var`.
#' 
#' @inherit extract_attribute return
#' 
#' @importFrom dplyr `%>%` mutate select
#' @importFrom rlang enquo .data
#' 
#' @export 
create_attribute <- function(
    df,
    condition,
    attrib_name,
    attrib_vars    
) {

    df_attribs <- df %>%
        dplyr::mutate(
            attrib_val = !!rlang::enquo(condition),
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

    return(df_attribs)

}

#' Count columns with value in microdata
#' 
#' Count then number of columns matching `var_pattern` that have value `var_val`.
#' 
#' @param df Data frame. Household-level microdata that contains the variables to count.
#' @param var_pattern Character Regular expression used to select variables to be counted.
#' @param var_val Numeric. Value(s) to count in columns identified by `var_pattern`.
#' @param attrib_name Character. Name of attribute.
#' @param attrib_vars Character. Regular expression that identifies the variable(s) in `var_pattern`.
#' 
#' @inherit extract_attribute return
#' 
#' @importFrom dplyr `%>%` mutate across matches select
#' @importFrom rlang .data
#' 
#' @export 
count_vars <- function(
    df,
    var_pattern,
    var_val = 1,
    attrib_name,
    attrib_vars = var_pattern
) {

    # check that vars exist
    # check that vars are all the same type (such that rowSums can be performed over them)

    df_attribs <- df %>%
        dplyr::mutate(
            # so that rowSum counts columns with the desired value,
            # mark all columns with `var_val` as TRUE; otherwise, FALSE
            dplyr::across(
                .cols = dplyr::matches(var_pattern),
                .fns = ~ .x %in% var_val
            ),
            attrib_val = rowSums(
                dplyr::select(., dplyr::matches(var_pattern)),
                na.rm = TRUE
            ),
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

    return(df_attribs)

}

#' Create attribute about whether any variable contains a specific value
#' 
#' Inspect all columns whose names match `var_pattern` 
#' and determine whether any contain the value `var_val`.
#' 
#' @param df Data frame. Household-level microdata that contains the attribute to extract.
#' @param var_pattern Character Regular expression used to select variables to be counted.
#' @param var_val Numeric. Value(s) to count in columns identified by `var_pattern`.
#' @param attrib_name Character. Name to give the attribute in the issues data file
#' @param attrib_vars Character. Regular expression that identifies the variable in `var`.
#' 
#' @inherit extract_attribute return
#' 
#' @importFrom dplyr `%>%` mutate across matches if_any select
#' @importFrom rlang .data
#' 
#' @export 
any_vars <- function(
    df,
    var_pattern,
    var_val = 1,
    attrib_name,
    attrib_vars = var_pattern
) {

    df_attribs <- df %>%
        dplyr::mutate(
            # so that rowSum counts columns with the desired value,
            # mark all columns with `var_val` as TRUE; otherwise, FALSE
            dplyr::across(
                .cols = dplyr::matches(var_pattern),
                .fns = ~ .data$.x %in% var_val
            ),
            attrib_val = dplyr::if_any(
                .cols = dplyr::matches(var_pattern),
                .fns = .data$.x == TRUE
            ),
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

    return(df_attribs)    

}

#' Count the number of observations where a condition is `TRUE`.
#' 
#' Count the number of observations in a non-household level microdata--such as
#' a parcel, plot, of food item data frame--where a user-provided condition 
#' is `TRUE`.
#' 
#' @param df Data frame. Household-level microdata that contains the attribute to extract.
#' @param where Logical expression whose variables exist in `df`.
#' @param attrib_name Character. Name to give the attribute in the issues data file
#' @param attrib_vars Character. Regular expression that identifies the variable in `var`.
#' 
#' @inherit extract_attribute return
#' 
#' @importFrom dplyr `%>%` mutate group_by summarise ungroup select
#' @importFrom rlang .data
#' 
#' @export 
count_obs <- function(
    df, 
    where, # consider renaming as `where`
    attrib_name,
    attrib_vars
) {

    df_attribs <- df %>%
        dplyr::mutate(attrib_val = {{where}}) %>%
        dplyr::group_by(.data$interview__id, .data$interview__key) %>%
        dplyr::summarise(attrib_val = sum(.data$attrib_val, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

    return(df_attribs)

}

#' Count the non-missing elements of a list variable
#' 
#' @param df Data frame. Household-level microdata that contains the attribute to extract.
#' @param var_pattern Character. Regular expression used to select variables to be counted.
#' @param missing_vals Character. Values that describes an empty element of a list variable.
#' @param attrib_name Character. Name to give the attribute in the issues data file
#' @param attrib_vars Character. Regular expression that identifies the variable in `var`.
#' 
#' @inherit extract_attribute return
#' 
#' @importFrom dplyr `%>%` mutate across select
#' @importFrom rlang .data
#' 
#' @export 
count_list <- function(
    df,
    var_pattern,
    missing_vals = c("##N/A##", "", NA_character_),
    attrib_name,
    attrib_vars = var_pattern
) {

    df_attribs <- df %>%
        dplyr::mutate(
            # so that rowSum counts columns with the desired value,
            # mark all columns with `missing_val` as TRUE; otherwise, FALSE
            dplyr::across(
                .cols = dplyr::matches(var_pattern),
                .fns = ~ !.x %in% missing_vals
            )
        ) %>%
        dplyr::mutate(
            attrib_val = rowSums(
                dplyr::select(., dplyr::matches(var_pattern)),
                na.rm = TRUE
            ),
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

    return(df_attribs)

}

#' Determine whether where a condition is `TRUE` for any observation.
#' 
#' Determine whether a user-provided condition is `TRUE` for any non-household
#' level microdata--such as a parcel, plot, of food item data frame.
#' 
#' @param df Data frame. Household-level microdata that contains the attribute to extract.
#' @param where Logical expression whose variables exist in `df`.
#' @param attrib_name Character. Name to give the attribute in the issues data file
#' @param attrib_vars Character. Regular expression that identifies the variable in `var`.
#' 
#' @inherit extract_attribute return
#' 
#' @importFrom dplyr `%>%` mutate group_by summarise ungroup select
#' @importFrom rlang .data
#' 
#' @export 
any_obs <- function(
    df,
    where, 
    attrib_name,
    attrib_vars
) {

    df_attribs <- df %>%
        dplyr::mutate(attrib_val = {{where}}) %>%
        dplyr::group_by(.data$interview__id, .data$interview__key) %>%
        dplyr::summarise(attrib_val = any(.data$attrib_val == 1, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

    return(df_attribs)

}

#' Sum the values of a variable across records constituting a household observation
#' 
#' @param df Data frame. Household-level microdata that contains the attribute to extract.
#' @param var Bare name. Variable to sum.
#' @param attrib_name Character. Name to give the attribute in the issues data file
#' @param attrib_vars Character. Regular expression that identifies the variable in `var`.
#' 
#' @inherit extract_attribute return
#' 
#' @importFrom dplyr `%>%` group_by summarise ungroup select
#' @importFrom rlang .data
#' 
#' @export 
sum_vals <- function(
    df,
    var,
    attrib_name,
    attrib_vars
) {

    df_attribs <- df %>%
        dplyr::group_by(.data$interview__id, .data$interview__key) %>%
        dplyr::summarise(attrib_val = sum({{var}}, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$attrib_name, .data$attrib_val, .data$attrib_vars
        )

}
