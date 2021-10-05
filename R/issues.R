
#' Create an issue from one or more attributes in attribute data
#' 
#' @param df_attribs Data frame of attributes
#' @param vars Character. Variable(s) needed for the logical expression in `condition`.
#' @param where Logical expression that if `TRUE` indicates an issue.
#' @param type Type of issue. Values are as follows: c(`Reject` = 1, `Comment` = 2, `Review` = 4)
#' @param desc Character. Short description for survey managers to understand the issue.
#' @param comment Character. Longer message for interviewers to understand the issue and what to do about it.
#' 
#' @return Data frame of issues with the following columns:
#' - `interview__id`
#' - `interview__key`
#' - `issue_type`
#' - `issue_desc`
#' - `issue_comment`
#' - `issue_vars`
#' - `issue_loc`
#' 
#' @importFrom dplyr `%>%` filter select distinct group_by summarise ungroup left_join mutate
#' @importFrom tidyr pivot_wider
#' @importFrom glue glue_collapse glue
#' @importFrom rlang .data enquo
#' 
#' @export 
create_issue <- function(
    df_attribs,
    vars,
    where,
    type = 1,
    desc,
    comment
) {

    # select necessary attributes only
    # so that data can be converted from long to wide format
    df <- df_attribs %>%
        dplyr::filter(.data$attrib_name %in% vars)

    # convert data from long to wide format
    # so that columns can be compared in `where`
    df_wide <- tidyr::pivot_wider(
        data = df,
        id_cols = c(.data$interview__id, .data$interview__key),
        names_from = .data$attrib_name,
        values_from = .data$attrib_val
    )

    # create a case-wise summary of issue variables
    # so that the summary can serve as a regular expression
    # for later searching of comments that might explain away
    # any issues compiled here
    df_vars <- df %>%
        dplyr::select(.data$interview__id, .data$interview__key, .data$attrib_vars) %>% 
        # tidy up input data so that `issue_vars`
        # no empty values
        dplyr::filter(!.data$attrib_vars %in% c(NA_character_, "")) %>%
        # no duplicate values
        dplyr::distinct(.keep_all = TRUE) %>%
        dplyr::group_by(.data$interview__id, .data$interview__key) %>% 
        dplyr::summarise(
            issue_vars = glue::glue_collapse(.data$attrib_vars, sep = "|")
        ) %>% 
        dplyr::ungroup()

    # combine attribute and issue variable data
    # so that can construct both issue and issue messages
    df_combined <- df_wide %>%
        dplyr::left_join(df_vars, by = c("interview__id", "interview__key"))

    # create output data
    # with columns of fixed names and types
    # so that this data frame can be appended to others
    df_issues <- df_combined %>%
        dplyr::filter(!!enquo(where)) %>%
        dplyr::mutate(
            issue_type = type,
            issue_desc = desc,
            issue_comment = glue::glue(comment),
            issue_loc = NA_character_
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$issue_type, .data$issue_desc, .data$issue_comment, 
            .data$issue_vars, .data$issue_loc
        )

    return(df_issues)

}

#' Make an issue from values in household-level microdata
#' 
#' @param df Data frame of microdata
#' @param where Logical expression that if `TRUE` indicates an issue.
#' @param type Type of issue. Values are as follows: c(`Reject` = 1, `Comment` = 2, `Review` = 4)
#' @param desc Character. Short description for survey managers to understand the issue.
#' @param comment Character. Longer message for interviewers to understand the issue and what to do about it.
#' @param issue_vars Character. Regular expression to describe variables involved in issue.
#' 
#' @inherit create_issue return
#' 
#' @importFrom dplyr `%>%` filter mutate select
#' @importFrom rlang enquo .data
#' 
#' @export 
make_issue <- function(
    df,
    where,
    type = 1,
    desc,
    comment,
    issue_vars
) {

    df_issues <- df %>%
        dplyr::filter(!!rlang::enquo(where))
        dplyr::mutate(
            issue_type = type,
            issue_desc = desc,
            issue_comment = comment,
            issue_vars = issue_vars,
            issue_loc = NA_character_
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$issue_type, .data$issue_desc, .data$issue_comment, 
            .data$issue_vars, .data$issue_loc
        )

    return(df_issues)
}

#' Make issue from values in roster-level microdata
#' 
#' @param df Data frame of microdata
#' @param where Logical expression that if `TRUE` indicates an issue.
#' @param roster_vars Character vector. Names of roster ID variables needed to locate observation in roster.
#' @param type Type of issue. Values are as follows: c(`Reject` = 1, `Comment` = 2, `Review` = 4)
#' @param desc Character. Short description for survey managers to understand the issue.
#' @param comment Character. Longer message for interviewers to understand the issue and what to do about it.
#' @param issue_vars Character. Regular expression to describe variables involved in issue.
#' 
#' @inherit create_issue return
#' 
#' @importFrom glue glue_collapse
#' @importFrom dplyr `%>%` filter mutate select
#' @importFrom rlang enquo .data
#' 
#' @export 
make_issue_in_roster <- function(
    df,
    where,
    roster_vars,
    type = 2,
    desc,
    comment,
    issue_vars
) {

    roster_var_expr <- glue::glue_collapse(
        x = paste0("{", roster_vars, "}"), 
        sep = ", "
    )

    df_issues <- df %>%
        dplyr::filter(!!enquo(where)) %>%
        dplyr::mutate(
            issue_type = type,
            issue_desc = desc,
            issue_comment = comment,
            issue_vars = issue_vars,
            issue_loc = glue::glue(roster_var_expr)           
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key,
            .data$issue_type, .data$issue_desc, .data$issue_comment, 
            .data$issue_vars, .data$issue_loc
        )

    return(df_issues)

}
