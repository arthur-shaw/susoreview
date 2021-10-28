#' Determine whether interviews contain comments relevant to the rejection decision.
#' 
#' @param df_comments Data frame. Corresponds to interview__comments export file.
#' @param df_issues Data frame. File of issues.
#' @param df_cases_to_review Data frame. File of cases that are subject to review.
#' 
#' @return Data frame. One observation per interview with a comment.
#' 
#' @importFrom dplyr `%>%` filter distinct semi_join group_by ungroup all_of row_number n select 
#' @importFrom rlang syms `!!!` .data
#' @importFrom fuzzyjoin regex_semi_join
#' 
#' @export 
check_for_comments <- function(
    df_comments,
    df_issues,
    df_cases_to_review
) {

    # =============================================================================
    # Identify comments relevant for rejection decision
    # =============================================================================

    # -----------------------------------------------------------------------------
    # Comments on issue variables
    # -----------------------------------------------------------------------------

    # create set of unique issue variables (regex patterns)
    unique_issue_vars <- df_issues %>% 
        dplyr::filter(.data$issue_vars != "") %>%
        dplyr::distinct(.data$issue_vars)

    # filter to interviews with any comments at all
    cases_to_review_w_comments <- df_comments %>%
        dplyr::semi_join(df_cases_to_review, by = "interview__id")

    # determine ID columns
    comment_id_cols <- stringr::str_subset(names(df_comments), "^id[0-9]+$") %>% rlang::syms()

    comments_on_issue_vars <- cases_to_review_w_comments %>% 
        # filter to comments left by the interviewer that are the last in their comment string
        dplyr::filter(!stringr::str_detect(string = .data$variable, pattern = "^@@")) %>% 	# remove Complete/Reject/Approve comments
        dplyr::group_by(.data$interview__id, .data$variable, !!!comment_id_cols) %>%    # group by interview-variable-row
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 							# keep last comment within group
        dplyr::filter(.data$role == 1) %>% 											        # retain only Interviewer comments
        dplyr::ungroup() %>%
        # filter to comments concerning variables used in identifying issues
        fuzzyjoin::regex_semi_join(
            unique_issue_vars, 
            by = c("variable" = "issue_vars")
        ) %>%
        dplyr::select(.data$interview__id)

    # -----------------------------------------------------------------------------
    # Comments on interview overall
    # -----------------------------------------------------------------------------

    # filter to overall comments
    comments_on_interview_overall <- cases_to_review_w_comments %>%
        dplyr::filter(.data$variable == "@@Completed") %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
        dplyr::ungroup()  %>%
        dplyr::select(.data$interview__id)

    # =============================================================================
    # Identify interviews with and without comments on issue variables
    # =============================================================================

    # interviews with any comments--either for key variables or for the interview overall
    interviews_with_comments <- dplyr::full_join(
        comments_on_issue_vars, 
        comments_on_interview_overall, 
        by = "interview__id"    
    )

    interviews_have_comments <- dplyr::semi_join(
        df_cases_to_review, 
        interviews_with_comments, 
        by = "interview__id"
    )

    return(interviews_have_comments)

}


#' Reject interview using the appropriate set of rejection API calls
#' 
#' @param interview__id Character. GUID for interview found in `interview__id`.
#' @param interview__status Numeric. Supports values in set c(100, 120, 130).
#' @param reject_comment Character. Comment to post upon rejection
#' @param statuses_to_reject Numeric vector. Supports values in set c(100, 120, 130).
#' @param reject_hq_approved Boolean. Flag that determines whether HeadquartersApproved assignments will be rejected or not. Default behavior is to reject.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose questionnaires and associated interviews to get.
#' @param user API user name
#' @param password API password
#' 
#' @importFrom susoapi reject_interview_as_sup reject_interview_as_hq unapprove_interview reject_interview_as_hq
#' @importFrom dplyr if_else
#' @importFrom glue glue
#' 
#' @export 
reject_interview <- function(
    interview__id,
    interview__status,
    reject_comment,
    statuses_to_reject = c(100, 120),
    reject_hq_approved = TRUE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = "primary",
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password       
) {

    # check inputs
    # TODO: Add check below
    # interview__id is GUID
    # interview__status is in set c(100, 120, 130)

    # only cases
    if (interview__status %in% statuses_to_reject) {

        # Completed
        if (interview__status == 100) {
            susoapi::reject_interview_as_sup(
                interview_id = interview__id, 
                comment = reject_comment,
                server = server,
                workspace = workspace,
                user = user,
                password = password
            )
        # ApprovedBySupervisor
        } else if (interview__status == 120) {
            susoapi::reject_interview_as_hq(
                interview_id = interview__id, 
                comment = reject_comment,
                server = server,
                workspace = workspace,
                user = user,
                password = password
            )
        # ApprovedByHeadquarters
        } else if (interview__status == 130) {
            if (reject_hq_approved == TRUE) {
                # first, unapprove
                susoapi::unapprove_interview(
                    interview_id = interview__id,
                    server = server,
                    workspace = workspace,
                    user = user,
                    password = password                    
                )
                # then, reject
                susoapi::reject_interview_as_hq(
                    interview_id = interview__id, 
                    comment = reject_comment,
                    server = server,
                    workspace = workspace,
                    user = user,
                    password = password
                )
            } else if (reject_hq_approved == FALSE) {
                message(glue::glue(
                    "Interview {interview__id} not rejected", 
                    "The parameter `reject_hq_approved` is set to `FALSE`",
                    "To reject the interview, set `reject_hq_approved` to `TRUE`",
                    .sep = "\n"
                ))
            }

        }
        # NOTE: consider providing message for all non-handled statuses

    } else {

        status_list <- glue::glue_collapse(x = statuses_to_reject, sep = ", ", last = " and ")
        is_are <- dplyr::if_else(length(statuses_to_reject) > 1, "are", "is")

        message(glue::glue(
            "Interview {interview__id} not rejected.",
            "The interview's status is {interview__status}, but only {status_list} {is_are} specified in `statuses_to_reject`",
            .sep = "\n"
        ))

    }



}

#' Add an issue for each SuSo validation error
#' 
#' First, transform SuSo validations from `interview__errors` into issues of the type indicated in `issue_type`. 
#' Then add these issues to the main issues files.
#' 
#' @param df_cases_to_review Data frame containing cases to review.
#' @param df_errors Data frame containing SuSo validation errors.
#' @param issue_type Numeric. Value of the issue type that each SuSo error will be considered.
#' @param df_issues Data frame containing auto-reject issues.
#' 
#' @return Data frame. Issues file with additional rows for SuSo validation errors.
#' 
#' @importFrom dplyr `%>%` semi_join mutate select bind_rows
#' @importFrom tidyr unite
#' @importFrom rlang .data
#' 
#' @export 
add_issues_for_suso_errors <- function(
    df_cases_to_review,
    df_errors,
    issue_type = 3,
    df_issues
) {

    errors <- df_errors %>%
        dplyr::semi_join(df_cases_to_review, by = c("interview__id", "interview__key"))

    errors_as_issues <- errors %>%
        # specify
        dplyr::mutate(
            issue_type = issue_type,
            issue_desc = "Validation error", 
            issue_comment = .data$message,
            issue_vars = .data$variable
        ) %>%
        # construct issue location as comma-separated concatenation of `id` columns
        tidyr::unite(
            col = "issue_loc", 
            starts_with("id"), 
            na.rm = TRUE, 
            sep = ", "
        ) %>%
        dplyr::select(
            .data$interview__id, .data$interview__key, 
            .data$issue_vars, .data$issue_type, .data$issue_desc, 
            .data$issue_comment, .data$issue_loc
        )

    all_issues <- dplyr::bind_rows(df_issues, errors_as_issues)

    return(all_issues)

}

#' Add an issue for interviews that have too many questions left unanswered
#' 
#' The threshold for unanswered interviews can be specified in one of two ways. The first way is to set a global threshold through the `n_unanswered_ok` parameter.
#' 
#' The second way is through a data frame that specifies an interview-specific threshold. This may be useful when there are questions legitimately left unanswered, most often due to the design of the questionnaire. 
#' 
#' @param df_cases_to_review Data frame. Cases to review.
#' @param df_interview_stats Data frame. Interview statistics fetched from the server.
#' @param df_issues Data frame. Contains issues for the interviews.
#' @param n_unanswered_ok Numeric. Maximum number of unanswered questions allowed before recording an issue.
#' @param df_legit_miss Data frame. Interview-specific threshold for unanswered questions.
#' @param issue_type Numeric. Issue type for the issue file: c(1, 2, 3, 4)
#' @param issue_desc Character. Short description of the issue.
#' @param issue_comment Expression. Expression--for example glue or paste0--that yields an issue message meant for interviewers to understand the problem.
#' 
#' @return Data frame. Issues data frame with new record for interviews with too many questions left unanswered.
#' 
#' @importFrom dplyr `%>%` filter select left_join mutate bind_rows starts_with
#' @importFrom rlang quo .data
#' 
#' @export 
add_issue_if_unanswered <- function(
    df_cases_to_review,
    df_interview_stats,
    df_issues,
    n_unanswered_ok = 0,
    df_legit_miss = NULL,
    issue_type = 1,
    issue_desc = "Questions left unanswered",
    issue_comment = glue::glue("ERROR: The interview is marked as complete, but there are {NotAnswered} questions left without a response. Please answer these questions.")
) {

    # issue_comment defuse expression for later evaluation
    issue_comment_expr <- rlang::enquo(issue_comment)

    # identify completed interviews
    completed_interviews <- df_cases_to_review %>%
        dplyr::filter(.data$interview_complete == 1) %>%
        dplyr::select(.data$interview__id, .data$interview__key)

    # retain only the relevant variables for the interview statistics
    interview_stats <- dplyr::select(df_interview_stats, .data$interview__id, .data$NotAnswered)

    # create an issue entry when the interview has more than the threshold of unanswered questions
    int_complete_but_q_unanswered <- dplyr::left_join(completed_interviews, interview_stats, by = "interview__id") %>%        
        # if no legit missing dset specified, carry forward data frame; otherwise, merge it
        {if (is.null(df_legit_miss)) . else dplyr::left_join(., df_legit_miss, by = c("interview__id", "interview__key"))} %>%
        # if legit missing dset specified, filter to those with unanswereds more than overall threshold; otherwise, use interview-specific threshold
        {if (is.null(df_legit_miss)) dplyr::filter(., .data$NotAnswered > n_unanswered_ok) else dplyr::filter(., .data$NotAnswered > (.data$n_unanswered_ok + .data$n_legit_miss))} %>%
        # create an issue entry for excess unanswered questions
        dplyr::mutate(
            issue_type = issue_type,
            issue_desc = issue_desc,
            issue_comment = !!issue_comment_expr, 
            issue_loc = "",
            issue_vars = "",
        ) %>%
        dplyr::select(.data$interview__id, .data$interview__key, dplyr::starts_with("issue"))

    # add the issue entry to the issues data frame
    issues_updated <- dplyr::bind_rows(df_issues, int_complete_but_q_unanswered)

    return(issues_updated)

}

#' Decide how to handle interviews: reject, review, or approve
#' 
#' @param df_cases_to_review Data frame. Cases to review.
#' @param df_issues Data frame. Issues.
#' @param issue_types_to_reject Numeric vector. Value of issues to reject (e.g., )
#' @param df_has_comments Data frame. Interviews with comments overall or on critical questions
#' @param df_interview_stats Data frame. Interview statistics, such as number of unanswered, invalid, or commented questions
#' 
#' @return List of data frames: `list(to_reject, to_review, to_approve)`
#' 
#' @importFrom dplyr `%>%` filter distinct left_join select anti_join semi_join inner_join full_join
#' @importFrom rlang .data
#' 
#' @export 
decide_action <- function(
    df_cases_to_review,
    df_issues,
    issue_types_to_reject,
    df_has_comments,
    df_interview_stats
) {

    # =============================================================================
    # Determine whether has attributes relevant to rejection decision
    # =============================================================================

    # has at least 1 major issue
    interviews_have_issues <- df_issues %>%
        dplyr::filter(.data$issue_type %in% issue_types_to_reject) %>%
        dplyr::distinct(.data$interview__id, .data$interview__key) %>%
        dplyr::left_join(df_cases_to_review, by = c("interview__id", "interview__key")) %>%
        dplyr::select(.data$interview__id, .data$interview__key)

    # requires review
    interviews_need_review <- df_issues %>%
        dplyr::filter(.data$issue_type == 4) %>%
        dplyr::distinct(.data$interview__id, .data$interview__key) %>%
        dplyr::left_join(df_cases_to_review, by = c("interview__id", "interview__key"))	%>%
        dplyr::select(.data$interview__id, .data$interview__key)

    # has 1+ SuSo validation error or 1+ comment on a non-critical question
    interview_has_error_or_comment <- df_interview_stats %>%
        dplyr::filter(.data$Invalid >= 1 | .data$WithComments >= 1) %>%
        dplyr::select(.data$interview__id, .data$interview__key)

    # has no unanswered question, no SuSo validation error, and no non-critical comment
    interview_has_no_error_or_comment <- df_interview_stats %>%
        dplyr::filter(.data$NotAnswered == 0 & .data$Invalid == 0 & .data$WithComments == 0) %>%
        dplyr::select(.data$interview__id, .data$interview__key)

    # =============================================================================
    # Reject
    # =============================================================================

    to_reject <- interviews_have_issues %>%
        # has at least 1 major issue, but no comments
        dplyr::anti_join(df_has_comments, by = c("interview__id", "interview__key")) %>%
        dplyr::inner_join(df_cases_to_review, by = c("interview__id", "interview__key")) %>%
        dplyr::select(.data$interview__id, .data$interview__key, interview__status)

    # =============================================================================
    # Review
    # =============================================================================

    to_review <- interviews_have_issues %>%
        dplyr::select(.data$interview__id, .data$interview__key) %>%
        # has 1+ critical issue and commments on at least 1 critical issue
        dplyr::inner_join(df_has_comments, by = c("interview__id", "interview__key")) %>%
        # and/or has 1+ SuSo validation error or 1+ comment on a non-critical question
        dplyr::full_join(interview_has_error_or_comment, by = c("interview__id", "interview__key")) %>%
        # and/or has an issue that requires review
        dplyr::full_join(interviews_need_review, by = c("interview__id", "interview__key")) %>%
        # but is not on the rejection list
        dplyr::anti_join(to_reject, by = c("interview__id", "interview__key")) %>%
        dplyr::inner_join(df_cases_to_review, by = c("interview__id", "interview__key")) %>%
        dplyr::select(.data$interview__id, .data$interview__key, interview__status)

    # =============================================================================
    # Approve
    # =============================================================================

    to_approve <- df_cases_to_review %>%
        dplyr::select(.data$interview__id, .data$interview__key) %>%
        dplyr::inner_join(interview_has_no_error_or_comment, by = c("interview__id", "interview__key")) %>%
        dplyr::anti_join(interviews_have_issues, by = c("interview__id", "interview__key")) %>%
        dplyr::inner_join(df_cases_to_review, by = c("interview__id", "interview__key")) %>%
        dplyr::select(.data$interview__id, .data$interview__key, interview__status)        

    # =============================================================================
    # Return a data frame for each recommendation
    # =============================================================================

    decisions <- list(
        to_reject = to_reject,
        to_review = to_review,
        to_approve = to_approve
    )

    return(decisions)

}

#' Add consolidated rejection message to data frame of interviews to reject
#' 
#' @param df_to_reject Data frame. Interviews to reject. Contains columns interview__id, interview__key, interviewComplete, interview__status
#' @param df_issues Data frame. Issues noted for each interview.
#' @param issue_types_to_reject Numeric. Issue type values to reject.
#' 
#' @return Data frame with 1 observation per interview that consists of interview ID, rejection message, and interview status
#' 
#' @importFrom dplyr `%>%` left_join arrange filter distinct group_by summarize first ungroup
#' @importFrom  rlang .data
#' 
#' @export 
add_rejection_msgs <- function(
    df_to_reject, 
    df_issues,
    issue_types_to_reject = 1    
) {

    to_reject <- df_to_reject %>%
        # add issues text to rejection list
        dplyr::left_join(df_issues, by = c("interview__id", "interview__key")) %>%
        # order by interview, interview type, and interview description
        dplyr::arrange(.data$interview__id, .data$issue_type, .data$issue_desc) %>%
        # limit to issues that will result in rejection
        dplyr::filter(.data$issue_type %in% issue_types_to_reject) %>%
        # remove duplicate issues (e.g., issues for variables in a roster)
        dplyr::distinct(.data$interview__id, .data$interview__key, .data$issue_comment, .keep_all = TRUE) %>%
        # create reject message that is vertical concatenation of issue text
        # separated by new line character
        # preserve interview__status for rejection
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(    
            reject_message = paste(.data$issue_comment, collapse = " \n"),
            interview__status = dplyr::first(.data$interview__status)
        ) %>%
        dplyr::ungroup()

}

#' Flag persistant issues
#' 
#' Use rejection comments to flag persistent issues--that is, rejection reasons that been repeated for the interview. 
#' Creates a data frame of interviews with persistent issues. These require manual follow-up by headquarters staff.
#' Removes any interviews with persistent isues from the to-reject data frame.
#' 
#' @param df_comments Data frame. Comments on interviews. Importantly, this includes comments posted upon rejection.
#' @param df_to_reject Data frame. Interviews to be rejected.
#' 
#' @return List of data frames: list(to_reject, to_follow_up). The to_reject data frame removes any cases where follow-up is required for persistent problems. The to_follow_up data frame identifies any cases where follow-up is required for persistent problems.
#' 
#' @importFrom rlang .data
#' @importFrom dplyr `%>%` filter mutate select rename full_join group_by arrange ungroup semi_join anti_join
#' @importFrom stringr str_replace
#' @importFrom tidyr separate_rows
#' 
#' @export 
flag_persistent_issues <- function(
    df_comments,
    df_to_reject
) {

    # =============================================================================
    # Process current and past rejection messages
    # =============================================================================

    reject_messages_past <- df_comments %>%
        # find rejections
        dplyr::filter(
            (.data$variable %in% c(
                "@@RejectedBySupervisor", 
                "@@RejectedByHeadquarter", 
                "@@UnapprovedByHeadquarters")
            ) & 
            (.data$role %in% c(
                3,  # Headquarters
                4,  # Administrator
                5   # API user
            ))
        ) %>%        
        dplyr::mutate(														
            # remove undesirable content from rejection messages
            comment = stringr::str_replace(.data$comment, '^"[ ]*', ""),			# starting quote
            comment = stringr::str_replace(.data$comment, '[ ]*"$', ''),			# ending quote
            comment = stringr::str_replace(.data$comment, 							# ending strange content
                "\\[WebInterviewUI:CommentYours[\\]]*$", ""),
            comment = stringr::str_replace(.data$comment,
                "^[\\[]*WebInterviewUI:CommentYours\\] ", ""),		        # starting strange content
            comment = stringr::str_replace(.data$comment, "Your comment ", ""),	# more starting strange content
            comment = stringr::str_trim(.data$comment, side = "both"), 			# whitespace padding
            comment = stringr::str_replace(.data$comment, "\\.$", ""), 			# terminal .
            comment = stringr::str_replace(.data$comment, "\\n[ \\.]*$", ""), 	# terminal \n
            # make date variable into Date type
            date = as.Date(.data$date, format = "%Y-%m-%d")
        ) %>%
        # expand data set to the error level, where separators are newline characters
        tidyr::separate_rows(.data$comment, sep = " \\n ") %>%
        dplyr::mutate(comment = as.character(.data$comment)) %>%
        # keep only the necessary columns
        dplyr::select(.data$interview__id, .data$date, .data$order, .data$comment)

    reject_messages_current <- df_to_reject %>%
        # use system date as rejection date
        dplyr::mutate(date = Sys.Date()) %>%
        # rename error message column to match interview__comments
        dplyr::rename(comment = .data$reject_message) %>%
        # expand data set to the error level, where separators are newline characters
        tidyr::separate_rows(.data$comment, sep = " \\n ") %>%
        dplyr::mutate(comment = as.character(.data$comment)) %>%
        # keep only the necessary columns
        dplyr::select(.data$interview__id, .data$date, .data$comment)

    reject_messages_combined <- 
        # merge the two data files
        dplyr::full_join(
            reject_messages_past, 
            reject_messages_current, 
            by = c("interview__id", "date", "comment")
        ) %>%
        # sort errors into sequential order
        dplyr::group_by(.data$interview__id) %>%
        dplyr::arrange(.data$interview__id, .data$date, .data$order, .by_group = TRUE) %>%
        dplyr::ungroup()

    # =============================================================================
    # Identify any current messages that have appeared in the past
    # =============================================================================

    # repeated messages--that is, that appear in current rejections but also in past rejections
    reject_messages_repeated <- reject_messages_current %>%
        dplyr::select(.data$interview__id, .data$comment) %>%
        dplyr::semi_join(
            reject_messages_past, 
            by = c("interview__id","comment")
        )

    # interviews where follow-up is required for repeated comments
    to_follow_up <- 
        dplyr::semi_join(
            reject_messages_combined, 
            reject_messages_repeated, 
            by = c("interview__id", "comment")
        ) %>%
        select(.data$interview__id, .data$date, .data$comment)

    # =============================================================================
    # Remove issues interviews with persistent problems for rejection list
    # =============================================================================

    # filter out recurrent issues from interviews to be rejected
    to_reject <- df_to_reject %>%
        dplyr::anti_join(to_follow_up, by = "interview__id")

    # =============================================================================
    # Return list of data frames: updated to_reject and new to_follow_up
    # =============================================================================

    more_decisions <- list(
        to_reject = to_reject,
        to_follow_up = to_follow_up
    )

    return(more_decisions)

}

#' Post comments to questions, if any
#' 
#' First, extracts comments from issues file for interviews to reject. Then, posts those comments, if any
#' 
#' @param df_to_reject Data frame containing interviews to reject.
#' @param df_issues Data frame of issues.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose questionnaires and associated interviews to get.
#' @param user API user name
#' @param password API password
#' 
#' @return Server-side effect of posting all applicable comments to all applicable questionnaires
#' 
#' @importFrom dplyr `%>%` left_join filter select
#' @importFrom rlang .data
#' @importFrom susoapi comment_question
#' @importFrom purrr pwalk
#' 
#' @export 
post_comments <- function(
    df_to_reject,
    df_issues,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = "primary",
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password      
) {

    # identify comments to post
    comments_to_post <- df_to_reject %>%
        dplyr::left_join(df_issues, by = "interview__id") %>%
        dplyr::filter(.data$issue_type == 2) %>%
        dplyr::select(
            .data$interview__id, 
            variable_name = .data$issue_vars, 
            row_number = .data$issue_loc, 
            comment = .data$issue_comment
        )

    # post comments
    if (nrow(comments_to_post) > 0) {
        purrr::pwalk(
            .l = comments_to_post,
            .f = comment_question,
            server = server,
            user = user,
            password = password
        )
    }

}
