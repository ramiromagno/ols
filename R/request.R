req <-
  function(res,
           ...,
           .body = NULL,
           .type = 'application/json',
           .page_size = 20L) {
    params <- list(...)

    req <-
      httr2::request(base_url()) |>
      httr2::req_url_path_append(res) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_headers(Accept = .type) |>
      httr2::req_user_agent(user_agent()) |>
      httr2::req_url_query(size = .page_size) |>
      httr2::req_paginate_next_url(
        parse_resp = httr2::resp_body_json,
        next_url = function(resp, parsed) parsed$`_links`$`next`$href,
        n_pages = function(resp, parsed) parsed$totalPages
      )

    if (!is.null(.body)) {
      req <- httr2::req_body_raw(req, body = .body, type = .type)
    }

    req
  }

multi_req <- function(res,
                      ...,
                      .body = NULL,
                      .type = 'application/json',
                      .page_size = 20L) {
  params <- rlang::list2(...)
  if (is.null(.body))
    .body <- list(.body)
  req_args <-
    vctrs::vec_recycle_common(res = res, !!!params, .body = .body, .type = .type, .page_size = .page_size)
  multi_req <- purrr::pmap(.l = req_args, .f = req)

  multi_req
}

request <-
  function(res,
           ...,
           .body = NULL,
           .type = 'application/json',
           .page_size = 20L,
           .progress = TRUE) {

    dots <- rlang::list2(...)

    # Create a list of multiple requests
    reqs <- multi_req(res = res, !!!dots, .body = .body, .type = .type, .page_size = .page_size)

    resp <-
      purrr::map(
        .x = reqs,
        .f = ~ httr2::paginate_req_perform(
          req = .x,
          max_pages = .page_size,
          progress = .progress
        )
      )

    invisible(resp)
  }

get <- function(endpoint,
                ...,
                .body = NULL,
                .type = 'application/json',
                .page_size = 20L,
                .progress = TRUE) {

  dots <- rlang::list2(...)

  # Replace variable components of the endpoint url
  res <- glue::glue(endpoint, .envir = dots)

  # Endpoint parameters
  params <- setdiff(names(dots), names_in_braces(endpoint))

  resp <-
    request(
      res = res,
      !!!dots[params],
      .body = .body,
      .type = .type,
      .page_size = .page_size,
      .progress = .progress)

  invisible(resp)
}
