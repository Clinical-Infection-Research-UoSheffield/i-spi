# ==============================================================
# 1. SETUP & LIBRARIES (MERGED)
# ==============================================================
source("global.R", local = TRUE)

# Set to 1 for local and do not push in prod
# Sys.setenv(LOCAL_DEV = "1")
# local_email_user <- "seamus.owen.stein@dartmouth.edu"
#local_email_user <- "mscotzens@gmail.com"

# Source authentication configuration (Step 1)
# Defines DEX_*, APP_REDIRECT_URI, OIDC_SCOPES, endpoints, get_jwks(), `%||%`, dex_client
print("Sourcing auth_config.R...")
source("auth_config.R", local = FALSE) # Source globally
print("Finished sourcing auth_config.R")

# --- Debug printing from HEAD ---
print("--- Final Auth Config Values ---")
print(paste("DEX_ISSUER:", DEX_ISSUER))
print(paste("DEX_INTERNAL_URL:", if(exists("DEX_INTERNAL_URL")) DEX_INTERNAL_URL else "NOT SET"))
print(paste("DEX_AUTH_ENDPOINT:", DEX_AUTH_ENDPOINT))
print(paste("DEX_TOKEN_ENDPOINT:", DEX_TOKEN_ENDPOINT))
print(paste("DEX_JWKS_ENDPOINT:", DEX_JWKS_ENDPOINT))
print(paste("DEX_LOGOUT_ENDPOINT:", DEX_LOGOUT_ENDPOINT))
print(paste("DEX_CLIENT_ID:", DEX_CLIENT_ID))
print(paste("DEX_CLIENT_SECRET:", if(nchar(DEX_CLIENT_SECRET)>0) "******" else "NOT SET"))
print(paste("APP_REDIRECT_URI:", APP_REDIRECT_URI))
print(paste("OIDC_SCOPES:", OIDC_SCOPES))
print("---------------------------------")

# ==============================================================
# UI Definition - Interactive Serology Plate Inspector
# ==============================================================

# --------------------------------------------------------------
# 1. HEADER
# --------------------------------------------------------------
header <- dashboardHeader(
  title = "Interactive Serology Plate Inspector",
  titleWidth = 350
)

# --------------------------------------------------------------
# 2. SIDEBAR
# --------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 350,

  # User panel and project info

  uiOutput("userpanel"),
  uiOutput("project_info"),

  # Primary navigation menu
  sidebarMenu(
    id = "main_tabs",
    menuItem("Home", tabName = "home_page", icon = icon("home")),
    menuItem("Create, Add, and Load Projects", tabName = "manage_project_tab", icon = icon("chart-line"))
  ),

  # Study selector and dynamic study menu
  uiOutput("main_study_selector"),
  uiOutput("study_sidebar")
)

# --------------------------------------------------------------
# 3. BODY
# --------------------------------------------------------------
body <- dashboardBody(
  style = "min-height: 100vh; background-color: whitesmoke;",

  # ----------------------------------------------------------
  # 3a. ALL CSS STYLES (Consolidated)
  # ----------------------------------------------------------
  tags$head(
    tags$style(HTML("
      /* ==============================================
         LAYOUT: Fixed sidebar and header
         ============================================== */
      .main-sidebar {
        position: fixed;
        top: 0;
        left: 0;
        height: 100%;
        width: 350px;
      }

      .main-header {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        z-index: 1000;
        width: 100% !important;
      }

      .content-wrapper {
        min-height: 100vh;
        padding-top: 50px;
        margin-left: 350px;
        background-color: whitesmoke;
      }

      /* Hide footer, adjust margin when sidebar collapsed */
      .main-footer {
        display: none;
      }

      .sidebar-collapse .main-footer {
        margin-left: 0 !important;
      }

      /* ==============================================
         PANELS: Collapse panel backgrounds
         ============================================== */
      #StandardCurveCollapse,
      #addProjectDocumentation,
      #createNewProjectCollapse,
      .panel-collapse {
        background-color: whitesmoke;
      }

      /* ==============================================
         CONTAINERS: Width and overflow for data panels
         ============================================== */
      #StandardCurveCollapse,
      #da_subject_level_inspection,
      #da_datasets,
      #main_dilution_linearity_collapse,
      #linearity_stats,
      #standard_curve_model_fit,
      #gated_samples,
      .table-container {
        width: 75vw;
        overflow-x: auto;
      }

      /* ==============================================
         BUTTONS: Standard curve button container
         ============================================== */
      #StandardCurveCollapse .button-container {
        width: 75vw;
        overflow-x: auto;
        gap: 300px;
      }

      #StandardCurveCollapse .button-container .btn {
        white-space: nowrap;
      }

      /* ==============================================
         NOTIFICATIONS: Custom styled notifications
         ============================================== */
      .big-notification {
        width: 450px !important;
        font-size: 16px !important;
        padding: 25px !important;
      }

      .success-notification {
        width: 400px !important;
        font-size: 14px !important;
        padding: 20px !important;
        border-left: 4px solid #28a745 !important;
      }

      .warning-notification {
        width: 400px !important;
        font-size: 14px !important;
        padding: 20px !important;
        border-left: 4px solid #ffc107 !important;
      }

      .error-notification {
        width: 450px !important;
        font-size: 14px !important;
        padding: 20px !important;
        border-left: 4px solid #dc3545 !important;
      }

         /* -----------------------------------------------------------------
         .dots – the span that lives inside the notification text
         ----------------------------------------------------------------- */
      .dots {
        display: inline-block;
        width: 1.2em;               /* keep the width constant while animating */
        text-align: left;
        overflow: hidden;
        vertical-align: bottom;
        animation: dotPulse 1.5s steps(3, start) infinite;
      }

      /* -----------------------------------------------------------------
         The actual animation: 3 steps, each step reveals one more dot.
         ----------------------------------------------------------------- */
      @keyframes dotPulse {
        0%   { content: '';          }   /* nothing  */
        33%  { content: '.';         }   /* one dot  */
        66%  { content: '..';        }   /* two dots */
        100% { content: '...';       }   /* three    */
      }

      /* -----------------------------------------------------------------
         Because ::after pseudo‑elements are easier to animate, we create a
         helper class that actually shows the dots via ::after.
         ----------------------------------------------------------------- */
      .dots::after {
        content: '';
        animation: dotPulse 1.5s steps(3, start) infinite;
      }
      
     /* Force horizontal layout */
        .radio-card-group .shiny-options-group{
        display:flex               !important;
        gap:15px                    !important;
        flex-wrap:nowrap           !important;   /* NEVER wrap automatically */
        overflow-x:auto;                       /* enable horizontal scroll on small screens */
          overflow: visible !important;   /* stop clipping the second card */

      }

    /* Each option becomes a card */
    .radio-card-group .shiny-options-group .radio{
        flex:0 0 370px            !important;   /* 0 grow, 0 shrink, 260 px basis */
        min-width:370px           !important;   /* browsers that ignore flex‑basis */
        margin:0                  !important;
      }
      
    /* Hide default spacing */
    .radio-card-group .shiny-options-group .radio label{
      width:100%                !important;
      cursor:pointer            !important;
      display:block             !important;
      margin-bottom: 15px       !important;
    }

.radio-card-group > .form-group > label {
  display: block;
  margin-bottom: 14px;  /* increase this as needed */
}

    /* Card styling */
        .radio-card{
      border:1px solid #ddd;
      border-radius:10px;
      padding:15px;
      background:#f8f9fa;
      transition:all .2s ease;
      display:flex;
      flex-direction:column;
      justify-content:space-between;
    }

    .radio-card:hover {
      border-color: #0d6efd;
      background-color: #eef4ff;
    }

    .radio-title {
      font-weight: 600;
      font-size: 15px;
      margin-bottom: 5px;
    }

    .radio-desc {
      font-size: 13px;
      color: #555;
      max-width:100%
    }

    /* Selected state */
    .radio-card-group input[type='radio']:checked + div {
      border: 2px solid #0d6efd;
      background-color: #e7f1ff;
    }
    "))
  ),

  # ----------------------------------------------------------
  # 3b. BODY CONTENT
  # ----------------------------------------------------------
  uiOutput("landing_page_ui"),
  uiOutput("body_content_ui")
)

# --------------------------------------------------------------
# 4. JAVASCRIPT HANDLERS
# --------------------------------------------------------------
# Consolidated JS for authentication, cookies, and user activity

js_handlers <- tags$script(HTML("
  $(document).on('shiny:connected', function(event) {
    console.log('JS: Shiny connected, setting up custom message handlers...');

    // ------------------------------------------------
    // REDIRECT: Navigate to a new URL
    // ------------------------------------------------
    Shiny.addCustomMessageHandler('redirect', function(url) {
      console.log('JS: Redirecting to:', url);
      if (url) {
        window.location.href = url;
      } else {
        console.error('JS: Null URL for redirect.');
      }
    });

    // ------------------------------------------------
    // QUERY STRING: Update URL without page reload
    // ------------------------------------------------
    Shiny.addCustomMessageHandler('updateQueryString', function(query) {
      console.log('JS: Updating query string to:', query);
      const newUrl = window.location.pathname + query + window.location.hash;
      window.history.replaceState({}, document.title, newUrl);
    });

    // ------------------------------------------------
    // COOKIES: Set authentication state cookie
    // ------------------------------------------------
    Shiny.addCustomMessageHandler('setAuthStateCookie', function(data) {
      if (data && data.state) {
        console.log('JS: Setting auth state cookie:', data.state);
        const cookieString = `shiny_oidc_state=${encodeURIComponent(data.state)}; path=/; SameSite=Lax`;
        document.cookie = cookieString;
        console.log('JS: Cookie string set:', cookieString);
      } else {
        console.error('JS: No state received to set cookie.');
      }
    });

    // ------------------------------------------------
    // COOKIES: Get and clear authentication state cookie
    // ------------------------------------------------
    Shiny.addCustomMessageHandler('getAuthStateCookie', function(message) {
      console.log('JS: getAuthStateCookie handler triggered.');
      const cookies = document.cookie.split('; ');
      let stateFromCookie = null;
      const cookieName = 'shiny_oidc_state=';

      for (let i = 0; i < cookies.length; i++) {
        let cookie = cookies[i].trim();
        if (cookie.startsWith(cookieName)) {
          stateFromCookie = decodeURIComponent(cookie.substring(cookieName.length));
          console.log('JS: Found state in cookie:', stateFromCookie);
          // Delete the cookie after reading
          document.cookie = `${cookieName}; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT; SameSite=Lax`;
          console.log('JS: Deleted auth state cookie.');
          break;
        }
      }

      if (!stateFromCookie) {
        console.warn('JS: Auth state cookie not found or already deleted.');
      }

      Shiny.setInputValue('authStateCookieValue', stateFromCookie, {priority: 'event'});
    });

    // ------------------------------------------------
    // RELOAD: Force page reload
    // ------------------------------------------------
    Shiny.addCustomMessageHandler('reloadPage', function(message) {
      console.log('JS: Received reloadPage message. Reloading...');
      window.location.reload();
    });

    console.log('JS: Finished setting up custom message handlers.');
  });
"))

js_inactivity_timer <- tags$script(HTML("
  // ------------------------------------------------
  // INACTIVITY TIMER: Log out user after 15 minutes
  // ------------------------------------------------
  (function() {
    const inactivityTimeout = 15 * 60 * 1000; // 15 minutes in milliseconds
    let timeout;

    function resetTimer() {
      clearTimeout(timeout);
      timeout = setTimeout(logout, inactivityTimeout);
    }

    function logout() {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('user_is_inactive', true, {priority: 'event'});
      }
    }

    // Reset timer on user activity
    window.addEventListener('mousemove', resetTimer, {passive: true});
    window.addEventListener('mousedown', resetTimer, {passive: true});
    window.addEventListener('keypress', resetTimer, {passive: true});
    window.addEventListener('touchmove', resetTimer, {passive: true});

    // Start the timer when the app loads
    resetTimer();
  })();
"))

# --------------------------------------------------------------
# 5. MAIN UI ASSEMBLY
# --------------------------------------------------------------
ui <- tagList(
  tags$head(
    # Favicon
    tags$link(rel = "shortcut icon", href = "greenicon.ico"),

    # JavaScript handlers
    js_handlers,
    js_inactivity_timer
  ),

  # Dashboard page structure
  dashboardPage(
    title = "Interactive Serology Plate Inspector",
    skin = "black",
    header = header,
    sidebar = sidebar,
    body = body
  )
)

# --------------------------------------------------------------
# 6. AUTHENTICATED BODY CONTENT FUNCTION
# --------------------------------------------------------------
# This function returns the UI structure shown after successful login

authenticated_body_content <- function() {
  fluidPage(
    useShinyjs(),
    useShinyFeedback(),
    uiOutput("body_tabs")
  )
}


# ==============================================================
# USAGE EXAMPLES FOR CUSTOM NOTIFICATIONS
# ==============================================================
#
# In your server code, use these notification styles:
#
# # Large notification
# showNotification(
#   div(class = "big-notification", "This is an important message!"),
#   duration = 5
# )
#
# # Success notification
# showNotification(
#   div(class = "success-notification", "Operation completed successfully!"),
#   type = "message",
#   duration = 4
# )
#
# # Warning notification
# showNotification(
#   div(class = "warning-notification", "Please review before proceeding."),
#   type = "warning",
#   duration = 5
# )
#
# # Error notification
# showNotification(
#   div(class = "error-notification", "An error occurred. Please try again."),
#   type = "error",
#   duration = NULL  # Requires manual dismissal
# )
# ==============================================================


# ==============================================================
# 3. Server Function
# ==============================================================
server <- function(input, output, session) {

  is_local_dev <- function() {
    Sys.getenv("LOCAL_DEV", unset = "0") == "1"
  }



  message("Shiny session started.")

  # --- Authentication Reactive Values from HEAD ---
  user_data <- reactiveVal(
    if (is_local_dev()) {
      list(
        is_authenticated = TRUE,
        email = local_email_user,   # Or any test user
        name = "Local Dev User",
        id_token = "FAKE_ID_TOKEN_FOR_DEV"
      )
    } else {
      list(is_authenticated = FALSE)
    }
  )

  jwks_cache <- reactiveVal(NULL)
  session$userData$callbackCode <- NULL
  session$userData$callbackState <- NULL
  session$userData$processedOIDCState <- FALSE


  # --- OIDC Authentication Logic (from HEAD) ---
  observe({
    query_params <- parseQueryString(session$clientData$url_search)
    if (!is.null(query_params$code) && !is.null(query_params$state) && is.null(session$userData$callbackCode)) {
      message("OIDC Callback received (code/state present & not yet stored).")
      session$userData$callbackCode <- query_params$code
      session$userData$callbackState <- query_params$state
      session$userData$processedOIDCState <- FALSE
      message(paste("Stored session$userData$callbackState:", session$userData$callbackState))
      message("Sending JS msg: getAuthStateCookie")
      session$sendCustomMessage("getAuthStateCookie", list())

      # *** ADD A SMALL DELAY HERE ***
      message("Delaying JS msg: getAuthStateCookie to allow client to initialize.")
      shinyjs::delay(150, {
        session$sendCustomMessage("getAuthStateCookie", list())
      })
    } else if (!is.null(query_params$error)) {
      warning(paste("OIDC Error on callback:", query_params$error))
      session$sendCustomMessage("updateQueryString", "?")
      session$userData$callbackCode <- NULL
      session$userData$callbackState <- NULL
      session$userData$processedOIDCState <- FALSE
    }
  })


  observeEvent(input$authStateCookieValue, {
    message(paste("--- input$authStateCookieValue observer triggered. Value:", input$authStateCookieValue %||% "NULL", "---"))

    # Immediately exit if this state has already been processed.
    if (isTRUE(session$userData$processedOIDCState)) {
      message("Ignoring cookie value: State already processed for this login attempt.")
      return()
    }

    # Gather all necessary values first.
    expected_state_from_cookie <- input$authStateCookieValue
    received_state_from_url <- session$userData$callbackState
    authorization_code <- session$userData$callbackCode

    # Validate that we have everything we need to proceed.
    if (is.null(expected_state_from_cookie) || !nzchar(expected_state_from_cookie) ||
        is.null(received_state_from_url) || is.null(authorization_code)) {
      message("Ignoring cookie value: Missing required auth data in session or from cookie.")
      return()
    }

    message(paste("Comparing State from URL (session$userData):", received_state_from_url))
    message(paste("With State from Cookie (input$):", expected_state_from_cookie))

    # Perform the state validation.
    if (identical(expected_state_from_cookie, received_state_from_url)) {
      message("State validated successfully using cookie.")

      # Mark state as processed *before* the long-running task.
      session$userData$processedOIDCState <- TRUE

      # Clean the URL now.
      session$sendCustomMessage("updateQueryString", "?")

      # Perform the token exchange and get the final user data.
      # This function will handle its own errors and notifications.
      final_user_data <- exchange_code_for_token(authorization_code)

      # **This is the most crucial part.**
      # We now update the central user_data() reactive value.
      # This single, clear update will trigger all downstream UI changes.
      if (!is.null(final_user_data) && isTRUE(final_user_data$is_authenticated)) {
        user_data(final_user_data)
      } else {
        # If the exchange failed, ensure we are in a logged-out state.
        user_data(list(is_authenticated = FALSE))
        warning("Token exchange process failed to return an authenticated user.")
        showNotification("Authentication failed. Please try logging in again.", type = "error", duration = 10)
      }

    } else {
      warning("Invalid state parameter! Cookie/URL mismatch.")
      showNotification("Authentication error (state validation failed). Please try again.", type = "error", duration = 10)
      session$sendCustomMessage("updateQueryString", "?")
      # Fully reset session state on failure
      session$userData$callbackCode <- NULL
      session$userData$callbackState <- NULL
      session$userData$processedOIDCState <- FALSE
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  exchange_code_for_token <- function(auth_code) {
    message("Attempting token exchange...")
    tryCatch({
      token_req <- httr2::request(DEX_TOKEN_ENDPOINT) |>
        httr2::req_method("POST") |>
        httr2::req_auth_basic(username = DEX_CLIENT_ID, password = DEX_CLIENT_SECRET) |>
        httr2::req_body_form(
          grant_type = "authorization_code",
          code = auth_code,
          redirect_uri = APP_REDIRECT_URI,
          client_id = DEX_CLIENT_ID
        ) |>
        httr2::req_headers(
          "Accept" = "application/json",
          "Content-Type" = "application/x-www-form-urlencoded"
        )
      resp <- httr2::req_perform(token_req)
      token_data <- httr2::resp_body_json(resp)
      if (is.null(token_data$id_token)) stop("No id_token received from token endpoint.")
      validated_payload <- validate_id_token(token_data$id_token)
      if (!is.null(validated_payload)) {
        final_user_data <- get_user_information(
          access_token = token_data$access_token,
          id_token_payload = validated_payload,
          raw_id_token = validated_payload$raw_id_token_string
        )

        #user_data(final_user_data)
        return(final_user_data)
      } else {
        warning("ID Token validation failed (returned NULL). User not authenticated.")
        user_data(list(is_authenticated = FALSE))
        showNotification("Authentication failed: Invalid identity.", type = "error")

        return(NULL)

      }
    }, error = function(e) {
      warning(paste("Error exchanging code:", e$message))
      if (inherits(e, "httr2_http") && !is.null(e$body)) {
        try({ error_body <- httr2::resp_body_json(e$resp); warning("Dex error response body:"); print(error_body) }, silent = TRUE)
      }
      user_data(list(is_authenticated = FALSE))
      showNotification("Error during login token exchange. Check logs.", type = "error")
      return(NULL)
    })
  }



  validate_id_token <- function(id_token) {
    message("Validating ID token...")

    # Internal function to perform validation against a given key set
    perform_validation <- function(token, keys) {
      if (is.null(keys) || length(keys) == 0) return("refetch_jwks") # Signal to refetch if cache is empty
      tryCatch({
        parts <- strsplit(token, ".", fixed = TRUE)[[1]]
        header_char <- rawToChar(jose::base64url_decode(parts[1]))
        header <- jsonlite::fromJSON(header_char, simplifyVector = FALSE)
        token_kid <- header$kid
        if (is.null(token_kid)) stop("Token header missing 'kid'.")

        public_key_jwk <- NULL
        for (key in keys) { if (!is.null(key$kid) && key$kid == token_kid) { public_key_jwk <- key; break; }}
        if (is.null(public_key_jwk)) stop("kid_not_found")

        jwk_json_string <- jsonlite::toJSON(public_key_jwk, auto_unbox = TRUE)
        public_key <- jose::read_jwk(jwk_json_string)
        payload <- jose::jwt_decode_sig(jwt = token, pubkey = public_key)

        current_time <- as.numeric(Sys.time())
        if (!identical(payload$iss, DEX_ISSUER)) stop("Invalid issuer.")
        aud_ok <- if(is.list(payload$aud)) DEX_CLIENT_ID %in% payload$aud else identical(payload$aud, DEX_CLIENT_ID)
        if (!aud_ok) stop("Invalid audience.")
        if (payload$exp < (current_time - 60)) stop("Token expired.")

        payload$raw_id_token_string <- token
        message("Token validation successful.")
        return(payload)
      }, error = function(e) {
        if (grepl("kid_not_found", e$message, fixed = TRUE)) {
          warning(paste("Token 'kid' [", token_kid, "] not found in the current JWKS set.", sep=""))
          return("refetch_jwks")
        }
        warning(paste("Token validation error:", e$message))
        return(NULL)
      })
    }

    # 1. First attempt with cached JWKS
    payload <- perform_validation(id_token, jwks_cache())

    # 2. If validation failed because the key was not found, refetch JWKS and retry
    if (is.character(payload) && payload == "refetch_jwks") {
      message("Re-fetching JWKS and retrying validation...")
      fresh_keys <- get_jwks(DEX_JWKS_ENDPOINT)
      if (!is.null(fresh_keys)) {
        jwks_cache(fresh_keys) # Update cache
        payload <- perform_validation(id_token, fresh_keys)
      } else {
        warning("Failed to fetch fresh JWKS. Authentication aborted.")
        return(NULL)
      }
    }

    if (!is.list(payload)) {
      warning("ID Token validation failed permanently.")
      return(NULL)
    }

    return(payload)
  }

  get_user_information <- function(access_token, id_token_payload, raw_id_token) {
    message("--- Entering get_user_information ---")
    user_info <- list(
      email = id_token_payload[["email"]],
      name = id_token_payload[["name"]],
      id_token = raw_id_token,
      is_authenticated = TRUE
    )
    message(paste("  > User info list created. Email:", user_info$email %||% "NULL"))
    return(user_info)
  }

  observeEvent(input$user_is_inactive, {
    if (isTRUE(input$user_is_inactive)) {
      message("User has been inactive for 15 minutes. Triggering logout.")
      # This triggers the exact same logic as the logout button
      # Using shinyjs::click is a robust way to avoid duplicating code
      shinyjs::click("logout_button")
    }
  })

  observeEvent(input$logout_button, {

    if (is_local_dev()) {
      user_data(list(
        is_authenticated = TRUE,
        email = "dev_user@dartmouth.edu",
        name = "Local Dev User",
        id_token = "FAKE_ID_TOKEN_FOR_DEV"
      ))
      showNotification("DEV mode: Simulated logout (still logged in as dev user).", type = "message")
      return()
    }

    message("Logout requested by user.")

    ud <- user_data()
    user_data(list(is_authenticated = FALSE)) # Immediately clear local auth state
    session$userData$app_logic_initialized <- FALSE

    if (!is.null(DEX_LOGOUT_ENDPOINT) && nzchar(DEX_LOGOUT_ENDPOINT) && !is.null(ud$id_token)) {
      # Redirect to the OIDC provider's logout endpoint for a clean session termination
      logout_url <- httr2::request(DEX_LOGOUT_ENDPOINT) |>
        httr2::req_url_query(
          id_token_hint = ud$id_token,
          post_logout_redirect_uri = APP_REDIRECT_URI
        ) |>
        purrr::pluck("url")

      message(paste("Redirecting to Dex logout URL:", logout_url))
      session$sendCustomMessage("redirect", logout_url)

    } else {
      # Fallback if logout endpoint isn't configured or token is missing
      message("DEX_LOGOUT_ENDPOINT not found or token missing. Performing simple page reload.")
      session$sendCustomMessage("reloadPage", list())
    }
  })

  # --- UI Rendering Logic (from HEAD) ---

  output$body_content_ui <- renderUI({
    ud <- user_data()
    if (!is.null(ud) && isTRUE(ud$is_authenticated)) {
      message("Rendering authenticated body content.")
      cat(input$main_tabs_panel)
      cat(input$readxMap_study_accession )
      authenticated_body_content()
    } else {
      message("Rendering login button page.")
      fluidPage(
        style = "display: flex; justify-content: center; align-items: center; height: 80vh;",
        actionButton("login_button", "Login with Dartmouth Dex", class = "btn-primary btn-lg")
      )
    }
  })

  output$userpanel <- renderUI({
    ud <- user_data()
    if (isTRUE(ud$is_authenticated)) {
      # Robustly check for a valid email string before displaying it
      user_email_display <- if (!is.null(ud$email) && is.character(ud$email) && length(ud$email) == 1 && nzchar(ud$email)) {
        ud$email
      } else {
        "N/A"
      }

      tagList(
        div(style="padding: 10px; text-align: center; color: white;", p(strong("User:")), p(user_email_display)),
        div(style="padding: 10px; text-align: center;", actionButton("logout_button", "Logout", class = "btn-danger", width = "80%"))
      )
    } else {
      div(style = "padding: 20px; text-align: center; color: grey;", "Please log in")
    }
  })

  # --- Login Button Action (from HEAD, with debug messages added) ---
  observeEvent(input$login_button, {

    if (is_local_dev()) {
      showNotification("DEV mode: Bypassing login.", type = "message")
      user_data(list(
        is_authenticated = TRUE,
        email = "dev_user@dartmouth.edu",
        name = "Local Dev User",
        id_token = "FAKE_ID_TOKEN_FOR_DEV"
      ))
      return()
    }

    message("Login button clicked. Starting auth process...")
    req(DEX_AUTH_ENDPOINT, DEX_CLIENT_ID, APP_REDIRECT_URI, OIDC_SCOPES)
    if (!requireNamespace("openssl")) stop("openssl required")
    state <- openssl::rand_bytes(16) |> jose::base64url_encode()
    message(paste("Generated state:", state))
    message("Sending JS msg: setAuthStateCookie")
    session$sendCustomMessage("setAuthStateCookie", list(state = state))
    Sys.sleep(0.1)
    auth_url_req <- httr2::request(DEX_AUTH_ENDPOINT) |> httr2::req_url_query(response_type="code", client_id=DEX_CLIENT_ID, redirect_uri=APP_REDIRECT_URI, scope=OIDC_SCOPES, state=state)
    auth_url <- auth_url_req$url
    if (!is.null(auth_url) && nzchar(auth_url)) {
      message(paste("SUCCESS: Auth URL:", auth_url))
      message("Sending JS msg: redirect")
      session$sendCustomMessage("redirect", auth_url)
    } else {
      message("ERROR: Failed to build auth URL.")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # --- Core Application Logic - GATED EXECUTION (MERGED) ---
  observe({
    ud <- req(user_data())
    if (isTRUE(ud$is_authenticated) && !isTRUE(session$userData$app_logic_initialized)) {
      message("--- GATED LOGIC: Initializing Core App Logic... ---")
      session$userData$app_logic_initialized <- TRUE

      # <<< MERGE POINT: All of your teammate's server logic from `main` is placed here >>>
      shinyInput <- function(FUN, len, id, ...) {
        inputs <- character(len)
        for (i in seq_len(len)) {
          inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
        }
        inputs
      }

      shinyValue <- function(id, len) {
        unlist(lapply(seq_len(len), function(i) {
          value <- input[[paste0(id, i)]]
          if (is.null(value)) NA else value
        }))
      }

      conn <- get_db_connection()

      userWorkSpaceID <- reactiveVal(NULL)
      userProjectName <- reactiveVal("unknown")
      reactive_df_study_exp <- reactiveVal(NULL)
      study_choices_rv <- reactiveVal(NULL)
      experiment_choices_rv <- reactiveVal(NULL)
      currentuser <- reactiveVal("unknown user")
      usersession <- reactiveVal("unknown session")

      refresh_data_trigger <- reactiveVal(0) # trigger to refresh data
      refresh_experiment_trigger <- reactiveVal(0) # trigger to refresh the experiment list
      stored_plates_data <- reactiveValues()
      storedlong_plates_data <- reactiveValues()
      selected_studyexpplate <- reactiveValues()

      p_data <- reactiveValues()
      pa_data <- reactiveValues()

      processing_status <- reactiveVal(FALSE)
      job_status <- reactiveVal(NULL)
      tabRefreshCounter <- reactiveVal(list(view_files_tab = 0, import_tab = 0, manage_project_tab = 0))

      antigen_families_rv <- reactiveVal(NULL) #used in study configuration
      n_plates_standard_curve <- reactiveVal(NULL)
      mininum_dilution_count_boolean <- reactiveVal(NULL)
      luminex_features <- reactiveVal(c("Well", "Type", "Description", "Region", "Gate", "Total", "% Agg Beads", "Sampling Errors", "Rerun Status", "Device Error", "Plate ID", "Regions Selected", "RP1 Target", "Platform Heater Target", "Platform Temp (°C)", "Bead Map", "Bead Count", "Sample Size (µl)", "Sample Timeout (sec)", "Flow Rate (µl/min)", "Air Pressure (psi)", "Sheath Pressure (psi)", "Original DD Gates", "Adjusted DD Gates", "RP1 Gates", "User", "Access Level", "Acquisition Time", "acquisition_time", "Reader Serial Number", "Platform Serial Number", "Software Version", "LXR Library", "Reader Firmware", "Platform Firmware", "DSP Version", "Board Temp (°C)", "DD Temp (°C)", "CL1 Temp (°C)", "CL2 Temp (°C)", "DD APD (Volts)", "CL1 APD (Volts)", "CL2 APD (Volts)", "High Voltage (Volts)", "RP1 PMT (Volts)", "DD Gain", "CL1 Gain", "CL2 Gain", "RP1 Gain"))

      std_curve_data_model_fit <- reactiveVal()
      sample_data_model_fit <- reactiveVal()
      buffer_data_model_fit <- reactiveVal()
      # aggrigate_mfi_dilution <- reactiveVal(TRUE)
      # lower_threshold_rv <- reactiveVal(35)
      # upper_threshold_rv <- reactiveVal(50)
      # failed_well_criteria <- reactiveVal("lower")
      background_control_rv <- reactiveVal("ignored")
      reference_arm_rv <- reactiveVal()
      reference_arm <- reactiveVal(NULL)

      dilution_analysis_params_rv <- reactiveVal(NULL)
      last_saved_dilution_params <- reactiveVal(NULL)
      margin_table_reactive <- reactiveVal(NULL)
      da_filters_rv <- reactiveValues(selected = list(n_pass_dilutions = NULL, status = NULL, timeperiod = NULL))
      average_au_table_reactive <- reactiveVal(NULL)
      final_average_au_table_rv <- reactiveVal(NULL)
      updated_classified_merged_rv <- reactiveVal(NULL)
      updated_margin_antigen_rv <- reactiveVal(NULL)


      # UI handler
      upload_state_value <- reactiveValues(upload_state = NULL)
      previousTab <- reactiveVal()
      rv_value_button <- reactiveValues(valueButton = 0)
      header_rvdata <- reactiveValues()
      tab_counter <- reactiveVal(0) # for dynamic tabs
      manual_studies <- reactiveValues(entries = character(0))

      standard_rvdata <- reactiveValues()
      sample_rvdata <- reactiveValues()
      control_rvdata <- reactiveValues()
      buffer_rvdata <- reactiveValues()
      # UI handler - reloading of Modules
      reload_bead_count <- 0
      reload_sc_fit_mod_count <- 0
      reload_sc_summary_mod_count <- 0
      reload_sg_count <- 0
      reload_dil_lin_count <- 0
      #importing
      type_observers <- reactiveValues()
      list_of_dataframes <- reactiveVal(NULL)
      original_df_combined <- reactiveVal(NULL)

     ## Optimization
      optimization_refresh <- reactiveVal(0)
      optimization_ready <- reactiveVal(FALSE)
      # in plates tab split by nominal sample dilution
      split_by_nominal_dilution <- reactiveVal(FALSE)
      show_wavelength_subtraction  <- reactiveVal(FALSE)
      
      
      plate_data <- reactiveVal()
      sc_feature_select <- reactiveVal()
      header_info <- reactiveVal()
      current_type_p_tab <- reactiveVal()
      unique_plate_types <- reactiveVal()
      #imported_h_study <- reactiveVal()
      #imported_h_experiment <- reactiveVal()
      #imported_h_plate_id <- reactiveVal()
      #imported_h_plate_number <- reactiveVal()
      type_p_completed <- reactiveVal(FALSE)
     #type_x_completed <- reactiveVal()
      type_x_status <- reactiveVal(list(plate_exists = FALSE, n_record = 0))
      type_s_status <- reactiveVal(list(plate_exists = FALSE, n_record = 0))
      type_c_status <- reactiveVal(list(plate_exists = FALSE, n_record = 0))
      type_b_status <- reactiveVal(list(plate_exists = FALSE, n_record = 0))


      availableSheets <- reactiveVal()
      is_valid_plate <- reactiveVal()
      plate_validation_messages <- reactiveVal(NULL)
      inFile <- reactiveVal()
      xponent_plate_data <- reactiveVal()
      xponent_meta_data <- reactiveVal()
      generated_tabs <- reactiveVal()
      lumcsv_reactive <- reactiveVal()

      ## Layout sheets
      batch_plate_data <- reactiveVal() # all plates combined from folder upload
      batch_metadata <- reactiveVal() # store validated metadata for the batch
      experimentPath <- reactiveVal()
      inLayoutFile <- reactiveVal()
      avaliableLayoutSheets <- reactiveVal()
      layout_template_sheets <- reactiveVal(list())
      batch_data_list <- reactiveVal(list())
      bead_array_header_list <- reactiveVal(list())
      bead_array_plate_list <- reactiveVal(list())
      sample_dilution_plate_df <- reactiveVal(NULL)

      batch_validation_state <- reactiveVal(list(
        is_validated = FALSE,
        is_uploaded = FALSE,
        validation_time = NULL,
        upload_time = NULL,
        metadata_result = NULL,
        bead_array_result = NULL,
        data_stored = FALSE  # NEW: Track if data has been stored to prevent duplicates
      ))

      description_status <- reactiveVal(list(
        has_content = TRUE,
        has_sufficient_elements = TRUE,
        min_elements_found = 0,
        required_elements = 3,  # PatientID, TimePeriod, DilutionFactor at minimum
        checked = FALSE,
        message = NULL
      ))

      # Add these to prevent multiple executions
      batch_processing_state <- reactiveVal(list(
        is_processing = FALSE,
        last_layout_hash = NULL,
        last_validation_hash = NULL,
        last_upload_hash = NULL
      ))

      layout_upload_state <- reactiveVal(list(
        is_uploaded = FALSE,
        upload_time = NULL,
        current_study = NULL,
        current_experiment = NULL,
        processing = FALSE
      ))

      # Debounce flag for layout processing
      layout_processing_lock <- reactiveVal(FALSE)

      delete_confirmed <- reactiveVal(FALSE)
      
      # study_configuration import 
      config_upload_state <- reactiveVal(list(
        is_uploaded = FALSE,
        upload_time = NULL,
        user = NULL
      ))
      
      config_preview <- reactiveVal(NULL)
      config_preview_antigen <- reactiveVal(NULL)
   

      # layout_template_sheets <- reactiveValues(
      #   plate_id = NULL,
      #   plates_map = NULL,
      #   antigen_list = NULL
      # )


      #outliers
      outlierJobStatus <- reactiveVal(list())
      outlierUIRefresher <- reactiveVal(0)
      concentrationJobStatus <- reactiveVal(list())
      
      concentrationUIRefresher <- reactiveVal(0)
      is_batch_processing      <- reactiveVal(FALSE)
      concentrationUIRefresher <- reactiveVal(0)
      
      # Persists the user's scope choice across concentration_calc_df() invalidations.
      # Without this, renderUI re-creates the radio buttons on every refresh and snaps
      # back to the first choice ("study") while a long calculation is running.
      selected_scope <- reactiveVal("plate")
      
      
      # Tracks which scope+method combinations are currently pending
      # Format: list of named vectors, e.g. list(c(scope="study", method="mcmc_robust"))
      mcmc_pending_scopes <- reactiveVal(list())
      # Stores the current MCMC progress message for the badge tooltip
      mcmc_progress_msg    <- reactiveVal(NULL)
      # Path to the temp file used to pass progress from future → main session for MCMC
      mcmc_progress_file   <- reactiveVal(NULL)
      
      # # ── Interpolated async state (mirrors MCMC pattern) ──────────────────────
      #Tracks which scope+method combinations are currently pending for interpolation
      interp_pending_scopes <- reactiveVal(list())
      # Path to the temp file used to pass progress from future → main session for interpolation
      interp_progress_file  <- reactiveVal(NULL)
      # Stores the current interpolation progress message for the badge tooltip
      interp_progress_msg   <- reactiveVal(NULL)

      ### Sourcing all the application logic files from `main`
      source("user.R", local = TRUE)
      source("user_management.R", local = TRUE)
      source("helpers.R", local = TRUE)
      source("ui_handler.R", local = TRUE)
      source("study_configuration.R", local = TRUE)
      source("study_configuration_ui.R", local = TRUE)
      source("plate_management.R", local = TRUE)
      source("study_overview_functions.R", local = TRUE)
      source("study_overview_ui.R", local = TRUE)
      source("antigen_family_ui.R", local = TRUE)
      source("split_plates_nominal_sample_dilution.R", local = TRUE)
      source("load_previous_stored_data.R", local=TRUE)
      source("plate_validator_functions.R", local = TRUE)
      source("generate_layout_template_ref.R", local = TRUE)
      source("batch_layout_functions.R", local = TRUE)
      source("import_lumifile.R", local = TRUE)
      source("xPonentReader.R", local = TRUE)
      source("elisa_reader.R", local = TRUE)
      source("elisa_diagnostic.R", local = TRUE)
      source("elisa_wavelength_subtraction.R", local = TRUE)
      # source("segment_reader.R", local = TRUE)

      source("plate_norm_server.R", local = TRUE)
      source("bead_count_analysis_ui.R", local = TRUE)
      source("bead_count_functions.R", local = TRUE)
      source("bead_count_controls_ui.R", local = TRUE)
      source("dilution_standards_controls_ui.R", local = TRUE)
      source("blank_control_ui.R", local = TRUE)
      source('propagate_functions.R', local = TRUE)

      source("std_curver_ui.R", local = TRUE)
      source("std_curve_functions.R", local = TRUE)
      source("db_functions.R", local = TRUE)
      source("model_functions.R", local = TRUE)
      # source("se_x_robust_fix.R", local = TRUE)
      source("plot_functions.R", local = TRUE)
      source("bayes_concentration_functions.R", local = T)
      source("batch_fit_functions.R", local = TRUE)

      source("std_curver_summary_ui.R", local = TRUE)
      source("std_curver_summary_functions.R", local = TRUE)

      source("outliers.R", local = TRUE)
      source("outlier_ui1.R", local = TRUE)
      source("dilution_analysis_ui.R", local = TRUE)
      source("dilutional_linearity_ui.R", local = TRUE)
      source("revised_dilution_analysis_functions.R", local = TRUE)
      source("dilution_analysis_parameters_ui.R", local = TRUE)
      source("dilution_linearity_functions.R", local = TRUE)
      source("subgroup_function.R", local = TRUE)
      source("subgroup_detection_ui.R", local = TRUE)
      source("reference_arm_ui.R", local = TRUE)
      source("subgroup_detection_summary_ui.R", local = TRUE)
      source("subgroup_summary_functions.R", local = TRUE)

    } else if (!isTRUE(ud$is_authenticated)) {
      if (!is.null(session$userData$app_logic_initialized) && isTRUE(session$userData$app_logic_initialized)) {
        message("--- GATED LOGIC: User logged out / unauthenticated. Resetting init flag. ---")
      }
      session$userData$app_logic_initialized <- FALSE
    }
  })

  message("Main server function setup complete (excluding core app logic until auth).")
}

# ==============================================================
# 4. Run App
# ==============================================================
options(shiny.host = "127.0.0.1")
options(shiny.port = 8080)
shinyApp(ui = ui, server = server)
