# Python Skin Listing Monitor  (BL5314 group impact project, "Show Me the Source")
# ---------------------------------------------------------------------------------
# A small R Shiny app that ranks online listings that may sell python skin goods,
# flags listings that show no proof of origin, and records a reviewer's decision.
#
# How to run in RStudio:
#   1. install.packages("shiny")            # only package needed (R 4.2 or newer)
#   2. Open this file and click "Run App"   # or: shiny::runApp(".")
#
# Input: a CSV with columns  id, platform, date, account, text, price
# The file data/sample_listings.csv contains FICTIONAL test listings only.
#
# Important: a flag is NOT proof that a listing is illegal. The app only sorts
# listings for a human reviewer, who decides what to do.

library(shiny)

# ---- 1. Keyword lists (edit these, or edit them live in the "Keywords" tab) ----
default_terms <- list(
  species = c("python", "reticulated", "burmese", "malayopython", "bivittatus",
              "蟒", "蟒蛇", "网纹蟒", "sanca", "ular sawa", "piton"),
  product = c("skin", "leather", "handbag", "bag", "wallet", "belt", "clutch",
              "sneakers", "shoes", "tote", "皮", "包", "钱包", "kulit", "tas"),
  sale    = c("ready stock", "pm", "dm", "whatsapp", "price", "wholesale", "selling",
              "sgd", "cash", "现货", "私聊", "价格", "转让", "harga", "murah"),
  proof   = c("cites", "permit", "skin tag", "tag id", "traceability",
              "许可证", "追溯", "izin", "sertifikat cites"),
  weak    = c("certificate of authenticity", "brand card", "dust bag", "sustainable"),
  exclude = c("programming", "developer", "pandas", "numpy", "django", "flask", "course",
              "print", "faux", "pu leather", "vegan", "rehoming", "tank", "仿", "印花")
)

# ---- 2. Matching helpers ----
escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

# Latin terms must match whole words (so "pm" does not match "pmc", and "bag"
# does not match "cabbage"). Chinese terms are matched as plain text.
term_pattern <- function(term) {
  if (grepl("^[ -~]+$", term)) {
    paste0("(^|[^a-z])", escape_regex(tolower(term)), "($|[^a-z])")
  } else {
    escape_regex(term)
  }
}

find_terms <- function(text, terms) {
  if (length(terms) == 0) return(character(0))
  t <- tolower(text)
  hit <- vapply(terms, function(k) grepl(term_pattern(k), t, perl = TRUE), logical(1))
  terms[hit]
}

parse_terms <- function(s) {
  x <- trimws(unlist(strsplit(s, ",")))
  x[nzchar(x)]
}

# ---- 3. Scoring ----
# score = 2 (species) + 2 (product) + 1 (sale cue) + 2 (no strong proof) - 6 (excluded)
score_listing <- function(text, terms) {
  h <- lapply(terms, function(v) find_terms(text, v))
  sp <- length(h$species) > 0; pr <- length(h$product) > 0
  sa <- length(h$sale) > 0;    pf <- length(h$proof) > 0
  ex <- length(h$exclude) > 0
  score <- 2 * sp + 2 * pr + 1 * sa + 2 * (sp && pr && !pf) - 6 * ex
  priority <- if (ex || !(sp && pr)) "P3 - not relevant" else
              if (!pf && score >= 7) "P1 - no proof, sale cues" else
              if (!pf) "P2 - no proof" else "P3 - proof mentioned"
  proof_level <- if (pf) "Partial or strong (check documents)" else
                 if (length(h$weak) > 0) "Weak (brand claim only)" else "None shown"
  reasons <- c(
    if (sp) paste0("species: ", paste(h$species, collapse = ", ")),
    if (pr) paste0("product: ", paste(h$product, collapse = ", ")),
    if (sa) paste0("sale cue: ", paste(h$sale, collapse = ", ")),
    if (pf) paste0("proof: ", paste(h$proof, collapse = ", ")),
    if (length(h$weak) > 0) paste0("weak claim: ", paste(h$weak, collapse = ", ")),
    if (ex) paste0("excluded: ", paste(h$exclude, collapse = ", "))
  )
  list(score = score, priority = priority, proof = proof_level,
       reasons = paste(reasons, collapse = " | "), hits = unlist(h[c("species", "product", "sale", "proof")]))
}

highlight <- function(text, words) {
  out <- htmltools::htmlEscape(text)
  for (w in unique(words)) {
    out <- gsub(paste0("(", escape_regex(w), ")"), "<mark>\\1</mark>", out,
                ignore.case = TRUE, perl = TRUE)
  }
  out
}

# ---- 4. User interface ----
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#F8F8F1;color:#1C2318}
    .well{background:#FFFFFA;border-color:#D6D8CB}
    mark{background:#F3DE9A;padding:0 2px}
    .p1{color:#B23A2A;font-weight:700} .p2{color:#8A6414;font-weight:700}
    .box{background:#FFFFFA;border:1px solid #D6D8CB;border-radius:10px;padding:14px;margin-bottom:12px}
  "))),
  titlePanel("Python Skin Listing Monitor"),
  p(em("Sorts listings for human review. A flag is not proof of illegal trade.")),
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("file", "Upload listings CSV", accept = ".csv"),
      helpText("Columns: id, platform, date, account, text, price.",
               "Without a file, the fictional sample data is used."),
      checkboxInput("hide_p3", "Hide P3 (not relevant or proof shown)", TRUE),
      downloadButton("dl_queue", "Export queue (CSV)"),
      br(), br(),
      downloadButton("dl_decisions", "Export decisions (CSV)")
    ),
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Queue", br(), uiOutput("summary"), tableOutput("queue")),
        tabPanel("Case review", br(),
          selectInput("case", "Listing", choices = NULL),
          uiOutput("case_box"),
          radioButtons("decision", "Your decision", c("Escalate", "Review later", "Dismiss"), inline = TRUE),
          selectInput("reason", "Reason", c("No proof of origin", "Possible wild-caught sold as captive-bred",
                                            "Proof looks complete", "Not python skin (false positive)", "Other")),
          textAreaInput("note", "Note for the team (optional)", rows = 2),
          actionButton("save", "Save decision", class = "btn-primary"),
          h4("Saved decisions"), tableOutput("decisions")),
        tabPanel("Keywords", br(),
          helpText("Comma-separated. Changes apply at once. Add new slang or local names here."),
          lapply(names(default_terms), function(n)
            textAreaInput(paste0("kw_", n), n, paste(default_terms[[n]], collapse = ", "), rows = 2, width = "100%"))),
        tabPanel("Performance", br(),
          helpText("After review, this shows how many flags were useful, so the team can fix keywords."),
          tableOutput("perf"), tableOutput("false_terms"))
      )
    )
  )
)

# ---- 5. Server logic ----
server <- function(input, output, session) {
  listings <- reactive({
    path <- if (is.null(input$file)) file.path("data", "sample_listings.csv") else input$file$datapath
    d <- read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    req(all(c("id", "text") %in% names(d)))
    d
  })

  terms <- reactive({
    t <- lapply(names(default_terms), function(n) {
      v <- input[[paste0("kw_", n)]]
      if (is.null(v)) default_terms[[n]] else parse_terms(v)
    })
    setNames(t, names(default_terms))
  })

  scored <- reactive({
    d <- listings(); tm <- terms()
    s <- lapply(d$text, score_listing, terms = tm)
    d$score    <- vapply(s, `[[`, numeric(1), "score")
    d$priority <- vapply(s, `[[`, character(1), "priority")
    d$proof    <- vapply(s, `[[`, character(1), "proof")
    d$reasons  <- vapply(s, `[[`, character(1), "reasons")
    d[order(substr(d$priority, 1, 2), -d$score), ]
  })

  queue <- reactive({
    q <- scored()
    if (isTRUE(input$hide_p3)) q <- q[!startsWith(q$priority, "P3"), ]
    q
  })

  output$summary <- renderUI({
    s <- scored()
    tags$p(strong(nrow(s)), " listings scanned . ",
           span(class = "p1", sum(startsWith(s$priority, "P1")), " P1"), " . ",
           span(class = "p2", sum(startsWith(s$priority, "P2")), " P2"), " . ",
           sum(startsWith(s$priority, "P3")), " P3")
  })

  output$queue <- renderTable({
    q <- queue()
    q[, intersect(c("priority", "id", "platform", "date", "price", "proof", "score", "reasons"), names(q))]
  }, striped = TRUE, spacing = "s")

  observe({
    q <- queue()
    updateSelectInput(session, "case", choices = setNames(q$id, paste(q$id, "-", q$priority)))
  })

  output$case_box <- renderUI({
    req(input$case)
    r <- scored()[scored()$id == input$case, ]
    s <- score_listing(r$text, terms())
    div(class = "box",
      h4(r$id, " . ", r$platform, " . ", r$date),
      p(strong("Account: "), r$account, "   ", strong("Price: "), r$price),
      p(HTML(highlight(r$text, s$hits))),
      p(strong("Proof of origin: "), s$proof),
      p(strong("Why flagged: "), s$reasons))
  })

  decisions <- reactiveVal(data.frame(id = character(), decision = character(), reason = character(),
                                      note = character(), time = character(), stringsAsFactors = FALSE))
  observeEvent(input$save, {
    req(input$case)
    d <- decisions()
    d <- d[d$id != input$case, ]
    decisions(rbind(d, data.frame(id = input$case, decision = input$decision, reason = input$reason,
                                  note = input$note, time = format(Sys.time(), "%Y-%m-%d %H:%M"),
                                  stringsAsFactors = FALSE)))
    showNotification(paste("Saved decision for", input$case), type = "message")
  })
  output$decisions <- renderTable(decisions())

  output$perf <- renderTable({
    d <- decisions(); req(nrow(d) > 0)
    data.frame(
      `Flags reviewed` = nrow(d),
      `Useful (escalate or review)` = sum(d$decision != "Dismiss"),
      `False positives (dismissed)` = sum(d$decision == "Dismiss"),
      `Share useful` = sprintf("%.0f%%", 100 * mean(d$decision != "Dismiss")),
      check.names = FALSE)
  })

  # Which matched words keep appearing in dismissed listings? These are candidates for the exclude list.
  output$false_terms <- renderTable({
    d <- decisions(); req(any(d$decision == "Dismiss"))
    s <- scored(); txt <- s$text[s$id %in% d$id[d$decision == "Dismiss"]]
    words <- unlist(lapply(txt, function(x) find_terms(x, c(terms()$species, terms()$product))))
    if (length(words) == 0) return(NULL)
    tb <- sort(table(words), decreasing = TRUE)
    data.frame(`Word in dismissed listings` = names(tb), Times = as.integer(tb), check.names = FALSE)
  })

  output$dl_queue <- downloadHandler(
    filename = function() paste0("python_queue_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(queue(), f, row.names = FALSE, fileEncoding = "UTF-8"))
  output$dl_decisions <- downloadHandler(
    filename = function() paste0("python_decisions_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(decisions(), f, row.names = FALSE, fileEncoding = "UTF-8"))
}

shinyApp(ui, server)
