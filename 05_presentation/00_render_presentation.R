#!/usr/bin/env Rscript

# Render presentation and copy to docs folder
# This script renders the Quarto presentation and copies it to the docs folder

cat("\n📊 Rendering Conference Presentation...\n\n")

# Render the presentation
presentation_file <- "05_presentation/presentation.qmd"
docs_folder <- "docs"

# Render presentation to its default location
system(paste("quarto render", presentation_file))

# Check if presentation was created
if (file.exists("05_presentation/presentation.html")) {
  cat("✅ Presentation rendered successfully\n")
  
  # Copy to docs folder
  file.copy(
    from = "05_presentation/presentation.html",
    to = file.path(docs_folder, "presentation.html"),
    overwrite = TRUE
  )
  
  # Copy supporting files if they exist
  if (dir.exists("05_presentation/presentation_files")) {
    if (!dir.exists(file.path(docs_folder, "presentation_files"))) {
      dir.create(file.path(docs_folder, "presentation_files"), recursive = TRUE)
    }
    file.copy(
      from = "05_presentation/presentation_files",
      to = docs_folder,
      overwrite = TRUE,
      recursive = TRUE
    )
  }
  
  cat("✅ Presentation copied to docs folder\n")
  cat("🌐 Accessible at: docs/presentation.html\n\n")
} else {
  cat("❌ Presentation rendering failed\n")
}
