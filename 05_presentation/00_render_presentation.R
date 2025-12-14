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
  
  # Copy HTML file to docs folder
  file.copy(
    from = "05_presentation/presentation.html",
    to = file.path(docs_folder, "presentation.html"),
    overwrite = TRUE
  )
  
  # Copy supporting files folder if it exists
  if (dir.exists("05_presentation/presentation_files")) {
    # Remove old presentation_files in docs if it exists
    if (dir.exists(file.path(docs_folder, "presentation_files"))) {
      unlink(file.path(docs_folder, "presentation_files"), recursive = TRUE)
    }
    
    # Copy the entire directory recursively using R.utils or system command
    success <- file.copy(
      from = "05_presentation/presentation_files",
      to = docs_folder,
      overwrite = TRUE,
      recursive = TRUE
    )
    
    if (success) {
      cat("✅ Presentation HTML copied to docs folder\n")
      cat("✅ Supporting files copied to docs/presentation_files\n")
    } else {
      cat("⚠️  Warning: Supporting files may not have copied correctly\n")
    }
  } else {
    cat("✅ Presentation copied to docs folder\n")
  }
  
  cat("🌐 Accessible at: docs/presentation.html\n\n")
} else {
  cat("❌ Presentation rendering failed\n")
}
