#-------------------------------------------------------------------
# Project: Education Production Function
# Script: Manuscript Rendering Pipeline
# Author: Garen Avanesian
# Date: October 23, 2025
#-------------------------------------------------------------------

cat("\n📚 MANUSCRIPT RENDERING PIPELINE\n")
cat("===============================\n\n")

# Set working directory to project root
setwd("..")

# Source the configuration to load Quarto utilities
source("project_config.R")

# Define paths
manuscript_file <- "04_manuscript/manuscript.qmd"
docs_folder <- "docs"

# Create docs folder if it doesn't exist (for GitHub Pages)
if (!dir.exists(docs_folder)) {
  dir.create(docs_folder, recursive = TRUE)
  cat("📁 Created docs folder for GitHub Pages\n")
}

cat("🎯 Starting manuscript rendering...\n\n")

# =============================================================================
# 1. RENDER PDF TO MANUSCRIPT FOLDER
# =============================================================================
cat("📄 RENDERING 1: PDF to manuscript folder\n")
cat("========================================\n")

tryCatch({
  system(paste("quarto render", shQuote(manuscript_file), "--to pdf"), intern = FALSE)
  
  pdf_path <- "04_manuscript/manuscript.pdf"
  if (file.exists(pdf_path)) {
    cat("✅ PDF rendered successfully:", pdf_path, "\n")
  } else {
    cat("❌ PDF rendering failed\n")
  }
}, error = function(e) {
  cat("❌ Error rendering PDF:", e$message, "\n")
})

cat("\n")

# =============================================================================
# 2. RENDER DOCX TO MANUSCRIPT FOLDER  
# =============================================================================
cat("📝 RENDERING 2: DOCX to manuscript folder\n")
cat("=========================================\n")

tryCatch({
  system(paste("quarto render", shQuote(manuscript_file), "--to docx"), intern = FALSE)
  
  docx_path <- "04_manuscript/manuscript.docx"
  if (file.exists(docx_path)) {
    cat("✅ DOCX rendered successfully:", docx_path, "\n")
  } else {
    cat("❌ DOCX rendering failed\n")
  }
}, error = function(e) {
  cat("❌ Error rendering DOCX:", e$message, "\n")
})

cat("\n")

# =============================================================================
# 3. RENDER HTML TO DOCS FOLDER (FOR GITHUB PAGES)
# =============================================================================
cat("🌐 RENDERING 3: HTML to docs folder (GitHub Pages)\n")
cat("==================================================\n")

tryCatch({
  system(paste("quarto render", shQuote(manuscript_file), "--to html", 
               "--output-dir", shQuote(docs_folder)), intern = FALSE)
  
  html_path <- file.path(docs_folder, "manuscript.html")
  
  # Also check if it was created in manuscript folder and move it
  manuscript_html <- "04_manuscript/manuscript.html"
  if (file.exists(manuscript_html) && !file.exists(html_path)) {
    file.copy(manuscript_html, html_path)
    file.remove(manuscript_html)
    cat("📁 Moved HTML file to docs folder\n")
  }
  
  if (file.exists(html_path)) {
    cat("✅ HTML rendered successfully:", html_path, "\n")
    cat("🌍 Ready for GitHub Pages deployment!\n")
    cat("📋 To enable GitHub Pages:\n")
    cat("   1. Go to repository Settings > Pages\n")
    cat("   2. Select 'Deploy from a branch'\n") 
    cat("   3. Choose 'main' branch and '/docs' folder\n")
  } else {
    cat("❌ HTML rendering failed\n")
  }
}, error = function(e) {
  cat("❌ Error rendering HTML:", e$message, "\n")
})

cat("\n")

# =============================================================================
# RENDERING SUMMARY
# =============================================================================
cat("📊 RENDERING SUMMARY\n")
cat("===================\n")

# Check which files were created
files_created <- 0
file_status <- data.frame(
  Format = c("PDF", "DOCX", "HTML"),
  Path = c("04_manuscript/manuscript.pdf", 
           "04_manuscript/manuscript.docx", 
           "docs/manuscript.html"),
  Status = c("❌", "❌", "❌"),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(file_status)) {
  if (file.exists(file_status$Path[i])) {
    file_status$Status[i] <- "✅"
    files_created <- files_created + 1
  }
}

for (i in 1:nrow(file_status)) {
  cat(sprintf("%-5s %s %s\n", file_status$Format[i], file_status$Status[i], file_status$Path[i]))
}

cat("\n")
cat("🎉 RENDERING COMPLETED!\n")
cat(sprintf("📈 Success rate: %d/3 formats rendered successfully\n", files_created))

if (files_created == 3) {
  cat("🚀 All formats ready for distribution!\n")
  cat("📚 PDF & DOCX: Ready for academic submission\n")
  cat("🌐 HTML: Ready for GitHub Pages at https://[username].github.io/[repo]/manuscript.html\n")
} else {
  cat("⚠️  Some formats failed. Check error messages above.\n")
}

cat("\n" , rep("=", 50), "\n")