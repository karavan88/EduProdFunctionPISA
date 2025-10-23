#-------------------------------------------------------------------
# Project: Education Production Function
# Script: Quarto Rendering Utilities
# Author: Garen Avanesian
# Date: October 23, 2025
#-------------------------------------------------------------------

cat("📝 QUARTO RENDERING UTILITIES LOADED\n")
cat("====================================\n\n")

# =============================================================================
# QUARTO RENDERING FUNCTIONS
# =============================================================================

#' Render Quarto Document to DOCX
#' 
#' @param qmd_file Path to the .qmd file (relative or absolute)
#' @param output_dir Directory to save the output (optional)
#' @param open_file Whether to open the file after rendering (default: TRUE)
#' @return Path to the rendered DOCX file
render_to_docx <- function(qmd_file, output_dir = NULL, open_file = TRUE) {
  
  cat("📄 RENDERING TO DOCX\n")
  cat("===================\n")
  cat("Source file:", qmd_file, "\n")
  
  # Check if file exists
  if (!file.exists(qmd_file)) {
    stop("❌ Quarto file not found: ", qmd_file)
  }
  
  # Prepare command
  cmd <- paste("quarto render", shQuote(qmd_file), "--to docx")
  
  if (!is.null(output_dir)) {
    cmd <- paste(cmd, "--output-dir", shQuote(output_dir))
    cat("Output directory:", output_dir, "\n")
  }
  
  cat("Command:", cmd, "\n")
  cat("⏳ Rendering in progress...\n")
  
  # Execute rendering
  result <- tryCatch({
    system(cmd, intern = TRUE)
  }, error = function(e) {
    cat("❌ Error during rendering:\n")
    cat(e$message, "\n")
    return(NULL)
  })
  
  if (is.null(result)) {
    cat("❌ Rendering failed!\n")
    return(invisible(NULL))
  }
  
  # Determine output file path
  qmd_dir <- dirname(qmd_file)
  qmd_name <- tools::file_path_sans_ext(basename(qmd_file))
  
  if (!is.null(output_dir)) {
    output_path <- file.path(output_dir, paste0(qmd_name, ".docx"))
  } else {
    output_path <- file.path(qmd_dir, paste0(qmd_name, ".docx"))
  }
  
  # Check if output was created
  if (file.exists(output_path)) {
    cat("✅ DOCX rendering completed successfully!\n")
    cat("📁 Output saved to:", output_path, "\n")
    
    if (open_file && interactive()) {
      cat("🔍 Opening file...\n")
      shell.exec(output_path)  # Windows
      # For macOS: system(paste("open", shQuote(output_path)))
      # For Linux: system(paste("xdg-open", shQuote(output_path)))
    }
    
    return(output_path)
  } else {
    cat("❌ Output file not found. Check for errors above.\n")
    return(invisible(NULL))
  }
}

#' Render Quarto Document to HTML
#' 
#' @param qmd_file Path to the .qmd file (relative or absolute)
#' @param output_dir Directory to save the output (optional)
#' @param open_file Whether to open the file after rendering (default: TRUE)
#' @param self_contained Whether to create a self-contained HTML (default: TRUE)
#' @return Path to the rendered HTML file
render_to_html <- function(qmd_file, output_dir = NULL, open_file = TRUE, self_contained = TRUE) {
  
  cat("🌐 RENDERING TO HTML\n")
  cat("===================\n")
  cat("Source file:", qmd_file, "\n")
  
  # Check if file exists
  if (!file.exists(qmd_file)) {
    stop("❌ Quarto file not found: ", qmd_file)
  }
  
  # Prepare command
  cmd <- paste("quarto render", shQuote(qmd_file), "--to html")
  
  if (self_contained) {
    cmd <- paste(cmd, "--embed-resources --standalone")
    cat("Mode: Self-contained HTML\n")
  } else {
    cat("Mode: Standard HTML\n")
  }
  
  if (!is.null(output_dir)) {
    cmd <- paste(cmd, "--output-dir", shQuote(output_dir))
    cat("Output directory:", output_dir, "\n")
  }
  
  cat("Command:", cmd, "\n")
  cat("⏳ Rendering in progress...\n")
  
  # Execute rendering
  result <- tryCatch({
    system(cmd, intern = TRUE)
  }, error = function(e) {
    cat("❌ Error during rendering:\n")
    cat(e$message, "\n")
    return(NULL)
  })
  
  if (is.null(result)) {
    cat("❌ Rendering failed!\n")
    return(invisible(NULL))
  }
  
  # Determine output file path
  qmd_dir <- dirname(qmd_file)
  qmd_name <- tools::file_path_sans_ext(basename(qmd_file))
  
  if (!is.null(output_dir)) {
    output_path <- file.path(output_dir, paste0(qmd_name, ".html"))
  } else {
    output_path <- file.path(qmd_dir, paste0(qmd_name, ".html"))
  }
  
  # Check if output was created
  if (file.exists(output_path)) {
    cat("✅ HTML rendering completed successfully!\n")
    cat("📁 Output saved to:", output_path, "\n")
    
    if (open_file && interactive()) {
      cat("🔍 Opening file in browser...\n")
      browseURL(output_path)
    }
    
    return(output_path)
  } else {
    cat("❌ Output file not found. Check for errors above.\n")
    return(invisible(NULL))
  }
}

#' Render Quarto Document to Multiple Formats
#' 
#' @param qmd_file Path to the .qmd file (relative or absolute)
#' @param formats Vector of formats to render (e.g., c("docx", "html", "pdf"))
#' @param output_dir Directory to save the output (optional)
#' @param open_files Whether to open files after rendering (default: FALSE)
#' @return List of paths to rendered files
render_to_multiple <- function(qmd_file, formats = c("docx", "html"), output_dir = NULL, open_files = FALSE) {
  
  cat("📚 RENDERING TO MULTIPLE FORMATS\n")
  cat("================================\n")
  cat("Source file:", qmd_file, "\n")
  cat("Formats:", paste(formats, collapse = ", "), "\n\n")
  
  rendered_files <- list()
  
  for (format in formats) {
    cat("🔄 Rendering to", toupper(format), "...\n")
    
    if (format == "docx") {
      result <- render_to_docx(qmd_file, output_dir, open_files)
    } else if (format == "html") {
      result <- render_to_html(qmd_file, output_dir, open_files)
    } else {
      # Generic rendering for other formats
      cmd <- paste("quarto render", shQuote(qmd_file), "--to", format)
      if (!is.null(output_dir)) {
        cmd <- paste(cmd, "--output-dir", shQuote(output_dir))
      }
      
      cat("Command:", cmd, "\n")
      system_result <- system(cmd, intern = TRUE)
      
      # Determine output path (simplified)
      qmd_dir <- dirname(qmd_file)
      qmd_name <- tools::file_path_sans_ext(basename(qmd_file))
      
      if (!is.null(output_dir)) {
        result <- file.path(output_dir, paste0(qmd_name, ".", format))
      } else {
        result <- file.path(qmd_dir, paste0(qmd_name, ".", format))
      }
    }
    
    rendered_files[[format]] <- result
    cat("\n")
  }
  
  cat("🎉 ALL FORMATS RENDERED!\n")
  cat("========================\n")
  for (format in names(rendered_files)) {
    if (!is.null(rendered_files[[format]])) {
      cat("✅", toupper(format), ":", rendered_files[[format]], "\n")
    } else {
      cat("❌", toupper(format), ": Failed\n")
    }
  }
  
  return(rendered_files)
}

#' Quick render function for your manuscript
#' 
#' @param format Format to render ("docx", "html", or "both")
#' @param open_file Whether to open the file after rendering
render_manuscript <- function(format = "both", open_file = TRUE) {
  
  manuscript_path <- "04_manuscript/manuscript.qmd"
  
  if (!file.exists(manuscript_path)) {
    cat("❌ Manuscript not found at:", manuscript_path, "\n")
    cat("💡 Make sure you're in the project root directory\n")
    return(invisible(NULL))
  }
  
  if (format == "docx") {
    return(render_to_docx(manuscript_path, open_file = open_file))
  } else if (format == "html") {
    return(render_to_html(manuscript_path, open_file = open_file))
  } else if (format == "both") {
    return(render_to_multiple(manuscript_path, c("docx", "html"), open_files = open_file))
  } else {
    cat("❌ Invalid format. Use 'docx', 'html', or 'both'\n")
    return(invisible(NULL))
  }
}

# =============================================================================
# TROUBLESHOOTING FUNCTIONS
# =============================================================================

#' Check Quarto Installation and Setup
check_quarto <- function() {
  cat("🔍 QUARTO SYSTEM CHECK\n")
  cat("=====================\n")
  
  # Check if Quarto is installed
  quarto_version <- tryCatch({
    system("quarto --version", intern = TRUE)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(quarto_version)) {
    cat("❌ Quarto not found in PATH\n")
    cat("💡 Install Quarto from: https://quarto.org/docs/get-started/\n")
    return(FALSE)
  } else {
    cat("✅ Quarto version:", quarto_version, "\n")
  }
  
  # Check if pandoc is available
  pandoc_version <- tryCatch({
    system("pandoc --version", intern = TRUE)[1]
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(pandoc_version)) {
    cat("❌ Pandoc not found\n")
    cat("💡 Pandoc is usually installed with Quarto\n")
  } else {
    cat("✅ Pandoc available:", pandoc_version, "\n")
  }
  
  # Check current working directory
  cat("📁 Current directory:", getwd(), "\n")
  
  # Check if manuscript exists
  if (file.exists("04_manuscript/manuscript.qmd")) {
    cat("✅ Manuscript found\n")
  } else {
    cat("❌ Manuscript not found at 04_manuscript/manuscript.qmd\n")
  }
  
  cat("\n")
  return(TRUE)
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

cat("📖 USAGE EXAMPLES:\n")
cat("==================\n")
cat("# Render manuscript to DOCX:\n")
cat("render_manuscript('docx')\n\n")
cat("# Render manuscript to HTML:\n")
cat("render_manuscript('html')\n\n")
cat("# Render to both formats:\n")
cat("render_manuscript('both')\n\n")
cat("# Check Quarto setup:\n")
cat("check_quarto()\n\n")
cat("# Custom rendering:\n")
cat("render_to_docx('04_manuscript/manuscript.qmd')\n")
cat("render_to_html('04_manuscript/manuscript.qmd')\n\n")

cat("✅ QUARTO UTILITIES READY!\n\n")