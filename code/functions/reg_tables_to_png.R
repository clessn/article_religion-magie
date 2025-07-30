latex_table_to_png <- function(input, output_path = "output.png", is_file = FALSE, dpi = 300) {
  # Ensure the function exits gracefully on error
  on.exit({
    temp_files <- c(
      "table.tex", "table.pdf", "table.aux", "table.log",
      "table.out", "table.xdv", "table.synctex.gz",
      "table-1.png", "table.png"
    )
    unlink(temp_files[file.exists(temp_files)])
  })

  # Check if running on Linux
  if (.Platform$OS.type != "unix") {
    stop("This function requires a Linux operating system")
  }

  # Check for required tools
  if (system("which xelatex", ignore.stdout = TRUE) != 0) {
    stop("xelatex not found. Please install TeX Live")
  }

  if (system("which pdftoppm", ignore.stdout = TRUE) != 0) {
    stop("pdftoppm not found. Please install poppler-utils")
  }

  # Validate DPI
  if (!is.numeric(dpi) || dpi < 1 || dpi > 2400) {
    stop("DPI must be a number between 1 and 2400")
  }

  # Handle input file or string
  if (is_file) {
    if (!file.exists(input)) {
      stop("Input file does not exist: ", input)
    }
    if (!grepl("\\.tex$", input)) {
      stop("Input file must have .tex extension")
    }
    tryCatch({
      table_code <- readLines(input, warn = FALSE)
      table_code <- paste(table_code, collapse = "\n")
    }, error = function(e) {
      stop("Failed to read input file: ", e$message)
    })
  } else {
    if (!is.character(input)) {
      stop("Input must be a character string when is_file is FALSE")
    }
    table_code <- input
  }

  # Create base LaTeX document with XeLaTeX specific settings
  base_latex <- "\\documentclass{standalone}
\\usepackage{graphicx}
\\usepackage{tabularray}
\\usepackage{float}
\\usepackage{codehigh}
\\usepackage[normalem]{ulem}
\\usepackage{fontspec}
\\usepackage{unicode-math}
\\UseTblrLibrary{booktabs}
\\UseTblrLibrary{siunitx}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
\\begin{document}
%s
\\end{document}"

  # Insert table code into base document
  full_latex <- sprintf(base_latex, table_code)

  # Set file paths
  tex_file <- "table.tex"
  pdf_file <- "table.pdf"

  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir) && output_dir != ".") {
    if (!dir.create(output_dir, recursive = TRUE)) {
      stop("Failed to create output directory: ", output_dir)
    }
  }

  # Write LaTeX file
  tryCatch({
    writeLines(full_latex, tex_file)
  }, error = function(e) {
    stop("Failed to write LaTeX file: ", e$message)
  })

  # Compile with XeLaTeX (two passes)
  for (i in 1:2) {
    system2("xelatex", 
            args = c("-interaction=nonstopmode", "-no-pdf", tex_file),
            stdout = NULL, stderr = NULL)
    
    # Check if XDV was created
    if (!file.exists("table.xdv")) {
      stop("XeLaTeX compilation failed - no XDV file created")
    }
  }

  # Convert XDV to PDF
  system2("xdvipdfmx", args = "table.xdv", stdout = NULL, stderr = NULL)
  
  # Verify PDF was created
  if (!file.exists(pdf_file)) {
    stop("PDF file was not created")
  }

  # Try multiple approaches for PDF to PNG conversion
  png_created <- FALSE
  
  # Approach 1: pdftoppm with -singlefile
  tryCatch({
    system2("pdftoppm", 
            args = c("-png", "-singlefile", sprintf("-r %d", dpi), pdf_file, "table"),
            stdout = NULL, stderr = NULL)
    
    # Check for various possible output names
    possible_files <- c("table.png", "table-1.png", "table-01.png")
    existing_files <- possible_files[file.exists(possible_files)]
    
    if (length(existing_files) > 0) {
      success <- file.rename(existing_files[1], output_path)
      if (!success) {
        success <- file.copy(existing_files[1], output_path, overwrite = TRUE)
        if (success) file.remove(existing_files[1])
      }
      png_created <- success
    }
  }, error = function(e) {
    # Continue to next approach
  })
  
  # Approach 2: pdftoppm without -singlefile (if first approach failed)
  if (!png_created) {
    tryCatch({
      system2("pdftoppm", 
              args = c("-png", sprintf("-r %d", dpi), pdf_file, "table"),
              stdout = NULL, stderr = NULL)
      
      # Look for any PNG files created
      png_files <- list.files(pattern = "^table.*\\.png$")
      if (length(png_files) > 0) {
        success <- file.rename(png_files[1], output_path)
        if (!success) {
          success <- file.copy(png_files[1], output_path, overwrite = TRUE)
          if (success) file.remove(png_files[1])
        }
        png_created <- success
      }
    }, error = function(e) {
      # Continue to next approach
    })
  }
  
  # Approach 3: Use convert from ImageMagick (if available)
  if (!png_created && system("which convert", ignore.stdout = TRUE) == 0) {
    tryCatch({
      system2("convert", 
              args = c("-density", as.character(dpi), pdf_file, output_path),
              stdout = NULL, stderr = NULL)
      png_created <- file.exists(output_path)
    }, error = function(e) {
      # Continue
    })
  }
  
  # Approach 4: Use gs (ghostscript) if available
  if (!png_created && system("which gs", ignore.stdout = TRUE) == 0) {
    tryCatch({
      system2("gs", 
              args = c("-dNOPAUSE", "-dBATCH", "-sDEVICE=png16m", 
                      sprintf("-r%d", dpi), sprintf("-sOutputFile=%s", output_path), 
                      pdf_file),
              stdout = NULL, stderr = NULL)
      png_created <- file.exists(output_path)
    }, error = function(e) {
      # Continue
    })
  }

  # Final check
  if (!png_created || !file.exists(output_path) || file.size(output_path) == 0) {
    stop("Failed to create PNG file. Try installing: sudo apt-get install poppler-utils imagemagick ghostscript")
  }

  return(normalizePath(output_path))
}