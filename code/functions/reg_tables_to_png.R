latex_table_to_png <- function(input, output_path = "output.png", is_file = FALSE, dpi = 300) {
  # Ensure the function exits gracefully on error
  on.exit({
    # List of all possible temporary files that might be created
    temp_files <- c(
      "table.tex", "table.pdf", "table.aux", "table.log",
      "table.out", "table.xdv", "table.synctex.gz",
      "table-1.png"  # temporary PNG file created by pdftoppm
    )
    # Remove any temporary files that exist
    unlink(temp_files[file.exists(temp_files)])
  })

  # Check if running on Linux
  if (.Platform$OS.type != "unix") {
    stop("This function requires a Linux operating system")
  }

  # Check for XeLaTeX
  if (system("which xelatex", ignore.stdout = TRUE) != 0) {
    stop(
      "xelatex not found. Please install TeX Live using:\n",
      "sudo apt-get install texlive-xetex texlive-latex-extra texlive-science"
    )
  }

  # Check for pdftoppm (part of poppler-utils)
  if (system("which pdftoppm", ignore.stdout = TRUE) != 0) {
    stop(
      "pdftoppm not found. Please install poppler-utils using:\n",
      "sudo apt-get install poppler-utils"
    )
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
  temp_png <- "table" # pdftoppm will add -1.png to this

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

  # Compile with XeLaTeX (two passes to ensure references are correct)
  for (i in 1:2) {
    compile_result <- system2(
      "xelatex",
      args = c(
        "-interaction=nonstopmode",
        "-no-pdf",
        tex_file
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    
    if (length(compile_result) > 0) {
      # Save compilation log for debugging
      writeLines(compile_result, "xelatex_compile.log")
    }
    
    # Check if XDV was created
    if (!file.exists(gsub("\\.tex$", ".xdv", tex_file))) {
      stop("XeLaTeX compilation failed. Check xelatex_compile.log for details.")
    }
  }

  # Convert XDV to PDF
  system2(
    "xdvipdfmx",
    args = gsub("\\.tex$", ".xdv", tex_file)
  )

  # Convert PDF to PNG using pdftoppm
  convert_result <- system2(
    "pdftoppm",
    args = c(
      "-png",            # Output format
      "-singlefile",     # Output single file
      sprintf("-r %d", dpi),  # Resolution
      pdf_file,          # Input file
      temp_png          # Output file prefix
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  # Check if conversion was successful
  temp_png_file <- "table-1.png"
  if (length(convert_result) > 0 || !file.exists(temp_png_file)) {
    stop(
      "PDF to PNG conversion failed. Error message:\n",
      paste(convert_result, collapse = "\n")
    )
  }

  # Move the temporary PNG to the desired location
  file.rename(temp_png_file, output_path)

  # Verify the PNG file exists and has content
  if (!file.exists(output_path) || file.size(output_path) == 0) {
    stop("PNG file was not created or is empty")
  }

  return(normalizePath(output_path))
}

# Example usage with direct LaTeX code:
# table_code <- "\\begin{tblr}{colspec={ccc}}
#   A & B & C \\\\
#   1 & 2 & 3 \\\\
#   4 & 5 & 6
# \\end{tblr}"
# png_path <- latex_table_to_png(table_code, "my_table.png", is_file = FALSE, dpi = 300)
