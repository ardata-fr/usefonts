font_id = c(
  "Arial" = "arial",
  "Open Sans" = "open-sans",
  "Roboto" = "roboto",
  "Fira Code" = "fira-code",
  "Fira Sans Condensed" = "fira-sans-condensed")

font_files = list(
  "arial" = c(
    plain = "arial.ttf",
    bold = "arial-bold.ttf",
    italic = "arial-italic.ttf",
    bolditalic = "arial-bold-italic.ttf"),
  "open-sans" = c(
    plain = "OpenSans-Regular.ttf",
    bold = "OpenSans-Bold.ttf",
    italic = "OpenSans-Italic.ttf",
    bolditalic = "OpenSans-BoldItalic.ttf"),
  "roboto" = c(
    plain = "roboto-v27-latin-regular.ttf",
    bold = "roboto-v27-latin-700.ttf",
    italic = "roboto-v27-latin-italic.ttf",
    bolditalic = "roboto-v27-latin-700italic.ttf"),
  "fira-code" = c(plain = "fira-code-v14-latin-regular.ttf",
                  bold = "fira-code-v14-latin-700.ttf"),
  "fira-sans-condensed" = c(
    plain = "fira-sans-condensed-v5-latin-regular.ttf",
    bold = "fira-sans-condensed-v5-latin-700.ttf",
    italic = "fira-sans-condensed-v5-latin-italic.ttf",
    bolditalic = "fira-sans-condensed-v5-latin-700italic.ttf")
)

#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @export
#' @title Font HTML dependency
#' @description Create an HTML dependency ready
#' to be used in 'Shiny' or 'R Markdown'. It allows
#' to use specific fonts in HTML page.
#'
#' @param font one of "Arial", "Open Sans", "Roboto", "Fira Code" or "Fira Sans Condensed".
#' @return an object defined with [htmlDependency()].
fontHtmlDependency <- function(font = c("Arial", "Open Sans", "Roboto", "Fira Code", "Fira Sans Condensed")) {
  font <- match.arg(font)
  id <- font_id[font]
  pkg_version <- packageVersion("usefonts")
  pkg_version_str <- format(pkg_version)

  htmlDependency(
    all_files = TRUE,
    name = id,
    version = pkg_version_str,
    src = system.file(package = "usefonts", "assets", id),
    stylesheet = paste0("css/", id, ".css"))
}

#' @export
#' @importFrom htmltools tags attachDependencies
#' @title Use a font in Shiny or Markdown
#' @description Add an empty `<style>` HTML element attached
#' to an [HTML Dependency](htmlDependency) containing
#' the css and the font files so that the font is available
#' in the HTML page.
#'
#' The htmlDependency is defined with function [fontHtmlDependency()].
#' @param font one of "Arial", "Open Sans", "Roboto", "Fira Code" or "Fira Sans Condensed".
#' @return an HTML object
#' @examples
#' addFontHtmlDependency(font = "Open Sans")
#' addFontHtmlDependency(font = "Roboto")
addFontHtmlDependency <- function(font = c("Arial", "Open Sans", "Roboto", "Fira Code", "Fira Sans Condensed")) {
  attachDependencies(
    x = tags$style(""),
    fontHtmlDependency(font = font)
  )
}

#' @importFrom systemfonts register_font
#' @export
#' @title Register a font for graphics
#' @description Register with package 'systemfonts' a font
#' from the package so that the font is available
#' when the graphics are built.
#' @param font one of "Arial", "Open Sans", "Roboto", "Fira Code" or "Fira Sans Condensed".
#' @return TRUE if the operation went ok.
#' @examples
#' add_font(font = "Open Sans")
#' add_font(font = "Roboto")
add_font <- function(font = c("Arial", "Open Sans", "Roboto", "Fira Code", "Fira Sans Condensed")) {
  font <- match.arg(font)
  id <- font_id[font]
  if(!font_family_exists(font)){
    args <- fontfiles(id)
    args[["name"]] <- font
    do.call(register_font, args)
  }
  font_family_exists(font)
}

#' @export
#' @title Shell command to install a font
#' @description Create a string containing the shell command to execute
#' so that the font is installed on the system. Its execution should require root
#' permissions.
#' @param font one of "Arial", "Open Sans", "Roboto", "Fira Code" or "Fira Sans Condensed".
#' @param dir the name of the sub-directory to use when copying the font files.
#' @param platform not yet supported, only ubuntu is supported for now.
#' @return the shell command as a string
#' @examples
#' install_font_str(font = "Open Sans")
install_font_str <- function(font = c("Arial", "Open Sans", "Roboto", "Fira Code", "Fira Sans Condensed"),
                             dir = "custom-fonts", platform = "ubuntu") {
  font <- match.arg(font)
  id <- font_id[font]
  create_dir <- sprintf("mkdir -p /usr/share/fonts/truetype/%s", dir)
  id_dir <- system.file(package = "usefonts", "assets", id, "fonts")
  install_cmd <- sprintf("find %s -name \"*.ttf\" -exec install -m644 {} /usr/share/fonts/truetype/%s/ \\; || return 1",
                         id_dir, dir)

  paste(create_dir, install_cmd, "fc-cache -f", sep = "\n")
}

# utils ----
#' @importFrom systemfonts system_fonts registry_fonts
fortify_font_db <- function(){
  db_sys <- system_fonts()
  db_reg <- registry_fonts()
  nam <- intersect(colnames(db_sys), colnames(db_reg))
  db_sys <- db_sys[,nam]
  db_reg <- db_reg[,nam]
  font_db <- rbind(db_sys, db_reg)
  font_db
}

font_family_exists <- function( font_family = "sans" ){
  datafonts <- fortify_font_db()
  tolower(font_family) %in% tolower(datafonts$family)
}

fontfiles <- function(id) {
  dir <- system.file(package = "usefonts", "assets", id, "fonts")
  args <- as.list(font_files[[id]])
  lapply(args, function(name, dir) {
    file.path(dir, name)
  }, dir = dir)

}
