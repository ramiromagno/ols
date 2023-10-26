names_in_braces <- function(x) {

  components <- unlist(strsplit(x, split = "\\/"))
  names_in_braces <- grep(pattern = "\\{\\w+\\}", x = components, value = TRUE)
  names <- gsub(pattern = "[{}]", replacement = "", x = names_in_braces)
  names
}
