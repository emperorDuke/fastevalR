format.label <- function(label, console_view, type = "bold") {
  if (console_view) {
    return(label)
  }

  return(switch(
    type,
    bold = sprintf("**%s**", label),
    subscript = sprintf("^%s^", label)
  ))
}
