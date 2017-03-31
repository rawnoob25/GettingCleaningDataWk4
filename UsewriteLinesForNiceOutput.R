s<-"He was referring to the word, \"banana\".It's a fruit."
writeLines(s)
writeLines("Periods should not be escaped unless they're part of a regex.\n")

x <- "In ALGOL, you could do logical AND with /\\."
print(x)      # shows it as above ("input-like")
writeLines(x) # shows it in a nicer format