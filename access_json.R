library(jsonlite)


#drop n=1000 if you want to read everything
con_pageviews <- textConnection(readLines(
  gzfile("page_views.json.gz"),
  n = 1000
))
pageviews_sample <- stream_in(con_pageviews, verbose = FALSE)
close(con_pageviews)

con_arwiki <- textConnection(readLines(gzfile("arwiki.json.gz"), n = 1000))
arwiki_sample <- stream_in(con_arwiki, verbose = FALSE)
close(con_arwiki)

con_pageinfo <- textConnection(readLines(gzfile("page_info.json.gz"), n = 1000))
pageinfo_sample <- stream_in(con_pageinfo, verbose = FALSE)
close(con_pageinfo)


head(pageviews_sample)
head(arwiki_sample)
head(pageinfo_sample)
