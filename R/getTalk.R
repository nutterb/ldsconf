#' @name getTalk
#' @export getTalk
#' 
#' @title Download a Talk
#' @description Download a talk from lds.org and process it into plain text.
#' 
#' @param url URL of the talk to be downloaded
#' 
#' @details The complete HTML code from the page is downloaded.  A series of 
#' regular expressions is used to parse the HTML into a plain text version of 
#' the talk.  Using regular expressions is not generally recommended for 
#' parsing HTML. Unfortunately, the HTML code doesn't appear to play well with
#' more advanced forms of parsing.  The regular expressions seem to work well 
#' here mostly because talk paragraphs start with the tag 
#' \code{<p uri= ... id="pid[num]">}.  HTML rendered characters are converted
#' to plain text and tags such as links, italic, and bold are removed.
#' 
#' @return A character string giving the full text of the talk.  Note that 
#' references are not included.
#' 
#' @author Benjamin Nutter
#' 
#' @examples
#' # Download the first talk of the 1971 General Conference
#' getTalk('https://www.lds.org/general-conference/1971/04/out-of-the-darkness?lang=eng')
#' 

getTalk <- function(url){
  #* Get all HTML Code
  x <- suppressWarnings(readLines(url))
  x <- x[min(grep("<p uri=", x)):(max(grep("<p uri=", x) + 1))]
  
  x <- unlist(strsplit(x, "</p>"))
  #* Remove span tag
  x <- sub("(?<=<span style=)(.*)(?=topic\">)", "", x, perl=TRUE)
  x <- sub("<span style=topic\">", "", x)
  #* Remove paragraph tags
  x <- sub("(?<=<p uri=)(.*)(?=id=\"p\\d{1,6}\">)", "", x, perl=TRUE)
  x <- sub("<p uri=id=\"p\\d{1,6}\">", "", x)
  #* Remove superscript tags
  x <- sub("(?<=<sup )(.*)(?=</sup>)", "", x, perl=TRUE)
  x <- sub("<sup </sup>", "", x)
  #* Remove link tags
  x <- gsub("(?<=<a href)(.*)(?=Ref\">)", "", x, perl=TRUE)
  x <- gsub("<a hrefRef\">", "", x)
  x <- gsub("</a>", "", x)
  #* Remove div tags
  x <- gsub("</div>", "", x)
  #* Plain text single quotes
  x <- gsub("’", "'", x)
  #* Plain text dashes
  x <- gsub("–", "-", x)
  #* Plain text emdash
  x <- gsub("—", "--", x)
  #* Plain text double quotes
  x <- gsub("(“… |“|�)", "\"", x)
  #* Plain text &
  x <- gsub("&amp;", "&", x)
  #* Trim leading white space
  x <- gsub("^[[:space:]]+", "", x)
  #* Italic, Bold
  x <- gsub("(<i>|</i>|<b>|</b>)", "", x)
  #* Ellipsis
  x <- gsub("�", "...", x)
  #* Story blocks
  x <- gsub("(<span class=\"story\">|</span>)", "", x)
  
  paste(x, collapse="\n\n")
}