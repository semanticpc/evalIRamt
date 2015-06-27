#' Parse the TREC XML file with topics and subtopics
#'
#' @param filename path to the TREC XML topics file
#' @return a list with the topics and subtopics
#' @examples
#' parseWebTopicDesc("trecFullTopics.xml")
parseWebTopicDesc <- function(filename){
  require(XML, quietly = T)
  doc <- xmlParse(filename)
  topics <- getNodeSet(doc, "//topic")
  process_TREC_topic <- function(t){
    desc <- xmlValue(xmlChildren(t)$description)
    subtopics <- sapply(getNodeSet(t, path="subtopic"), xmlValue)

    # Text clean up text
    subtopics <- gsub( "^\\s+|\\s+$|\n|\"|\'|\\,", "",subtopics)
    subtopics <- gsub("\\s{2,}"," ",subtopics)

    desc <- gsub( "^\\s+|\\s+$|\n|\"|\'|\\,", "",desc)
    desc <- gsub("\\s{2,}"," ",desc)

    return(list(number=as.numeric(xmlGetAttr(t, "number")),
                type=xmlGetAttr(t, "type"),
                text=xmlValue(xmlChildren(t)$query),
                desc=desc,
                subtopics=subtopics))
  }
  topics <- lapply(topics, process_TREC_topic)
  names(topics) <- sapply(topics, function(x) x$number)

  return(topics)
}
