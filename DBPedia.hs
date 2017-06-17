{-# LANGUAGE Trustworthy #-}

{-| 
 Module: DBPedia
 Description: SPARQL queries against DBPedia, a structured extract of Wikipedia.
-}
module DBPedia (runDBPedia, defaultDBPQuery)
where
import Network.HTTP
import System.IO.Unsafe

preamble = urlEncode $ unlines [
  "prefix ont: <http://dbpedia.org/ontology/>",
  "prefix res: <http://dbpedia.org/resource/>",
  "prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
  "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
  ""]
defaultDBPQuery = unlines ["select distinct ?c where {",
  "?a ont:kingdom res:Animal",
  ". ?a rdf:label ?c",
  "FILTER regex(str(?c), \"^.a....$\")",
  "FILTER langMatches(lang(?c), \"en\")",
  "}"]
postamble = urlEncode " "



queryToURL = ( ("http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=" ++ preamble) ++) . (++ (postamble ++ "&format=text%2Ftab-separated-values&CXML_redir_for_subjs=121&CXML_redir_for_hrefs=&timeout=30000&debug=on") ) . urlEncode


runDBPedia query = unsafePerformIO $ do 
        rv <- simpleHTTP $ getRequest $ queryToURL query
        rb <- getResponseBody rv
        return $ tail $ lines $ filter (/= '"') rb

