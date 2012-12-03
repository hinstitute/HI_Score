#see below for query specifications
getInsights <- function(
        username = "Insights4R@gmail.com",
        password = "googleinsights4r",
	text_query = "'hello world'",
	date_query = "1/2004 108m",
	geo_query = "US",
	search_type= "all", 
	filter_category="none"){

#user name & password:
#     input from your personal account
#
#text queries:
#     'single quotes around phrases'
#     "'double quotes'+'around queries'"
#     "(parntheses + to) + (group + statments)"
#     + = OR
#     , = comparison

#date queries:
#    "dat[month]/[year] [num months]m"
#	  ie: "1/2004 108m" - DEFAULT
#	  comparison: "1/2005 12m, 1/2006 12m"
#    other date queries:
#	  "last week"
# 	  "last month"
#	  "last 3 months"
#	  "last year"

#geo queries:
#   "[country code(2)]-[state code(2)]-[metro area(?)]"
#	  ie: "US" - DEFAULT 
#    comparison: "US,GB"
#	  New York State: "US-NY
#	  see google insight for further geoprecision

#search types:
#   "news"
#   "images"
#   "froogle
#   "all" - DEFAULT
#

# filter categories: (input "category" field) 
#
# ** These are just examples, to find more, 
# ** filter by a category on the website and
# ** note the code in the url after "&cat="

#input filter category and corresponding code
category <- c("none",
"Arts and Entertainment",
"Entertainment Industry",
"Film and TV Industry",
"Film and TV Awards",
"Film and TV Production",
"Film Festivals",
"Movie Listings and Theater Showtimes",
"Ticket Sales",
"Flash-Based Entertainment",
"Online Video",
"TV Shows and Programs",
"News",
"Broadcast and Network News",
"Celebrities and Entertainment News",
"Newspapers",
"Politics",
"People and Society")

code <- c("",
"0-3",
"0-3-612",
"0-3-612-1116",
"0-3-612-1116-1108",
"0-3-612-1116-1117",
"0-3-569-1086",
"0-3-569-1085",
"0-3-569-614",
"0-3-539-447",
"0-3-36-211",
"0-3-36-358",
"0-16",
"0-16-112",
"0-16-184",
"0-16-408",
"0-16-396",
"0-14")

# create category reference table
ref <- data.frame(category, code)
ref <- data.frame(lapply(ref, function(x){tolower(x)}))

# for your convenience:
                               # category              code
# 1                                  none                  
# 2                arts and entertainment               0-3
# 3                entertainment industry           0-3-612
# 4                  film and tv industry      0-3-612-1116
# 5                    film and tv awards 0-3-612-1116-1108
# 6                film and tv production 0-3-612-1116-1117
# 7                        film festivals      0-3-569-1086
# 8  movie listings and theater showtimes      0-3-569-1085
# 9                          ticket sales       0-3-569-614
# 10            flash-based entertainment       0-3-539-447
# 11                         online video        0-3-36-211
# 12                tv shows and programs        0-3-36-358
# 13                                 news              0-16
# 14           broadcast and network news          0-16-112
# 15   celebrities and entertainment news          0-16-184
# 16                           newspapers          0-16-408
# 17                             politics          0-16-396
# 18                   people and society              0-14

#add necessary packages
require(stringr)
require(RCurl)

#simplify query names
q <- text_query
date <- date_query
geo <- geo_query
gprop <- tolower(search_type)

# generate:
# "" other date queries
	if(date_query == "last week") date <- "today 7-d"
	if(date_query == "last month") date <- "today 1-m"	
	if(date_query == "last 3 months") date <- "today 3-m"	
	if(date_query == "last year") date <- "today 12-m"

# "" filter categories
	ref <- ref[which(ref[,1]==tolower(filter_category)),]
	cat <- as.character(ref[1,2])

# ""comparison component
	tn <- length(unlist(strsplit(text_query, ",")))
	dn <- length(unlist(strsplit(date_query, ",")))
	gn <- length(unlist(strsplit(geo_query, ",")))

	if( tn > 1 & dn == 1 & gn == 1 ) cmpt <- "q"
	if( dn > 1 & tn == 1 & gn == 1 ) cmpt <- "date"
	if( gn > 1 & tn == 1 & dn == 1 ) cmpt <- "geo"
	# generate error for multiple comparisons
	if ((tn > 1 & dn >1) | 
		(tn > 1 & gn >1) | 
		(dn > 1 & gn >1)) print("Error: You cannot have multiple comparison types")
#generate NULLS:
	# "" geo queries
		if(geo_query=="all") geo <- ""
	# "" search types
		if(search_type=="all") gprop <- ""	
	# "" compare bys	
		if( gn == 1 & tn == 1 & dn == 1 ) cmpt <- ""
	# "" filter categories
		if(filter_category=="none") cat <- ""	

# Set up Web Crawler
ch <- getCurlHandle()
curlSetOpt(curl = ch,
            ssl.verifypeer = FALSE,
            useragent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13",
            timeout = 60,
            followlocation = TRUE,
            cookiejar = "./cookies",
            cookiefile = "./cookies")

## Login to Google
loginURL <- "https://accounts.google.com/accounts/ServiceLogin"
loginPage <- getURL(loginURL, curl = ch)

to_match <- str_extract(string = loginPage, pattern = ignore.case('name="GALX"\\s*value="([^"]+)"'))
                          
galx <- str_replace(string = to_match,pattern = ignore.case('name="GALX"\\s*value="([^"]+)"'),replacement = "\\1")

authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"         
authenticatePage <- postForm(authenticateURL, .params =list(Email = username, Passwd = password, GALX = galx) , curl = ch)

## Get Google Insights results CSV
insightsURL <- "http://www.google.com/insights/search/overviewReport"

#input variables created above
resultsText <- getForm(insightsURL, .params = list(q = q, geo=geo, date=date, cat=cat, cmpt = cmpt, gprop=gprop, content = 1, export = 1), curl = ch)

if(isTRUE(unname(attr(resultsText, "Content-Type")[1] == "text/csv"))) {
  ## got CSV file

  ## create temporary connection from results
  tt <- textConnection(resultsText)

  output <- read.csv(tt, header = FALSE)

  ## close connection
  close(tt)
} else {
  print("something went wrong")
}
  return(output)
}
getInsights()