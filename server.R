
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyapps)
library(ggplot2)
library(scales)

get_dataset <- function() { 
  datasetFile <- "./data/P00000001-ALL.csv"
  if(!file.exists("./data/P00000001-ALL.csv")) {
    datasetUrl <- "ftp://ftp.fec.gov/FEC/Presidential_Map/2016/P00000001/P00000001-ALL.zip"
    destFile <- "./data/P00000001-ALL.zip"
    # Download the file
    download.file(datasetUrl, destfile=destFile, method="curl")
    unzip(destFile, exdir = "./data/")
  }
  return(datasetFile)
}
load_dataset <- function(datasetFile) { 
  fullDataSet <- read.csv(datasetFile, na.strings=c("NA","","INFORMATION REQUESTED PER BEST EFFORTS"), skip=1, head=FALSE,sep = ",", quote = "\"")
  dataSetVariables <- paste("V", c(3,5,6,7,8,9,10,11), sep="")
  dataset <- fullDataSet[dataSetVariables]
  # Remove the full dataset, it's big and we don't need it
  #rm(fullDataSet)
  colnames(dataset) <- c('candidate_name','contributor_city','contributor_state','contributor_zip','contributor_employer','contributor_occupation','contributor_money','contributor_money_date')  
  dataset$search_occupation <- dataset$contributor_occupation
  # Remove refunds as they are not relevant due to not being active contributions during this time
  dataset$contributor_money <- as.numeric(sub("\\$","", dataset$contributor_money))
  dataset$search_occupation <- gsub("LAWYER", "ATTORNEY", dataset$search_occupation)
  dataset$search_occupation <- gsub("ATTORNEY AT LAW", "ATTORNEY", dataset$search_occupation)
  dataset$search_occupation <- gsub("PRESIDENT", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("VICE PRESIDENT","EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("VP","EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("VICE EXECUTIVE","EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("VICE-EXECUTIVE","EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("CEO", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("CFO", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("CPO", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("GLOBAL CHIEF RISK OFFICER", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("CHAIRMAN", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("MANAGEMENT/EXECUTIVE", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("FOUNDER/EXECUTIVE", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("SENIOR DIRECTOR", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("PRINCIPAL", "EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("EXECUTIVE/EXECUTIVE","EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("EXECUTIVE/EXECUTIVE","EXECUTIVE", dataset$search_occupation)
  dataset$search_occupation <- gsub("PRIVATE BANKER","BANKER", dataset$search_occupation)
  
  return(dataset[as.numeric(as.character(dataset$contributor_money)) > 0,])
}
get_candidate <- function(data, candidate) { 
  if(candidate == "All") { 
    return(data)
  } else { 
    subset_data <- data[data$candidate_name == candidate, ]
    return(subset_data)
  }  
}

get_employers <- function(data) { 
  employers_decreasing <- data$contributor_employer [order (data$contributor_money, decreasing = TRUE)]  
  employers <- subset(data, contributor_employer %in% employers_decreasing [1: 5])
  return(employers)
}

get_occupations <- function(data) { 
  occupations_decreasing <- data$search_occupation [order (data$contributor_money, decreasing = TRUE)]  
  occupations <- subset(data, search_occupation %in% occupations_decreasing [1: 5])
  return(occupations)
}

get_states <- function(data) { 
  states_decreasing <- data$contributor_state [order (data$contributor_money, decreasing = TRUE)]  
  states <- subset(data, contributor_state %in% states_decreasing [1: 10])
  return(states)
}

get_candidate_summary <- function(data) { 

  if(data == "All") { 
    text <- "<h1>Welcome</h1>In order to explore the dataset, please choose a specific candidate.<br><br>How to use this app:<br><br>Summary - A Wikipedia-introduction to each candidate.<br>Occupations - The top occupations provided by campaign contributors.<br>Employers - The top employers provided by campaign contributors.<br>States - The top states providing campaign contributions.<br>Raw - Investigate your own assumptions -- search for specific data like company names.<br><br>Test your own bias:<br><br><b>Which candidate do you think received the most contributions from pastors?<br><br>Which candidate received contributions from a writer on The Simpsons?<br><br>Which candidate received the most contributions from Google staff?<br><br>Who received the most contributions from actresses?"
  }
  if(data == "Bush, Jeb") { 
    text <- "<h1>Bush, Jeb</h1><br>John Ellis \"Jeb\" Bush (born February 11, 1953) is an American businessman and politician who served as the 43rd Governor of Florida from 1999 to 2007.<br>Bush is the second son of former President George H. W. Bush and former First Lady Barbara Bush, a brother of former President George W. Bush, and grandson of the late Senator Prescott Sheldon Bush. He grew up in Houston, Texas. He graduated from the Phillips Academy in Andover, Massachusetts and attended the University of Texas, where he earned a degree in Latin American affairs. Following his father's successful run for Vice President in 1980, he moved to Florida and pursued a career in real estate development. In 1986, Bush was named Florida's Secretary of Commerce, a position he held until his resignation in 1988 to help his father's successful campaign for the Presidency.<br><br>In 1994, Bush made his first run for office, losing the election for governor by less than two percentage points to the incumbent Lawton Chiles. Bush ran again in 1998 and defeated Lieutenant Governor Buddy MacKay with 55 percent of the vote. He ran for reelection in 2002 and won with 56 percent to become Florida's first two-term Republican governor.[2] During his eight years as governor, Bush was credited with initiating environmental improvements, such as conservation in the Everglades, supporting caps for medical malpractice litigation, moving Medicaid recipients to private systems, and instituting reforms to the state education system, including the issuance of vouchers and promoting school choice.<br><br>Read more at <a href=\"https://en.wikipedia.org/wiki/Jeb_Bush\">Wikipedia</a>"
  } 
  if(data == "Paul, Rand") { 
    text <- "<h1>Paul, Rand</h1><br>Randal Howard \"Rand\" Paul (born January 7, 1963) is an American politician and physician. Since 2011, Paul has served in the United States Senate as a member of the Republican Party representing Kentucky. He is the son of former U.S. Representative Ron Paul of Texas.<br><br>Born in Pittsburgh, Pennsylvania, Paul attended Baylor University and is a graduate of the Duke University School of Medicine. Paul began practicing ophthalmology in 1993 in Bowling Green, Kentucky, and established his own clinic in December 2007. Throughout Paul's life, he volunteered for his father's campaigns. In 2010, Paul entered politics by running for a seat in the United States Senate. Paul has described himself as a Constitutional conservative and a supporter of the Tea Party movement, and has advocated for a balanced budget amendment, term limits, and privacy reform.<br><br>On April 7, 2015, Paul officially announced his candidacy for the Republican nomination at the 2016 U.S. presidential election. <br><br>Read more at <a href=\"https://en.wikipedia.org/wiki/Rand_Paul\">Wikipedia</a>"
  }
  
  if(data == "Rubio, Marco") { 
    text <- "<h1>Rubio, Marco</h1>Marco Antonio Rubio (born May 28, 1971) is the junior United States Senator from Florida, serving since January 2011, and a candidate for President of the United States. A member of the Republican Party, he previously served as Speaker of the Florida House of Representatives.    
    <br><br>Rubio is a Cuban American native of Miami. He graduated from the University of Florida and the University of Miami School of Law. In the late 1990s, he served as a City Commissioner for West Miami and was elected to the Florida House of Representatives in 2000, representing the 111th House district. He was elected Speaker in September 2005.
    <br><br>Rubio announced a run for U.S. Senate in May 2009 after incumbent Republican Mel Martínez resigned. Initially trailing by double-digits against the incumbent Republican Governor Charlie Crist, Rubio eventually surpassed him in polling for the Republican nomination. Rubio won the Republican nomination after Crist opted instead to run with no party affiliation. In a three-way split against Crist and Democratic candidate Kendrick Meek, Rubio won the general election in November 2010 with 49 percent of the vote.
    <br><br>Rubio is one of three Cuban Americans in the Senate, along with Bob Menendez of New Jersey and Ted Cruz of Texas.[2]
    <br><br><a href=\"https://en.wikipedia.org/wiki/Marco_Rubio\">Read more at Wikipedia</a>"
  }
  if(data == "Perry, James R. (Rick)") { 
    text <- "<h1>Perry, James R. (Rick)</h1>James Richard \"Rick\" Perry (born March 4, 1950) is an American politician who served as the 47th Governor of Texas from December 2000 to January 2015. A Republican, he was elected Lieutenant Governor of Texas in 1998 and assumed the governorship in December 2000 when then-governor George W. Bush resigned to become President of the United States.
    <br><br>Perry was the longest serving governor in Texas state history. As a result, he was the only governor in modern Texas history to have appointed at least one person to every eligible state office, board, or commission position (as well as to several elected offices to which the governor can appoint someone to fill an unexpired term, such as six of the nine current members of the Texas Supreme Court).
    <br><br>Perry was elected to full gubernatorial terms in 2002, 2006 and 2010 and is the fourth Texas governor (after Allan Shivers, Price Daniel and John Connally) to serve three terms. With a tenure in office of 14 years, 30 days, Perry was, at the time he left office, the second longest serving current U.S. governor (after Terry Branstad of Iowa). Perry served as chairman of the Republican Governors Association in 2008 and again in 2011.[3]
    <br><br>On July 8, 2013, Perry announced that he would not seek re-election to his fourth term in the 2014 election.[4] On August 15, 2014, he was indicted on two felony charges related to his actions as governor. One of the two charges was dismissed in July 2015 as violative of the First Amendment. The other remains pending.[5][6][7][8]
    <br><br>On June 4, 2015, Perry launched his candidacy for the Republican nomination in the 2016 U.S. presidential election, but after poor performance in polling after the first debate, he suspended his campaign, withdrawing on September 11, 2015, the first in the field to exit.
    <br><br><a href=\"https://en.wikipedia.org/wiki/Rick_Perry\">Read more at Wikipedia</a>"
  }
  if(data == "Sanders, Bernard") { 
    text <- "<h1>Sanders, Bernard</h1>Bernard \"Bernie\" Sanders (born September 8, 1941) is an American politician and the junior United States Senator from Vermont. He is a candidate for the Democratic Party's nomination for president in the 2016 U.S. presidential election.[3][4][5]
    <br><br>Sanders is the longest-serving independent in U.S. congressional history. A self-described democratic socialist,[6][7][8][9] he favors policies similar to those of social democratic parties in Europe, particularly those instituted by the Nordic countries.[10][11][12] He caucuses with the Democratic Party and has been the ranking minority member on the Senate Budget Committee since January 2015.[13]    
    <br><br>Born in Brooklyn, New York, Sanders is a graduate of the University of Chicago. While a student, he was a member of the Young People's Socialist League and an active Civil Rights protest organizer for the Congress of Racial Equality and the Student Nonviolent Coordinating Committee.[14][15] In 1963, he participated in the March on Washington for Jobs and Freedom.
    <br><br>After settling in Vermont in 1968, Sanders ran unsuccessful third party campaigns for Governor and U.S. Senator in the early to mid-1970s. As an independent, Sanders was elected mayor of Burlington, Vermont's most populous city, in 1981. He was reelected three times before being elected to represent Vermont's at-large congressional district in the United States House of Representatives in 1990. He served as a congressman for 16 years before being elected to the U.S. Senate in 2006. In 2012, he was reelected by a large margin, capturing almost 71% of the popular vote.
    <br><br>Sanders is known as a leading progressive voice on issues such as income inequality,[7] universal healthcare, parental leave, climate change,[16] LGBT rights, and campaign finance reform.[17] He rose to national prominence following his 2010 filibuster[18][19] against the proposed extension of the Bush tax cuts. He is also outspoken on civil rights and civil liberties, and has been particularly critical of mass surveillance policies such as the USA PATRIOT Act,[20] as well as racial discrimination in the criminal justice system. He has long been critical of U.S. foreign policy, and was an early and outspoken opponent of the Iraq War.
    <br><br><a href=\"https://en.wikipedia.org/wiki/Bernie_Sanders\">Read more at Wikipedia</a>"
  }
  if(data == "Santorum, Richard J.") { 
    text <- "<h1>Santorum, Richard J.</h1>Richard John \"Rick\" Santorum (born May 10, 1958) is an American attorney and Republican Party politician. He served as a United States Senator representing Pennsylvania (1995–2007) and was the Senate's third-ranking Republican (2001–07).[1] He ran as a candidate for the 2012 Republican Party presidential nomination,[2] finishing second to the eventual Republican nominee Mitt Romney.
    <br><br>Born in Virginia, Santorum was raised primarily in Butler, Pennsylvania. He obtained an undergraduate degree from Pennsylvania State University, an M.B.A. from the University of Pittsburgh, and a J.D. from the Dickinson School of Law (now part of Penn State). Santorum worked as an attorney at Kirkpatrick & Lockhart, where he met Karen Garver. They married in 1990, and have seven living children (one child died shortly after birth). Santorum was elected to the U.S. House of Representatives to represent Pennsylvania's 18th congressional district in 1990 and later became a member of a group dubbed the \"Gang of Seven\".
    <br><br>Santorum was elected as a United States Senator for Pennsylvania in 1994. He served two terms until losing his re-election bid in 2006. A devout, practicing Catholic, Santorum is a social conservative who opposes same-sex marriage and artificial birth control. While serving as a senator, Santorum was the author of what came to be known as the Santorum Amendment, which promoted the teaching of intelligent design. In 2005, Santorum introduced the Workplace Religious Freedom Act along with Senator John Kerry.
    <br><br>In the years following his departure from the Senate, Santorum worked as a consultant, private-practice lawyer, and news contributor. On June 6, 2011, Santorum announced his run for the Republican nomination in the 2012 U.S. presidential election. Upon announcing his campaign suspension on April 10, 2012, he had won 11 primaries and caucuses and received nearly 4 million votes, making him the runner-up to eventual nominee Mitt Romney. Santorum officially endorsed Romney on May 7, 2012.[3] Santorum announced his candidacy for the 2016 presidential election on May 27, 2015.[4]
    <br><br><a href=\"https://en.wikipedia.org/wiki/Rick_Santorum\">Read more at Wikipedia</a>"
  }
  if(data == "Clinton, Hillary Rodham") { 
    text <- "Hillary Diane Rodham Clinton (born October 26, 1947) is an American politician who served as the 67th United States Secretary of State under President Barack Obama from 2009 to 2013. The wife of Bill Clinton, the 42nd President of the United States, she was First Lady of the United States during his tenure from 1993 to 2001. She served as a United States Senator from New York from 2001 to 2009.    
<br><br>An Illinois native, Hillary Rodham graduated from Wellesley College in 1969, where she became the first student commencement speaker, then earned her J.D. from Yale Law School in 1973. After a stint as a Congressional legal counsel, she moved to Arkansas, marrying Bill Clinton in 1975. She co-founded Arkansas Advocates for Children and Families in 1977, became the first female chair of the Legal Services Corporation in 1978, and was named the first female partner at Rose Law Firm in 1979. The National Law Journal twice listed her as one of the hundred most influential lawyers in America. While First Lady of Arkansas from 1979 to 1981 and 1983 to 1992, she led a task force that reformed Arkansas' education system, while sitting on the board of directors of Wal-Mart, among other corporations.
<br><br>As First Lady of the United States, her major initiative, the Clinton health care plan of 1993, failed to reach a vote in Congress. In 1997 and 1999, she played a leading role in advocating the creation of the State Children's Health Insurance Program, the Adoption and Safe Families Act and the Foster Care Independence Act. The only First Lady to have been subpoenaed, she testified before a federal grand jury in 1996 regarding the Whitewater controversy, although no charges against her related to this or other investigations during her husband's presidency were ever brought. Her marriage to the president was subject to considerable public discussion following the Lewinsky scandal of 1998, and overall her role as First Lady drew a polarized response from the American public.
<br><br>After moving to New York, Clinton was elected in 2000 as the first female senator from the state, the first and so far only First Lady ever to have sought elected office. Following the September 11 attacks, she voted for and supported military action in Afghanistan and the Iraq Resolution, but subsequently objected to the George W. Bush administration's conduct of the Iraq War, as well as most of Bush's domestic policies. Clinton was re-elected to the Senate in 2006. Running for the Democratic nomination in the 2008 presidential election, Clinton won more primaries and delegates than any other female candidate in American history, but ultimately lost the nomination to Obama.
<br><br>As Secretary of State in the Obama administration from January 2009 to February 2013, Clinton was at the forefront of the U.S. response to the Arab Spring and advocated the U.S. military intervention in Libya. She took responsibility for security lapses related to the 2012 Benghazi attack, which resulted in the deaths of American consulate personnel, but defended her personal actions in regard to the matter. Clinton visited more countries than any other Secretary of State. She viewed \"smart power\" as the strategy for asserting U.S. leadership and values, by combining military power with diplomacy and American capabilities in economics, technology, and other areas. She encouraged empowerment of women everywhere and used social media to communicate the U.S. message abroad. Leaving office at the end of Obama's first term, she authored her fifth book and undertook speaking engagements before announcing her second run for the Democratic nomination in the 2016 presidential election.
    <br><br><a href=\"https://en.wikipedia.org/wiki/Hillary_Clinton\">Read more at Wikipedia</a>"
  }
  if(data == "Carson, Benjamin S.") { 
    text <- "Benjamin Solomon \"Ben\" Carson, Sr. (born September 18, 1951) is an American author and retired Johns Hopkins neurosurgeon. On May 4, 2015, Carson announced he was running for the Republican nomination in the 2016 presidential election at a rally in Detroit, his hometown.[1]
    <br><br>Carson was the first surgeon to successfully separate twins conjoined at the head. In 2008, he was awarded the Presidential Medal of Freedom by President George W. Bush.
    <br><br>After delivering a widely publicized speech at the 2013 National Prayer Breakfast, he became a popular conservative figure in political media for his views on social and political issues.[2]
    <br><br><a href=\"https://en.wikipedia.org/wiki/Ben_Carson\">Read more at Wikipedia</a>"
  }

  if(data == "Cruz, Rafael Edward 'Ted'") { 
    text <- "Rafael Edward \"Ted\" Cruz (born December 22, 1970) is the junior U.S. Senator from Texas. A Republican, Cruz was elected to the U.S. Senate in 2012 and is the first Hispanic American to serve as a U.S. senator representing Texas.[4][2][5] He is the chairman of the subcommittee on the Oversight, Agency Action, Federal Rights and Federal Courts, U.S. Senate Judiciary Committee.[6] He is also the chairman of the United States Senate Commerce Subcommittee on Space, Science and Competitiveness, U.S. Senate Commerce Committee. On March 23, 2015, Cruz announced during a rally at Liberty University he would run for the Republican Party nomination in the 2016 U.S. Presidential election.
    <br><br>Between 1999 and 2003, Cruz was the director of the Office of Policy Planning at the Federal Trade Commission, an associate deputy attorney general at the United States Department of Justice, and domestic policy advisor to President George W. Bush on the 2000 George W. Bush presidential campaign. He served as Solicitor General of Texas from 2003 to May 2008, after being appointed by Texas Attorney General Greg Abbott.[7] He was the first Hispanic,[5][8] the youngest[5][9] and the longest-serving solicitor general in Texas history.[10] Cruz was also an adjunct professor of law at the University of Texas School of Law in Austin, from 2004 to 2009.[11][12] While there, he taught U.S. Supreme Court litigation.[11] Cruz is one of three Senators of Cuban descent.[13]
    <br><br>Cruz was the Republican nominee for the Senate seat vacated by fellow Republican Kay Bailey Hutchison.[14] On July 31, 2012, he defeated Lieutenant Governor David Dewhurst in the Republican primary runoff, 57%–43%.[15] Cruz defeated former state Representative Paul Sadler in the general election on November 6, 2012. He prevailed 56%–41% over Sadler.[15][16] Cruz openly identifies with the Tea Party movement and has been endorsed by the Republican Liberty Caucus.[17] On November 14, 2012, Cruz was appointed vice-chairman of the National Republican Senatorial Committee.[18]
    <br><br><a href=\"https://en.wikipedia.org/wiki/Ted_Cruz\">Read more at Wikipedia</a>"
  }

  if(data == "O'Malley, Martin Joseph") { 
    text <- "Martin Joseph O'Malley (born January 18, 1963) was the 61st Governor of Maryland, from 2007 to 2015, and is running for President of the United States in the 2016 election. Prior to being elected as Governor, he served as the Mayor of Baltimore from 1999 to 2007 and was a Baltimore City Councilor from 1991 to 1999.
  <br><br>O'Malley served as the Chair of the Democratic Governors Association from 2011 to 2013, while serving as governor of Maryland. Following his departure from public office in early 2015, he was appointed to the Johns Hopkins University's Carey Business School as a visiting professor focusing on government, business, and urban issues.
  <br><br>As Governor, in 2011, he signed a law that would make illegal immigrants brought to the United States as children eligible for in-state college tuition, and in 2012, he signed a law to legalize same-sex marriage in Maryland. Each law was put to a voter referendum in the 2012 general election and upheld by a majority of the voting public.
  <br><br>O'Malley publicly announced his candidacy in the 2016 presidential election on May 30, 2015, in Baltimore, Maryland, and filed his candidacy form seeking the Democratic Party nomination with the Federal Election Commission on May 29, 2015.[1][2]
  <br><br><a href=\"https://en.wikipedia.org/wiki/Martin_O%27Malley\">Read more at Wikipedia</a>"
  
  }

  if(data == "Fiorina, Carly") { 
    text <- "Carly Fiorina (born Cara Carleton Sneed, September 6, 1954) is an American Republican politician and former business executive who currently chairs the non-profit philanthropic organization Good360.[8][9]
    <br><br>In 1980, Fiorina started at AT&T and its equipment and technology spin-off, Lucent Technologies, and rose through the ranks to become an executive. As chief executive officer of Hewlett-Packard (HP) from 1999 to 2005, she was the first woman to lead a top-20 company as ranked by Fortune magazine.[10]
    <br><br>In 2002, Fiorina oversaw the biggest high-tech merger in history up to that time, with rival computer company Compaq, which made HP the world's largest personal computer manufacturer.[11][12] During Fiorina's tenure, HP laid off 30,000 U.S. employees. By 2004 the number of HP employees was about the same as the pre-merger total of HP and Compaq combined, and that 2004 number included roughly 8,000 employees of other companies acquired by HP since 2001.[13][14][15] On February 9, 2005, the HP board of directors forced Fiorina to resign as chief executive officer and chair due to declining stock value, disappointing earning reports, disagreements about the company's performance, and her resistance to transferring authority to division heads.[16][17][18]
    <br><br>After leaving HP, Fiorina served on the boards of several organizations.[19] She was an adviser to Republican John McCain's 2008 presidential campaign. She ran for the United States Senate in California in 2010 and won a three-person race for the Republican nomination, but lost the general election to incumbent Democratic Senator Barbara Boxer by 10 percentage points.[20][21]
    <br><br>On May 4, 2015, Fiorina announced her candidacy for the Republican nomination in the 2016 U.S. presidential election.[1]
    <br><br><a href=\"https://en.wikipedia.org/wiki/Carly_Fiorina\">Read more at Wikipedia</a>"
  }

  if(data == "Graham, Lindsey O.") { 
    text <- "Lindsey Olin Graham (born July 9, 1955) is an American politician and member of the Republican Party, who has served as a United States Senator from South Carolina since 2003, and has been the senior Senator from South Carolina since 2005.
    <br><br>Born in Central, South Carolina, Graham graduated from the University of South Carolina in 1977. He received his Juris Doctor from the University of South Carolina School of Law in 1981. He served in the United States Air Force from 1982 to 1988 and served as a Guardsman first in the South Carolina Air National Guard then in the Air Force Reserves, attaining the rank of colonel. He worked as a lawyer in private practice before he was elected to the South Carolina House of Representatives in 1992, serving one term from 1993 to 1995. He then served in the United States House of Representatives, representing South Carolina's 3rd congressional district from 1995 to 2003. He was elected to four terms, receiving at least 60% of the vote each time.
    <br><br>In 2002, Graham ran for the U.S. Senate after eight-term Republican incumbent Strom Thurmond announced his retirement. Graham won the primary unopposed and defeated Democratic opponent Alex Sanders in the general election. Graham was re-elected to a second term in 2008, defeating Bob Conley. He won a third term in 2014, defeating Democrat Brad Hutto and Independent Thomas Ravenel.
    <br><br>Referred to by some as a \"war hawk\" and \"interventionist\",[1] Graham is known in the Senate for his advocacy of a strong national defense, his support of the military, and as an advocate of strong United States leadership in world affairs.[2] He is also known for his willingness to be bipartisan and work with Democrats on issues like global warming, tax reform and immigration reform and his belief that judicial nominees should not be opposed solely on their philosophical positions.[3][4][5][6][7][8] He is also a critic of the Tea Party movement, arguing for a more inclusive Republican Party.
    <br><br>On May 18, 2015, Graham informally announced his candidacy for President of the United States,[14] followed by a formal announcement on June 1, 2015, in his hometown of Central, South Carolina.[15]
    <br><br><a href=\"https://en.wikipedia.org/wiki/Lindsey_Graham\">Read more at Wikipedia</a>"
  }

  if(data == "Huckabee, Mike") { 
  text <- "Michael Dale \"Mike\" Huckabee (born August 24, 1955) is an American politician, Christian minister, author, and commentator who served as the 44th Governor of Arkansas from 1996 to 2007.[4] He is a candidate for U.S. president in the 2016 election and was a candidate in the 2008 United States Republican presidential primaries. He won the 2008 Iowa Republican caucuses and finished second in delegate count and third in both popular vote and number of states won, behind nominee John McCain and Mitt Romney.[5]
  <br><br>Beginning in 2008, Huckabee hosted the Fox News Channel talk show Huckabee, ending the show in January 2015 in order to explore a potential bid for the presidency.[6] From April 2012 through December 2013, he hosted a daily radio program, The Mike Huckabee Show, on weekday afternoons for Cumulus Media Networks.[7] Huckabee is the author of several best-selling books, an ordained Southern Baptist minister noted for his evangelical views,[8] a musician, and a public speaker. He is also a political commentator on The Huckabee Report.[9]
  <br><br>Huckabee announced his candidacy for the Republican nomination in the 2016 presidential election,[10] in Hope, Arkansas on May 5, 2015.[10] It is his second run for the U.S. presidency.
  <br><br><a href=\"https://en.wikipedia.org/wiki/Mike_Huckabee\">Read more at Wikipedia</a>"
  }

  if(data == "Jindal, Bobby") { 
  
    text <- "Piyush \"Bobby\" Jindal (born June 10, 1971)[1] is an American politician who is the 55th governor of Louisiana, a former US Congressman, and former vice chairman of the Republican Governors Association.[3]<br><br>In 1996, Governor Murphy Foster appointed Jindal secretary of the Louisiana Department of Health and Hospitals, and in 1999 he was appointed president of the University of Louisiana System. In 2001, Jindal was appointed as the principal adviser to Tommy Thompson, the United States Secretary of Health and Human Services by the 43rd President, George W. Bush.<br><br>He first ran for governor in 2003 and won a plurality in the nonpartisan blanket primary but lost in the general election to the Democratic candidate, Kathleen Blanco. He then won a seat in the United States House of Representatives in the 2004 elections. The second Indian American in Congress, he was re-elected in 2006. He ran for governor again in 2007 and secured an outright majority in the first round of balloting; in doing so, he became the first Indian American governor in the United States.[4] He was re-elected in a landslide in 2011.<br><br>On June 24, 2015, Jindal announced his candidacy for the Republican nomination in the 2016 presidential election.
  <br><br><a href=\"https://en.wikipedia.org/wiki/Bobby_Jindal\">Read more at Wikipedia</a>"
  
  }

  if(data == "Pataki, George E.") { 
    text <- "George Elmer Pataki (/pəˈtɑːki/; born June 24, 1945) is an American lawyer and politician who served as the 53rd Governor of New York (1995–2006). A member of the Republican Party, Pataki was a lawyer who was elected Mayor of his home town of Peekskill, later going on to be elected to State Assembly, then State Senate. In 1994, he ran for Governor against three-term incumbent Mario Cuomo, defeating him by over a three-point margin as part of the Republican Revolution of 1994. Pataki, succeeding a three-term Governor, would himself be elected to three consecutive terms, and was one of only three Republican governors of New York elected since 1923, the others being Thomas Dewey and Nelson Rockefeller.<br><br>In early 2015, Pataki began exploring a candidacy for the Republican nomination for President of the United States in 2016,[2] and announced his candidacy on May 28, 2015.<br><br><a href=\"https://en.wikipedia.org/wiki/George_Pataki\">Read more at Wikipedia</a>"
  }

  return(text) 
} 


file = get_dataset()
dataset = load_dataset(file)
unique_candidates <- unique(dataset$candidate_name)

shinyServer(function(input, output) {
  candidate_data <- reactive({
    candidate_data <- get_candidate(dataset, input$candidate)  
  })
  candidate_summary <- reactive({
    candidate_summary <- get_candidate_summary(input$candidate)
  })
  employers <- reactive({ 
    employers <- get_employers(candidate_data()[!is.na(candidate_data()$contributor_employer),])
  })

  occupations <- reactive({ 
    occupations <- get_occupations(candidate_data()[!is.na(candidate_data()$search_occupation),])    
  }) 
  
  states <- reactive({ 
    states <- get_states(candidate_data()[!is.na(candidate_data()$contributor_state),])
    })
  
  output$occupationsPlot <- renderPlot({
    ggplot(data = occupations(), aes(x=search_occupation, y=contributor_money)) +scale_y_continuous("Contributions ($)", labels=comma) + geom_bar(stat="identity",fill="blue") +
      ggtitle("Top Occupations for campaign contributions") + labs(x="Occupation", y="Contributions ($)")
  })
  output$employersPlot <- renderPlot({
    ggplot(data = employers(), aes(x=contributor_employer, y=contributor_money)) +scale_y_continuous("Contributions ($)", labels=comma) + geom_bar(stat="identity",fill="blue") +
      ggtitle("Top Employers for campaign contributions") + labs(x="Employers", y="Contributions ($)")
  })
  output$statesPlot <- renderPlot({
    ggplot(data = states(), aes(x=contributor_state, y=contributor_money)) +scale_y_continuous("Contributions ($)", labels=comma) + geom_bar(stat="identity",fill="blue") +
      ggtitle("Top States for campaign contributions") + labs(x="States", y="Contributions ($)")
  })
  
  output$table <- renderDataTable(candidate_data())

  output$summary <- renderText(candidate_summary())
  
  
})