library(tidyverse)
library(readxl)
library(purrr)
library(data.table)

# read in fall 16 to spring 20 data
# all data in one file and in one sheet

# function used to give correct semester information
semesterFunc <- function( str)
{
  prefix <- if_else( substr(str, 1,1) == "1", "s", "f")
  val <- str_c(prefix, str_sub( str, -2))
  return( val)
}

# read student data
f16s20 <- read.csv( "data-raw/F2016-S2020 MAT125 Data with student id.csv")
f16s20 <- f16s20[ !is.na( f16s20$Gradable.Item.Score)]

f16s20$semester <- sapply(f16s20$Course.Start.Date, semesterFunc )

f16s20$ï..CourseID <- str_extract( f16s20$ï..CourseID, "\\d+")

f16s20 <- rename( f16s20, "section_Id" = 1)


rawF20s1 <- read_excel("data-raw/Fall2020 data/f20s1.xlsx")
rawF20s2 <- read_excel("data-raw/Fall2020 data/f20s2.xlsx")
rawF20s4 <- read_excel("data-raw/Fall2020 data/f20s4.xlsx")
rawF20s5 <- read_excel("data-raw/Fall2020 data/f20s5.xlsx")
rawF20s6 <- read_excel("data-raw/Fall2020 data/f20s6.xlsx")
rawF20s7 <- read_excel("data-raw/Fall2020 data/f20s7.xlsx")
rawF20s8 <- read_excel("data-raw/Fall2020 data/f20s8.xlsx")
rawF20s9 <- read_excel("data-raw/Fall2020 data/f20s9.xlsx")
rawF20s13 <- read_excel("data-raw/Fall2020 data/f20s13.xlsx")

rawF20s1 <- select( rawF20s1, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s1 <- rawF20s1 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s1$professor <- "Weinberger,C"
# rawF20s1$section_Id <- "1" #actual section id replaceing all the following
rawF20s1$section_Id <- "1"

rawF20s2 <- select( rawF20s2, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s2 <- rawF20s2 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s2$professor <- "Weinberger,C"
# rawF20s2$section_Id <- "2"
rawF20s2$section_Id <- "2"

rawF20s4 <- select( rawF20s4, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s4 <- rawF20s4 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s4$professor <- "Christina,C"
# rawF20s4$section_Id <- "4"
rawF20s4$section_Id <- "3"


rawF20s5 <- select( rawF20s5, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s5 <- rawF20s5 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s5$professor <- "Loucks,W"
# rawF20s5$section_Id <- "5"
rawF20s5$section_Id <- "4"

rawF20s6 <- select( rawF20s6, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s6 <- rawF20s6 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s6$professor <- "Loucks,W"
# rawF20s6$section_Id <- "6"
rawF20s6$section_Id <- "5"

rawF20s7 <- select( rawF20s7, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s7 <- rawF20s7 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s7$professor <- "Sima,J"
# rawF20s7$section_Id <- "7"
rawF20s7$section_Id <- "6"

rawF20s8 <- select( rawF20s8, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s8 <- rawF20s8 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s7$professor <- "Sima,J"
# rawF20s7$section_Id <- "8"
rawF20s7$section_Id <- "7"

rawF20s9 <- select( rawF20s9, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s9 <- rawF20s9 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s9$professor <- "Abdulrazzaq,H"
# rawF20s9$section_Id <- "9"
rawF20s9$section_Id <- "8"

rawF20s13 <- select( rawF20s13, 1, 2, 3, 11, 34, 54, 73, 98 )
rawF20s13 <- rawF20s13 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF20s13$professor <- "Abdulrazzaq,H"
# rawF20s13$section_Id <- "13"
rawF20s13$section_Id <- "9"

rawF20 <- bind_rows(rawF20s1, rawF20s2, rawF20s4, rawF20s5, rawF20s6,
                    rawF20s7, rawF20s8, rawF20s9, rawF20s13)
rawF20$semester <- "f20"

# read spring 2021 data
# data is split in multiple files be section

rawS21s2 <- read_excel("data-raw/Spring2021 data/s21s2.xlsx")
rawS21s3 <- read_excel("data-raw/Spring2021 data/s21s3.xlsx")
rawS21s4 <- read_excel("data-raw/Spring2021 data/s21s4.xlsx")
rawS21s5 <- read_excel("data-raw/Spring2021 data/s21s5.xlsx")
rawS21s6 <- read_excel("data-raw/Spring2021 data/s21s6.xlsx")
rawS21s7 <- read_excel("data-raw/Spring2021 data/s21s7.xlsx")
rawS21s8 <- read_excel("data-raw/Spring2021 data/s21s8.xlsx")
rawS21s9 <- read_excel("data-raw/Spring2021 data/s21s9.xlsx")
rawS21s10 <- read_excel("data-raw/Spring2021 data/s21s10.xlsx")
rawS21s14 <- read_excel("data-raw/Spring2021 data/s21s14.xlsx")

rawS21s2 <- select( rawS21s2, 1, 2, 3, 10, 31, 48, 65, 86 )
rawS21s2 <- rawS21s2 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s2$professor <- "Weinberger,C"
# rawS21s2$section_Id <- "2"
rawS21s2$section_Id <- "10"

rawS21s3 <- select( rawS21s3, 1, 2, 3, 10, 31, 48, 65, 86 )
rawS21s3 <- rawS21s3 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s3$professor <- "Weinberger,C"
# rawS21s3$section_Id <- "3"
rawS21s3$section_Id <- "11"

rawS21s4 <- select( rawS21s4, 1, 2, 3, 10, 31, 48, 65, 86 )
rawS21s4 <- rawS21s4 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s4$professor <- "Abdulrazzaq,H"
# rawS21s4$section_Id <- "4"
rawS21s4$section_Id <- "12"

rawS21s5 <- select( rawS21s5, 1, 2, 3, 10, 31, 48, 65, 86 )
rawS21s5 <- rawS21s5 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s5$professor <- "Abdulrazzaq,H"
# rawS21s5$section_Id <- "5"
rawS21s5$section_Id <- "13"

rawS21s6 <- select( rawS21s6, 1, 2, 3, 10, 31, 48, 65, 86 )
rawS21s6 <- rawS21s6 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s6$professor <- "Sima,J"
# rawS21s6$section_Id <- "6"
rawS21s6$section_Id <- "14"

rawS21s7 <- select( rawS21s7, 1, 2, 3, 10, 31, 48, 65, 86 )
rawS21s7 <- rawS21s7 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s7$professor <- "Sima,J"
# rawS21s7$section_Id <- "7"
rawS21s7$section_Id <- "15"


rawS21s8 <- select( rawS21s8, 1, 2, 3, 11, 32, 49, 66, 87 )
rawS21s8 <- rawS21s8 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s8$professor <- "Barnes,J"
# rawS21s8$section_Id <- "8"
rawS21s8$section_Id <- "16"


rawS21s9 <- select( rawS21s9, 1, 2, 3, 11, 31, 48, 65, 86 )
rawS21s9 <- rawS21s9 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s9$professor <- "Christina,C"
# rawS21s9$section_Id <- "9"
rawS21s9$section_Id <- "17"


rawS21s10 <- select( rawS21s10, 1, 2, 3, 11, 32, 49, 66, 87 )
rawS21s10 <- rawS21s10 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s10$professor <- "Singh,S"
# rawS21s10$section_Id <- "10"
rawS21s10$section_Id <- "18"

rawS21s14 <- select( rawS21s14, 1, 2, 3, 10, 31, 48, 65, 86 )
rawS21s14 <- rawS21s14 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawS21s14$professor <- "Singh,S"
# rawS21s14$section_Id <- "14"
rawS21s14$section_Id <- "19"

rawS21 <- bind_rows(rawS21s2,rawS21s3, rawS21s4, rawS21s5, rawS21s6,
                    rawS21s7, rawS21s8, rawS21s9, rawS21s10)
rawS21$semester <- "s21"


rawF21s2 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                      sheet = "Grades Sec 2")

rawF21s2 <- select( rawF21s2, 1, 2, 3, 15, 11, 12, 13, 14 )

rawF21s2 <- rawF21s2 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s2$professor <- "Jibrin,S"
# rawF21s2$section_Id <- "2"
rawF21s2$section_Id <- "20"


rawF21s3 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 3")
rawF21s3 <- select( rawF21s3, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s3 <- rawF21s3 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s3$professor <- "Jibrin,S"
# rawF21s3$section_Id <- "3"
rawF21s3$section_Id <- "21"

rawF21s4 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 4")
rawF21s4 <- select( rawF21s4, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s4 <- rawF21s4 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s4$professor <- "Libberton,A"
# rawF21s4$section_Id <- "4"
rawF21s4$section_Id <- "22"

rawF21s6 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 6")
rawF21s6 <- select( rawF21s6, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s6 <- rawF21s6 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s6$professor <- "Arlie,J"
# rawF21s6$section_Id <- "6"
rawF21s6$section_Id <- "23"

rawF21s8 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 8")
rawF21s8 <- select( rawF21s8, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s8 <- rawF21s8 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s8$professor <- "Arlie,J"
# rawF21s8$section_Id <- "8"
rawF21s8$section_Id <- "24"

rawF21s9 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 9")
rawF21s9 <- select( rawF21s9, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s9 <- rawF21s9 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s9$professor <- "	Kennedy,E"
# rawF21s9$section_Id <- "9"
rawF21s9$section_Id <- "25"

rawF21s10 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 10")
rawF21s10 <- select( rawF21s10, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s10 <- rawF21s10 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s10$professor <- "Libberton,A"
# rawF21s10$section_Id <- "10"
rawF21s10$section_Id <- "26"

rawF21s11 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 11")
rawF21s11 <- select( rawF21s11, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s11 <- rawF21s11 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s11$professor <- "Leland,J"
# rawF21s11$section_Id <- "11"
rawF21s11$section_Id <- "27"

rawF21s12 <- read_excel( "data-raw/Fall2021 Grades and MLM Course ID.xlsx",
                        sheet = "Grades Sec 12")
rawF21s12 <- select( rawF21s12, 1, 2, 3, 15, 11, 12, 13, 14 )
rawF21s12 <- rawF21s12 %>%
  rename( "F1" = 4,
          "M1T1" = 5,
          "M2T1" = 6,
          "M3T1" = 7,
          "M4T1" = 8)
rawF21s12$professor <- "Leland,J"
# rawF21s12$section_Id <- "12"
rawF21s12$section_Id <- "28"

rawF21 <- bind_rows(rawF21s2, rawF21s3, rawF21s4, rawF21s6,
                    rawF21s8, rawF21s9, rawF21s10, rawF21s11,
                    rawF21s12)
rawF21$semester <- "f21"

rawData <- bind_rows( rawF20, rawF21, rawS21)

rawData <- rawData %>%
  rename( "LastName" = 1,
          "FirstName" = 2)

rawData <- rawData %>%
  pivot_longer(
    F1:M4T1,
    names_to  = 'test',
    values_to = 'score')

f16s20 <- f16s20 %>%
  rename( "professor" = 5,
          "LastName" = 8,
          "FirstName" = 9,
          "Username" = 10,
          "test" = 11,
          "score" = 15) %>%
  select( 1, 5, 8, 9, 10, 11, 15, 16)




# here everything is selected that has "F1" and "test" in the test column
f16s20 <- f16s20 %>% filter(grepl('F1|Test|test', test))
unique(f16s20$test)

# typo fix
f16s20$test <- gsub( "M3T1: Module 3 Test 2nd Attempt", "M3T2", f16s20$test)
unique(f16s20$test)

# one semester had three attempts given for a test
# the one with the asterisk was always the second attempt
# the first one was M2T1: Module 2 Test or M2T1: Module 2 Test - 2 attempts
f16s20$test <- gsub( "M2T1: Module 2 Test - 2 attempts\\*", "M2T2", f16s20$test)
unique(f16s20$test)

f16s20$test <- gsub( "M2T1: Module 2 Test - 2 attempts", "M2T1", f16s20$test)

unique(f16s20$test)
# remove all quizzes
f16s20 <- f16s20 %>% filter( !grepl( 'Quiz|quiz', test))

f16s20$Username <- sub("@.*","",f16s20$Username)

allData <- bind_rows( rawData, f16s20)

allData <- allData %>% mutate(
  name = paste0( LastName,FirstName, Username)
)

# assign random ids to students
blind_ID_df <- data.frame(
  name = unique(allData$name),
  Id = sample( 10000 : 99999, length( unique(allData$name)))
)

write.csv(blind_ID_df, "blind_ids.csv")

allData <- subset(allData, select = -c(LastName,FirstName, Username))

blindData <- full_join( blind_ID_df, allData, by = "name")
blindData <- blindData %>% select( -c( name))
blindData <- blindData %>% filter(!is.na(test))

# assign random ids to professors
blind_prof_ID_df <- data.frame(
  professor = unique(blindData$professor),
  professor_Id = LETTERS[ sample( 1 : 26, length( unique( allData$professor)))]
)

write.csv(blind_prof_ID_df, "blind_prof_ids.csv")

blindData <- full_join( blindData, blind_prof_ID_df, by = "professor")
blindData <- blindData %>% select( -c( professor))

unique(blindData$test)

blindData$test <- gsub('M3TP1: Module 3 Practice Test with Learning Aids', 'M3T1PA', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M4TP1: Module 4 Practice Test with Learning Aids', 'M4T1PA', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M1TP1: Module 1 Practice Test with Learning Aids', 'M1T1PA', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M2TP1: Module 2 Practice Test with Learning Aids', 'M2T1PA', blindData$test)
unique(blindData$test)

blindData$test <- gsub('Module', 'M', blindData$test)
unique(blindData$test)

blindData$test <- gsub('Test (1st Attempt) Honor Code Prerequisite', 'T1H', fixed= T, blindData$test)
unique(blindData$test)

blindData$test <- gsub('Test (2nd Attempt) Honor Code Prerequisite', 'T2H',fixed= T, blindData$test)
unique(blindData$test)


# dont need due to removing quizzes on line 428
# blindData$test <- gsub('Test (1st Attempt) Honor Code Prereq Quiz', 'T1H',fixed= T, blindData$test)
# unique(blindData$test)
#
# blindData$test <- gsub('Test (2nd Attempt) Honor Code Prereq Quiz', 'T2H',fixed= T, blindData$test)
# unique(blindData$test)

# use lower case "p" for pre tests

blindData$test <- gsub('M1P1: M 1 Pretest', 'M1T1p', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M2P1: M 2 Pretest', 'M2T1p', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M3P1: M 3 Pretest A', 'M3T1p', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M3P1: M 3 Pretest', 'M3T1p', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M3TP1: M 3 Pretest', 'M3T1p', blindData$test)
unique(blindData$test)

blindData$test <- gsub('M4P1: M 4 Pretest', 'M4T1p', blindData$test)
unique(blindData$test)

# M1T1: M 1 Test (Copy) was a duplicate T1 test entry for a single student
blindData <- blindData %>%  filter( test != "M1T1: M 1 Test (Copy)")
unique(blindData$test)

# remove all characters after the ":"
blindData$test <- gsub('\\:.*', '', blindData$test)
unique(blindData$test)

blindData$test <- gsub('P1', '1P', blindData$test)
unique(blindData$test)

blindData$test <- gsub(" ", "", blindData$test)


blindData$learning_aid <- if_else( str_detect( blindData$test, "A"), 1, 0)

blindData$honors_code <- if_else( str_detect( blindData$test, "H"), 1, 0)

blindData$practice_test <- if_else( str_detect( blindData$test, "P"), 1, 0)

# add pretest column
blindData$pre_test <- if_else( str_detect( blindData$test, "p"), 1, 0)

blindData <- separate( blindData, test, c( "module_final", "test_attempt"), sep = c(2, 4))

unique(blindData$test_attempt)

blindData$test_attempt <- gsub("P1", "", blindData$test_attempt)
blindData$test_attempt <- gsub("P", "", blindData$test_attempt)

unique(blindData$test_attempt)

blindData <- blindData %>% mutate(
  test_attempt = ifelse( module_final == "F1", "T1", test_attempt)
)

blindData$season <- if_else( str_detect( blindData$semester, "f"), "fall", "spring")

blindData$year <- sub('\\w', '', blindData$semester)

blindData <- subset( blindData, select = -semester)



# m1t2 <- blindData %>% filter( module_final == "M1" & test_attempt == "T2") #%>%
  # filter( score != 0)

m1 <- blindData %>% filter( module_final == "M1")
not_m1 <- blindData %>%  filter ( module_final != "M1")

# blindData2 <- blindData %>% filter( module_final != "M1" & test_attempt != "T2")

m1_not_t2 <- m1 %>% filter( test_attempt != "T2")

m1_t2 <- m1 %>% filter( test_attempt == "T2") %>% filter( score != 0)

mat125data <- rbind( not_m1, m1_not_t2, m1_t2)

mat125data  <- mat125data %>%
  mutate( season = fct_relevel( season, "spring")) %>%
  mutate( module_final = fct_relevel( module_final, "M1", "M2", "M3", "M4", "F1"))

# blindData2$module_final %>% unique()

write.csv( blindData, "blindData.csv")
write.csv( mat125data, "mat125data.csv")


usethis::use_data( mat125data, overwrite = T)
usethis::use_data( blindData, overwrite = T)
