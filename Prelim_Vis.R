library(ggplot2)

data <- read.csv("~/Desktop/Capstone_project_data/Cleaned_Up_data/cleaned_up_occt_data.csv")


data <- data[data$ACADEMIC_PERIOD_DESC != "Spring_x0020_2023", ]
data <- data[data$ACADEMIC_PERIOD_DESC != "Winter_x0020_2024", ]
data$ACADEMIC_PERIOD_DESC[data$ACADEMIC_PERIOD_DESC == "Spring_x0020_2024"] <- "Spring 2024"



ggplot(data, aes(x = reorder(ACADEMIC_PERIOD_DESC, -table(ACADEMIC_PERIOD_DESC)[ACADEMIC_PERIOD_DESC]))) +
  geom_bar(fill = "#112446") +
  labs(
    title = "Frequency count of the academic periods in data",
    x = "Academic Period",
    y = "Frequency count in data"
  ) +
  theme_minimal()
  
  
  ggplot(data) +
    aes(x = CURRENT_AGE) +
    geom_histogram(bins = 30L, fill = "#112446") +
    labs(
      title = "Distribution of ages in the data",
      x = "age",
      y = "frequency count"
    ) +
    theme_minimal()


#Gender
data$GENDER_IDENTITY_DESC[data$GENDER_IDENTITY_DESC == "Woman"] <- "Female"
data$GENDER_IDENTITY_DESC[data$GENDER_IDENTITY_DESC == "Man"] <- "Male"
data <- data[data$GENDER_IDENTITY_DESC != "Trans_x0020_man", ]
data <- data[data$GENDER_IDENTITY_DESC != "Trans_x0020_woman", ]
data <- data[data$GENDER_IDENTITY_DESC != "Different_x0020_Identity", ]

ggplot(data) +
  aes(x = GENDER_IDENTITY_DESC) +
  geom_bar(fill = "#112446") +
  labs(
    title = "Frequency account of gender identities in data",
    x = "Gender identities",
    y = "frequency count"
  ) +
  theme_minimal()
  
  
ggplot(data) +
  aes(x = Month) +
  geom_histogram(bins = 30L, fill = "#112446") +
  labs(
    title = "Frequency of rides per month",
    x = "month",
    y = "frequency count"
  )
  theme_minimal()


data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_CCPA"] <- "GD CCPA"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_CCPA_Online"] <- "GD CCPA Online"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_Grad_x0020_CCPA"] <- "GD Grad CCPA"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_Grad_x0020_School"] <- "GD Grad School"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_CCPA_x0020_Online"] <- "GD CCPA Online"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_Harpur"] <- "GD Harpur"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_Management"] <- "GD Management"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_Nursing"] <- "GD Nursing"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_Pharmacy"] <- "GD Pharmacy"
data$COLLEGE_DESC[data$COLLEGE_DESC == "GD_x0020_Watson"] <- "GD Watson"
data$COLLEGE_DESC[data$COLLEGE_DESC == "UG_x0020_CCPA"] <- "UG CCPA"
data$COLLEGE_DESC[data$COLLEGE_DESC == "UG_x0020_CEO"] <- "UG CEO"
data$COLLEGE_DESC[data$COLLEGE_DESC == "UG_x0020_Harpur"] <- "UG Harpur"
data$COLLEGE_DESC[data$COLLEGE_DESC == "UG_x0020_Management"] <- "UG Management"
data$COLLEGE_DESC[data$COLLEGE_DESC == "UG_x0020_Nursing"] <- "UG Nursing"
data$COLLEGE_DESC[data$COLLEGE_DESC == "UG_x0020_Watson"] <- "UG Watson"

ggplot(data, aes(x = reorder(COLLEGE_DESC, -table(COLLEGE_DESC)[COLLEGE_DESC]))) +
  geom_bar(fill = "#112446") +
  labs(
    title = "frequency of college description",
    x = "College description",
    y = "frequency"
  )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



ggplot(data) +
  aes(x = PROGRAM_LEVEL_DESC) +
  geom_bar(fill = "#112446") +
  labs(
    title = "program type frequency count",
    x = "program type",
    y = "frequency count"
  ) +
  theme_minimal() 

unique(data['StopName'])
unique(data['CorridorName'])


library(dplyr)
data$StopName[data$StopName == "University_x0020_Union"] <- "University Union"


data_filtered <- data %>%
  group_by(StopName) %>%
  filter(n() >= 25000)

ggplot(data_filtered, aes(x = reorder(StopName, -table(StopName)[StopName]))) +
  geom_bar(fill = "#112446") +
  labs(
    title = "frequency count of stops (only stops that appear >25000 times)",
    x = "Stop Name",
    y = "Frequency Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




data_filtered <- data %>%
  group_by(StopName) %>%
  filter(n() >= 5000)

ggplot(data_filtered, aes(x = reorder(StopName, -table(StopName)[StopName]))) +
  geom_bar(fill = "#112446") +
  labs(
    title = "frequency count of stops (only stops that appear >5000 times)",
    x = "Stop Name",
    y = "Frequency Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Doctoral_x0020_advanced_x0020_to_x0020_candidacy"] <- "Doctoral advanced to candidacy"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Fresh_x0020_1_x0020_Standing_x0020_0_x0020_to_x0020_7_x0020_Hrs"] <- "Fresh 1 Standing 0 to 7 Hrs"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Fresh_x0020_2_x0020_Standing_x0020_8_x0020_to_x0020_23_x0020_Hrs"] <- "Fresh 2 Standing 8cto 23 Hrs"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Junior_x0020_1_x0020_Standing_x0020_56_x0020_to_x0020_71_x0020_Hrs"] <- "Junior 1 Standing 56 to 72 Hrs"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Junior_x0020_2_x0020_Standing_x0020_72_x0020_to_x0020_87_x0020_Hrs"] <- "Junior 2 Standing 72 to 87 Hrs"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Master_x0020_Done_x0020_and_x0020_begun_x0020_Doctor"] <- "Master Done and begun Doctor"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Masters_x0020_24_x0020_credits_x0020_and_x0020_over"] <- "Masters 24 credits and over"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Masters_x0020_less_x0020_than_x0020_24_x0020_credits"] <- "Masters less than 24 credits"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Senior_x0020_1_x0020_Standing_x0020_88_x0020_to_x0020_103_x0020_Hr"] <- "Junior 2 Standing 72 to 87 Hrs"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Senior_x0020_2_x0020_Standing_x0020_104_x002B__x0020_Hrs"] <- "Junior 2 Standing 72 to 87 Hrs"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Soph_x0020_1_x0020_Standing_x0020_24_x0020_to_x0020_39_x0020_Hrs"] <- "Junior 2 Standing 72 to 87 Hrs"
data$STUDENT_CLASS_DESC_BOAP[data$STUDENT_CLASS_DESC_BOAP == "Soph_x0020_2_x0020_Standing_x0020_40_x0020_to_x0020_55_x0020_Hrs"] <- "Junior 2 Standing 72 to 87 Hrs"



ggplot(data, aes(x = reorder(STUDENT_CLASS_DESC_BOAP, -table(STUDENT_CLASS_DESC_BOAP)[STUDENT_CLASS_DESC_BOAP]))) +
  geom_bar(fill = "#112446") +
  labs(
    title = "Frequency count of class descriptions",
    x = "Class description",
    y = "Frequency count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



data_filtered <- data %>%
  group_by(MAJOR_DESC) %>%
  filter(n() >= 15000)

ggplot(data_filtered, aes(x = reorder(MAJOR_DESC, -table(MAJOR_DESC)[MAJOR_DESC]))) +
  geom_bar(fill = "#112446") +
  labs(
    title = "Frequency count of majors",
    x = "Major",
    y = "Frequency count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

mean(data$CURRENT_AGE)
median(data$CURRENT_AGE)


library(rcompanion)
tbl <- table(data$MAJOR_DESC, data$STUDENT_CLASS_DESC_BOAP)
cramerV(tbl)

tbl <- table(data$GENDER_IDENTITY_DESC, data$StopName)
cramerV(tbl)

tbl <- table(data$MAJOR_DESC, data$StopName)
cramerV(tbl)

tbl <- table(data$STUDENT_CLASS_DESC_BOAP, data$StopName)
cramerV(tbl)

tbl <- table(data$Month, data$StopName)
cramerV(tbl)

tbl <- table(data$CURRENT_AGE, data$StopName)
cramerV(tbl)

chisq.test(table(data$MAJOR_DESC, data$STUDENT_CLASS_DESC_BOAP))
chisq.test(table(data$GENDER_IDENTITY_DESC, data$StopName))
chisq.test(table(data$MAJOR_DESC, data$StopName))
