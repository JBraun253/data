library(tidyverse)
library(ggplot2)
library(usmap)

clickstream_data <- read.csv(file="data/interim/clickstream/clickstream_cleaned.csv", header = TRUE)

# DATA 1
partial_data <- clickstream_data %>%
  select("Session_ID", "REQUEST_DAY_OF_WEEK", "Request_Sequence", "Request_Date", "Request_Date_Time", "Session_First_Request_Date", "Session_First_Request_Date_Time") %>%
  unite(col = "Request_Timestamp",c("Request_Date", "Request_Date_Time"), sep = " ", remove = TRUE) %>%
  unite(col = "Session_First_Request_Timestamp", c("Session_First_Request_Date", "Session_First_Request_Date_Time"), sep = " ", remove = TRUE) %>%
  mutate(Request_Timestamp = as.POSIXct(Request_Timestamp, format="%Y-%m-%d %H\\:%M\\:%S")) %>%
  mutate(Session_First_Request_Timestamp = as.POSIXct(Session_First_Request_Timestamp, format="%Y-%m-%d %H\\:%M\\:%S")) %>%
  group_by(Session_ID) %>%
  summarise(Pages_Visited = n(), Session_First_Request = max(Session_First_Request_Timestamp), Session_Last_Request = max(Request_Timestamp), Day_Of_Week = first(REQUEST_DAY_OF_WEEK)) %>%
  mutate(Session_Duration = as.numeric(difftime(Session_Last_Request, Session_First_Request, units = "secs")))

# DATA 2
partial_data2 <- partial_data %>%
  group_by(Pages_Visited) %>%
  summarise(avg_Session_Duration = mean(Session_Duration), Session_Count = n())

partial_data2$Perc <- partial_data2$Session_Count / sum(partial_data2$Session_Count)

# DATA 3
partial_data3 <- partial_data %>%
  group_by(Day_Of_Week) %>%
  summarise(Session_Count = n(), avg_Session_Duration = mean(Session_Duration), avg_Pages_Visited = mean(Pages_Visited))

partial_data3$Day_Of_Week <- factor(partial_data3$Day_Of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

partial_data3$Perc <- partial_data3$Session_Count / sum(partial_data3$Session_Count)

# DATA 4
pd4 <- clickstream_data %>%
  select(Session_ID, Request_Date, REQUEST_DAY_OF_WEEK) %>%
  group_by(Request_Date) %>%
  summarise(Request_Count = n(), Day_Of_Week = first(REQUEST_DAY_OF_WEEK))

# DATA 5
pd5 <- clickstream_data %>%
  select(Product_ID, Session_ID) %>%
  group_by(Product_ID) %>%
  summarise(Request_Count = n())

# DATA 6
pd6 <- clickstream_data %>%
  select(Customer_ID) %>%
  group_by(Customer_ID) %>%
  summarise(Request_Count = n())

# DATA 7
pd7 <- clickstream_data %>%
  select(Customer_ID, Product_ID) %>%
  group_by(Customer_ID, Product_ID) %>%
  summarise(Request_Count = n())

# DATA 8
pd8 <- clickstream_data %>%
  select(Session_User_Agent, Customer_ID) %>%
  group_by(Session_User_Agent, Customer_ID) %>%
  summarise(Request_Count = n())

# DATA 9
pd9 <- clickstream_data %>%
  select(US_State) %>%
  group_by(US_State) %>%
  summarise(Request_Count = n(), state = first(US_State))

# DATA 10
pd10 <- clickstream_data %>%
  select(Customer_ID, US_State) %>%
  group_by(US_State) %>%
  summarise(Customer_Count = n_distinct(Customer_ID), state = first(US_State))

# GRAFIKEN

# Pages Visited x Session Count
ggplot(data = partial_data2) +
  geom_point(aes(x = Pages_Visited, y = Session_Count))

# Day of week x Session Count
ggplot(partial_data3) +
  geom_bar(aes(x = Day_Of_Week, y = Session_Count), stat = "identity")

ggplot(partial_data3) +
  geom_bar(aes(x = Day_Of_Week, y = Perc), stat = "identity")

# avg Pages Visited x avg Session Duration by Day of Week
ggplot(partial_data3) +
  geom_point(aes(x = avg_Pages_Visited, y = avg_Session_Duration, color = Day_Of_Week))

# Pages Visited x Percentage of Sessions
ggplot(data = filter(partial_data2, Perc > 0.001)) +
  geom_point(aes(x = Pages_Visited, y = Perc))

# Pages Visited x Percentage of Sessions
ggplot(data = filter(partial_data2, Perc > 0.001)) +
  geom_line(aes(x = Pages_Visited, y = Perc))

# Pages Visited x avg Session Duration
ggplot(data = filter(partial_data2, Session_Count > 2)) +
  geom_point(aes(x = Pages_Visited, y = avg_Session_Duration))

# Request Count by Date
ggplot(pd4) +
  geom_bar(aes(x = Request_Date, y = Request_Count, fill = Day_Of_Week), stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(fill = "Day of the Week", x = "Request Date", y = "Number of Requests")

# Product ID x Session Count
ggplot(filter(pd5, Request_Count < 1000)) +
  geom_point(aes(x = reorder(Product_ID, desc(Request_Count)), y = Request_Count))

# Customer ID x Session Count
ggplot(filter(pd6, Request_Count < 10000)) +
  geom_point(aes(x = reorder(Customer_ID, desc(Request_Count)), y = Request_Count))

# Customer ID x Product ID
ggplot(filter(pd7, Request_Count > 1)) +
  geom_point(aes(y = reorder(Product_ID, Request_Count), x = reorder(Customer_ID, Request_Count), size = Request_Count))

# Session User Agent x Request Count
ggplot(filter(pd8, Request_Count > 500)) +
  geom_point(aes(x = Request_Count, y = Session_User_Agent))

# Requests per US State
plot_usmap(data = pd9, values = "Request_Count", labels = TRUE) +
  scale_fill_continuous(low = "white", high = "red", name = "Requests")

# Customers per US State
plot_usmap(data = pd10, values = "Customer_Count", labels = TRUE) +
  scale_fill_continuous(low = "white", high = "red", name = "Customers")
