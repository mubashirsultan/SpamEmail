#Load packages. Openintro packakge has email dataset.
library(ggplot2)
library(dplyr)
library(openintro)

Email <- email

str(Email)
glimpse(Email)
#This data contains 21 variables and 3921 observations.

#Lets convert spam from a numerical variable to a factor variabe.
Email$spam <- as.factor(Email$spam)
levels(Email$spam)

table(Email$spam)
#Out of 3921 observations, only 367 are spam emails. 0 is not-spam, 1 is spam.

#lets rename labels of spam. 0 as not-spam and 1 as spam
levels(Email$spam) <- c("Not-spam", "Spam")

#lets plot it
ggplot(Email, aes(x=spam, fill = spam))+
  geom_bar()

#Lets check number of characters in dataset
Email %>%
  group_by(spam)%>%
  summarise(mean(num_char), median(num_char), IQR(num_char), sd(num_char))

#Spam emails seem to have less number of characters in the email.

Email %>%
  ggplot(aes(x=spam, y=num_char)) +
  geom_boxplot()

email%>%
  ggplot(aes(x=num_char))+
  geom_density() +
  facet_wrap(~spam)

#The graphs is heavily skewed to the right. Lets use log transformation

Email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()

email%>%
  mutate(log_num_char = log(num_char))%>%
  ggplot(aes(x=log_num_char))+
  geom_histogram() +
  facet_wrap(~spam)

#The median length of not-spam emails is greater than that of spam emails.

#Let's look at exclamation marks.

eEmail%>%
  group_by(spam)%>%
  summarize(median(exclaim_mess), IQR(exclaim_mess))


# Lets create plot for spam and exclaim_mess
Email%>%
  mutate(log_exclaim_mess = log(exclaim_mess+ 0.01))%>%
  ggplot(aes(x=log_exclaim_mess))+
  geom_histogram() +
  facet_wrap(~spam)

Email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + 0.01)) %>%
  ggplot(aes(x = 1, y = log_exclaim_mess)) +
  geom_boxplot() +
  facet_wrap(~ spam)

Email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim_mess, fill = spam)) +
  geom_density(alpha = 0.3)

#The typical number of exclamations in the not-spam group appears to be slightly higher than in the spam group.
#Even after a transformation, the distribution of exclaim_mess in both classes of email is right-skewed.


Email %>%
  mutate(zero = exclaim_mess == 0) %>%
  ggplot(aes(x = zero)) +
  geom_bar() +
  facet_wrap(~spam)

#Looks like a lot of no-spam emails contain more exclaimation marks than spam emails.

#lets take a look at images in the emails
Email %>%
  mutate(has_image = image>0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = "fill")

#An email without an image is more likely to be not-spam than spam.

#lets take a look at dollar
email %>%
  filter(dollar> 0) %>%
  group_by(spam) %>%
  summarize(median(dollar))

email %>%
  filter(dollar > 10) %>%
  ggplot(aes(x = spam)) +
  geom_bar()

#Looks like emails that are not spam contain more dollar signs than spam emails.

