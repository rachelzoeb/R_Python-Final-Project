# -*- coding: utf-8 -*-
"""
Created on Mon Apr 25 15:17:23 2016

@author: rachelblumhagen
"""

## Dataread script for reading in data from Twitter
# Goal is to pull data twice from Twitter

# Reading in data pulled by Caroline
# Following code example provided by http://adilmoujahid.com/posts/2014/07/twitter-analytics/

# The data that we stored twitter_data.txt is in JSON format. JSON stands for JavaScript Object Notation. 
# import json and pandas modules
import json
import pandas as pd
import matplotlib.pyplot as plt

# Next read in data that we will call tweets
tweets_data_path = '/Users/rachelblumhagen/Google Drive/SCHOOL_work/R_Python/Final_Project/data/fetched_tweets.txt'

tweets_data = []
# Added code to pull location information out of tweet['user']
location = []
country = []
geo_enabled = []
tweets_file = open(tweets_data_path, "r")
for line in tweets_file:
    try:
        tweet = json.loads(line)
        tweets_data.append(tweet)
        if ('user' in tweet) and ('location' in tweet['user']):
            location.append(tweet['user']['location'])
        else: 
            location.append('No user information')
        
        if ('place' in tweet) and (tweet['place'] != None) and ('country' in tweet['place']) and (tweet['place']['country'] != None):
            country.append(tweet['place']['country'])
        else:
            country.append('No country information')
        
        if ('user' in tweet) and (tweet['user'] != None) and ('geo_enabled' in tweet['user']) and (tweet['user']['geo_enabled'] != None):
            geo_enabled.append(tweet['user']['geo_enabled'])
        else:
            geo_enabled.append('No geo data')

    except:
        continue

# Print the number of tweets    
print len(tweets_data)
#199,264 tweets

# Put into pandas Dataframe structure
tweets = pd.DataFrame()

# Add columns to tweets dataframe (text, language and country)
variables = ['text', 'lang', 'place', 'created_at', 'retweeted']
for var in variables:
    tweets[var] = [tweet.get(var, '') for tweet in tweets_data]

# Add location variable to tweets dataframe
tweets['location'] = location
tweets['country'] = country
tweets['geo_enabled'] = geo_enabled

# Export pandas dataframe to R object to be read into R
import rpy2.robjects as ro
import pandas.rpy.common as com
r_dataframe = com.convert_to_r_dataframe(tweets)
ro.globalenv['tweets'] = r_dataframe
ro.r('save(tweets, file = "/Users/rachelblumhagen/Google Drive/SCHOOL_work/R_Python/Final_Project/data/fetched_tweets_dataframe.RData")')

########################### EXPLORATORY ####################################

## Graph tweets by language
#tweets_by_lang = tweets['lang'].value_counts()
#
#fig, ax = plt.subplots()
#ax.tick_params(axis='x', labelsize=15)
#ax.tick_params(axis='y', labelsize=10)
#ax.set_xlabel('Languages', fontsize=15)
#ax.set_ylabel('Number of tweets' , fontsize=15)
#ax.set_title('Top 5 languages', fontsize=15, fontweight='bold')
#tweets_by_lang[:5].plot(ax=ax, kind='bar', color='red')
#
## Tweets by country
#tweets_by_country = tweets['country'].value_counts()
#
#fig, ax = plt.subplots()
#ax.tick_params(axis='x', labelsize=15)
#ax.tick_params(axis='y', labelsize=10)
#ax.set_xlabel('Countries', fontsize=15)
#ax.set_ylabel('Number of tweets' , fontsize=15)
#ax.set_title('Top 5 countries', fontsize=15, fontweight='bold')
#tweets_by_country[:5].plot(ax=ax, kind='bar', color='blue')

# ideas 
# can mine for relevant tweets 'virus' 
