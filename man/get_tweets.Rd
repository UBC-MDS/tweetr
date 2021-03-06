% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tweetr.R
\name{get_tweets}
\alias{get_tweets}
\title{Get Tweets}
\usage{
get_tweets(handle, n_tweets = -1, include_replies = FALSE, verbose = TRUE)
}
\arguments{
\item{handle}{character, the username to query}

\item{n_tweets}{integer, the number of tweets to retrieve. Must be positive or -1 (by default, all)}

\item{include_replies}{logical, whether or not to include replies}

\item{verbose}{logical, whether or not to print progress during the fetch}
}
\value{
data.frame
}
\description{
Create a data.frame of a user's tweets given the username/handle.
The output data.frame will contain two columns: sent time and tweet.
}
\details{
REQUIRES: Twitter API credentials need to be stored as environment
variables as this function calls directly to the bash_profile.
}
\examples{
\dontrun{
  get_tweets('@BrunoMars', n_tweets=100)
}
}
