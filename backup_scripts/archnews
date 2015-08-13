#!/usr/bin/env python3

# Depends on python-feedparser

import argparse
import datetime
import feedparser
import time


FEED_URL = 'https://www.archlinux.org/feeds/news/'



# Command-line argument parser.
parser = argparse.ArgumentParser(description='List Arch Linux news titles.')
parser.add_argument(
  '-d', '--days', metavar='n', type=int, default=30,
  help='Show titles from the last n days. Default: %(default)s.'
)
parser.add_argument(
  '-s', '--since', metavar='yyyy-mm-dd',
  help='Show titles between now and the given date.'
)



# Convert time struct objects to datetime objects.
def ts_to_dt(ts):
  return datetime.datetime.fromtimestamp(time.mktime(ts))



# Main function to print out titles.
def main(args=None):
  args = parser.parse_args(args)

  # Load the feed.
  feed = feedparser.parse(FEED_URL)

  # Get the current time.
  now = datetime.datetime.now()

  if args.since:
    ts = time.strptime(args.since, '%Y-%m-%d')
    dt = ts_to_dt(ts)
    cutoff = now - dt
  else:
    cutoff = datetime.timedelta(days=args.days)


  # Find all entries not older than the cutoff.
  for entry in feed.entries:
    ts = entry.published_parsed
    dt = ts_to_dt(ts)
    if (now - dt) <= cutoff:
      print(entry.title)



# Run the main function when invoked as a script.
# This will catch keyboard interrupts (ctrl+c) and broken pipe errors.
if __name__ == '__main__':
  try:
    main()
  except (KeyboardInterrupt, BrokenPipeError):
    pass
