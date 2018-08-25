# Saved for Reddit

[![Build Status](https://travis-ci.com/JBlackN/saved-for-reddit.svg?token=UyUqK5Y7LhTWBHGCPQhz&branch=master)](https://travis-ci.com/JBlackN/saved-for-reddit)

[Reddit](https://www.reddit.com) allows users to save posts and comments. However, after thousand or so items are saved, the oldest ones start to become unreachable, which is undesirable for long term archivation. This web app enables users logged in through [Reddit](https://www.reddit.com) to load their saved items to local database without any restrictions and to browse them with basic filtering.

## Features

- [x] Login through OAuth2.
- [x] Load all available saved items from Reddit API (JSON).
- [x] Update saved items database (insert new ones).
- [x] Web page presentation of saved items.
- [x] Saved items filtering (at least by subreddit =~ category).
- [x] Export saved items (JSON).

## Used dependencies

- [Aeson](http://hackage.haskell.org/package/aeson)
- [Base64-ByteString](https://hackage.haskell.org/package/base64-bytestring)
- [BlazeHtml](https://hackage.haskell.org/package/blaze-html) + [BlazeMarkup](https://hackage.haskell.org/package/blaze-markup)
- [Clay](https://hackage.haskell.org/package/clay)
- [HTTP-Conduit](https://hackage.haskell.org/package/http-conduit)
- [Persistent](http://hackage.haskell.org/package/persistent) + [SQLite](https://www.sqlite.org/index.html) backend
- [Scotty](https://github.com/scotty-web/scotty) + [ScottyCookie](https://hackage.haskell.org/package/scotty-cookie)
- [UUID](https://hackage.haskell.org/package/uuid)
- [Yaml](https://hackage.haskell.org/package/yaml)

More in [package.yaml](package.yaml).

## Installation and usage

The application needs `client ID` and `client secret`, which can be both obtained by registering at [https://ssl.reddit.com/prefs/apps/](https://ssl.reddit.com/prefs/apps/) and filled in the [config.yml](config.yml.template) file (**see instructions below**).

```bash
git clone https://github.com/JBlackN/saved-for-reddit.git  # download
cd saved-for-reddit

cp config.yml.template config.yml                          # configuration (initialization)
vi config.yml                                              # configuration (editing)
stack build                                                # build

stack test                                                 # test
stack haddock                                              # documentation (build)
xdg-open .stack-work/install/*/*/*/doc/index.html          # documentation (browse)

stack exec saved-for-reddit-exe                            # run
```
