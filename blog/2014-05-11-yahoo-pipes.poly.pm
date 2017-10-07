#lang pollen

@(define-meta title "Yahoo! Pipes")
@(define-meta tags (yahoo-pipes rss scraping))

@migration-notice[]

I don't know if @link["https://en.wikipedia.org/wiki/RSS"]{RSS} or @link["https://en.wikipedia.org/wiki/Atom_(standard)"]{Atom} are still popular or not. I personally use them a lot. Here are some examples of the feeds that I subscribed.

@see-more


@itemlist[
  @item{@link["http://www.xkcd.com/atom.xml"]: for reading @link["https://www.xkcd.com"]{xkcd} comic.}
  @item{@link["https://en.wikipedia.org/w/index.php?title=Special:RecentChanges&feed=rss&namespace=828"]: to see if there are new updated @link["https://en.wikipedia.org/wiki/Lua_(programming_language)"]{Lua} modules in English Wikipedia so that I can update corresponding modules in Thai Wikipedia.}
]

xkcd feed works very well. However, frequently I found that some entries in the Wikipedia feed (Module namespace) are not really code updates. Rather, they are documentation updates or experiments in sandboxes, indicated by their titles ending with @code{/doc} or @code{/sandbox}. Since I subscribed the feed for the entire Module namespace, these undesirable entries showed up unavoidably.

Yesterday, I thought that it would be good if there is a program that will filter the feed I subscribed. There was no program in Mac that did what I want. However, I finally found @link["http://pipes.yahoo.com/pipes/"]{Yahoo! Pipes}, a web service that solves this problem!

@link["https://www.youtube.com/watch?v=J3tS_DkmbVA"]{Here} is a tutorial for Yahoo! Pipes.

Basically, what I did was to retrieve the feed, and then use @link["https://en.wikipedia.org/wiki/Regular_expression"]{regex} filter to block all entries containing @code{/doc} and @code{/sandbox} at the end of their title. Finally, I exported the result as another RSS feed from Yahoo Pipes! and use this feed in my feed reader instead of the old one.

Oh, and, Yahoo! Pipes is the only reason why I am using Yahoo! Hopefully the company will not shut this service down in the same way Google usually does, because, if so, ...

Bye-bye Yahoo!

@notice{Update on December 26, 2016: Yahoo! shut down Yahoo! Pipes on June 4, 2015. What a shame!}
