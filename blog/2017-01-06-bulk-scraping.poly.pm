#lang pollen

@(define-meta title "Bulk Scraping")
@(define-meta tags (python scraping))

One of my friends who is a non-CS concentrator would like to scrape emails of all faculties listed in @link["http://engineering.vanderbilt.edu/eecs/faculty-staff/index.php"]{this website}. Unfortunately, the emails are not on the page itself, but are on subpages. It would take forever to scrape the data by hand, so I helped. To do this, I need to send multiple requests to scrape each subpage. Naively, we would send a request, wait for a response, then repeat until we go over all list of faculties. This however would take a lot of time. We can do better by sending requests asynchronously. This is feasible because there is no dependency in the data.

@/see-more[]

But first, let's consider a naive implementation that doesn't exploit asynchronicity:

@highlight['python3]|{
  from lxml import html
  import requests
  page = requests.get("http://engineering.vanderbilt.edu/eecs/faculty-staff/index.php")
  tree = html.fromstring(page.content)
  for e in tree.xpath("//h4/a/@href[starts-with(., '/bio/')]"):
      page = requests.get("http://engineering.vanderbilt.edu" + e)
      tree2 = html.fromstring(page.content)
      for e2 in tree2.xpath("//a/@href[starts-with(., 'mailto:')]"):
          print(e2[7:])
}|

This takes about 2 minutes to run.

Now, we will use a drop-in replacement of @code{requests}: @code{@link["https://github.com/kennethreitz/grequests"]{grequests}}, which supports asynchronicity. The library is not built-in and needed to be installed separately@numbered-note{via @code{pip}, so it's actually very easy}, but it's the easiest way to go without altering the code much.

@highlight['python3]|{
  from lxml import html
  import requests
  import grequests
  page = requests.get("http://engineering.vanderbilt.edu/eecs/faculty-staff/index.php")
  tree = html.fromstring(page.content)
  urls = ["http://engineering.vanderbilt.edu" + e
          for e in tree.xpath("//h4/a/@href[starts-with(., '/bio/')]")]
  for page in grequests.map(grequests.get(url) for url in urls):
      tree2 = html.fromstring(page.content)
      for e2 in tree2.xpath("//@href[starts-with(., 'mailto:')]"):
          print(e2[7:])
}|

takes 20 seconds!

@link["http://jswrenn.com"]{Jack} writes a one liner@numbered-note{It's not quite one liner here because I want to make it readable.} to solve this problem using shell script and a tool named @code{@link["http://www.videlibri.de/xidel.html"]{xidel}}:

@highlight['bash]|{
  xidel -s http://engineering.vanderbilt.edu/eecs/faculty-staff/index.php \
        --extract "//h4/a/@href[starts-with(.,'/bio/')]" \
        | xargs -n1 -P10 -I{} xidel -s http://engineering.vanderbilt.edu/{} \
        --extract "substring(//@href[starts-with(.,'mailto:')], 8)"
}|

also takes 20 seconds. The disadvantage of this approach is that it could produce output like:

@highlight['text]|{
  david.kerns@vanderbilt.edu
  daniel.loveless@vanderbilt.edugerry_lucovsky@ncsu.edu
}|

I guess we can instead append to a file to tidy this up, but then it becomes more verbose. Plus, it @link["http://stackoverflow.com/questions/2443786/is-it-safe-to-pipe-the-output-of-several-parallel-processes-to-one-file-using"]{doesn't always} work.

Lastly, one can also use a sledgehammer to @/strike{crack a nut} scrape pages: @link["https://scrapy.org"]{Scrapy}! There are four steps:

@numberlist[
  @item{Generate a project: @code{scrapy startproject basic-scraper}}
  @item{Create a structure to hold data: put @code{email = scrapy.Field()} in the class in @code{items.py}}
  @item{Write the scraper: @filebox-highlight["spiders/basic_spider.py" 'python]|{
    import scrapy

    class BasicSpider(scrapy.Spider):
        name = 'basic'
        start_urls = ['http://engineering.vanderbilt.edu/eecs/faculty-staff/index.php']

        def parse(self, response):
            for data in response.xpath("//h4/a/@href[starts-with(., '/bio/')]"):
                next_page = response.urljoin(data.extract())
                yield scrapy.Request(next_page, callback=self.subpage)

        def subpage(self, response):
            data = response.xpath("//@href[starts-with(., 'mailto:')]").extract_first()
            if data:
                yield {
                    'email': data[7:]
                }
  }|}
  @item{Run it: @code{scrapy crawl basic -t csv -o items.csv; sed -i '1d' items.csv}}
]

And yes, this takes 20 seconds (it'd better not be worse than the @code{grequests} method!).
