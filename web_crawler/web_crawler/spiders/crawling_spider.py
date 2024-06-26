from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor

class CrawlingSpider (CrawlSpider):
    name = "my_crawler"
    allowed_domains = ["toscrape.com"]
    start_urls = ["http://books.toscrape.com/"]
    rules = (
        Rule(LinkExtractor(allow="catalogue/category")),
        Rule(LinkExtractor(allow="catalogue", deny="category"), callback="parse_item")
    )

    def parse_item(self, response):
        yield {
            "price": response.css(".price_color::text").get()
        }