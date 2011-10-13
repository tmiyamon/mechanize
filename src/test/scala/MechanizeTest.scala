package cn.orz.pascal.scala.mechanize


import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import java.net.URL

class MechanaizeTest extends WordSpec with ShouldMatchers {
    "GET 'http://www.google.co.jp'" when {
        val agent:Mechanize = new Mechanize()
        val page:HtmlPage = agent.get("http://www.google.co.jp")

        page.title should be("Google")
        page.url should be(new URL("http://www.google.co.jp/"))
        (page.get(Id("hplogo")) \ "@title").text should be("Google")
        (page.get(Class("jhp")) \ "@class").text should be("jhp")
        (page.get(Name("f")) \ "@action").text should be("/search")
        (page.get(XPath(".//center/form")) \ "@action").text should be("/search")

        val form:HtmlForm = page.forms(0)
        form.name should be("f")
        form.method should be(Get)
        form.fields_with(Name("q"))(0).name should be("q")
        form.fields_with(Class("lst"))(0).name should be("q")
        form.fields_with(Type("hidden"))(0).name should be("hl")
        form.fields_with(XPath(".//input[@type='hidden' and @value='hp']"))(0).name should be("source")

        val input:HtmlField = form.fields_with(Name("q"))(0)
        input.value should be("")
        input.value = "Scala"
        input.value should be("Scala")

        val result_page:HtmlPage = form.submit()
        result_page.title should be("Google")
        result_page.forms(0).field("lst-ib").name should be("q")
        result_page.asXml.isInstanceOf[scala.xml.Node] should be(true)

    }

    "GET 'http://www.amazon.co.jp/'" when {
        val agent:Mechanize = new Mechanize()
        val page:HtmlPage = agent.get("http://www.amazon.co.jp/")

        page.title should be("Amazon.co.jp： 通販 - ファッション、家電から食品まで【無料配送】")
        page.url should be(new URL("http://www.amazon.co.jp/"))

        val form = page.forms(0)
        form.name should be("site-search")
        form.method should be(Get)
        form.fields_with(Name("field-keywords"))(0).name should be("field-keywords")
        form.fields_with(Class("searchSelect"))(0).name should be("field-keywords")
        form.fields_with(Type("text"))(0).name should be("field-keywords")
        form.fields_with(XPath(".//input[@type='text' and @title='検索']"))(0).name should be("field-keywords")

    }

    "GET 'http://www.ebookjapan.jp/ebj/search.asp?q=%96%B2%97%88%92%B9%82%CB%82%DE&ebj_desc=on'" when {
        val agent:Mechanize = new Mechanize()
        val page:HtmlPage = agent.get("http://www.ebookjapan.jp/ebj/search.asp?q=%96%B2%97%88%92%B9%82%CB%82%DE&ebj_desc=on")
        page.asXml.isInstanceOf[scala.xml.Node] should be(true)
        page.get(Id("main_line")).isInstanceOf[scala.xml.Node] should be(true)
      }

    "GET http://www.amazon.co.jp" when {
        val agent: Mechanize = new Mechanize()
        val page: HtmlPage = agent.get("http://www.amazon.co.jp")

        "image tags in its response" should {
            "not be empty" in {
                page.images.length should not be(0)
            }
        }
        "help link" should {
            "be able to jump to help page with clicking" in {
                page.links.collect{ case a if a.text == "ヘルプ" => a }.head.click{case _ => true} match {
                    case Some(a:HtmlPage) => {
                        a.title should be("Amazon.co.jp ヘルプ")
                        a.url.toString should startWith("http://www.amazon.co.jp/gp/help/customer/display.htm")

                    }
                    case _ => fail()
                }
            }
        }
    }
    "GET http://images.google.co.jp/" when {
        val agent: Mechanize = new Mechanize()
        val page: HtmlPage = agent.get("http://images.google.co.jp/")

        "image search form" should {
            "be found with its id" in {
                page.formWith(Id("qbf")) match {
                    case Some(_) => 
                    case _ => fail()
                }
            }
        }
    }
}

// vim: set ts=4 sw=4 et:
