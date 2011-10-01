package cn.orz.pascal.scala.mechanize

import com.gargoylesoftware.htmlunit.html.{HtmlElement => HtmlUnitElement}
import com.gargoylesoftware.htmlunit.html.{HtmlPage => HtmlUnitPage}
import com.gargoylesoftware.htmlunit.html.{HtmlForm => HtmlUnitForm}
import com.gargoylesoftware.htmlunit.html.{HtmlInput => HtmlUnitInput}
import com.gargoylesoftware.htmlunit.html.{HtmlAnchor => HtmlUnitLink}
import com.gargoylesoftware.htmlunit.html.{HtmlImage => HtmlUnitImage}
import com.gargoylesoftware.htmlunit.html.{DomNode => DomUnitNode}
import com.gargoylesoftware.htmlunit.{BinaryPage => HtmlUnitBinaryPage}
import com.gargoylesoftware.htmlunit.{Page => IPage}
import java.net.URL
import java.io.{File, BufferedInputStream, BufferedOutputStream, FileOutputStream}
import scala.collection.JavaConversions._
import scala.xml.Node
import scala.util.control.Exception._

abstract class HttpMethod
case object Get extends HttpMethod
case object Post extends HttpMethod
case object Put extends HttpMethod
case object Delete extends HttpMethod
case object Head extends HttpMethod

abstract class FieldAttribute() { def value:String }
case class Id(val value:String) extends FieldAttribute
case class Name(val value:String) extends FieldAttribute
case class Class(val value:String) extends FieldAttribute
case class Type(val value:String) extends FieldAttribute
case class XPath(val value:String) extends FieldAttribute

trait HtmlBase {
    type DomNode = {
        def getElementById2(id:String):{ def asXml():String }
        def asXml():String
        def dom():DomUnitNode
    }
    def source:DomNode

    implicit def htmlpage2domnode(x:HtmlUnitPage):DomNode = new {
        def asXml() = x.asXml()
        def dom() = x
        def getElementById2(id:String):{ def asXml():String } = {
           x.getElementById(id).asInstanceOf[HtmlUnitElement]
        }
    }

    implicit def htmlelement2domnode(x:HtmlUnitElement):DomNode = new {
        def asXml() = x.asXml
        def dom() = x
        def getElementById2(id:String):HtmlUnitElement = {
           x.getElementById[HtmlUnitElement](id)
        }
    }

    def asXml:Node = { toNode(source.asXml) }

    def get(attr:FieldAttribute):Node = {
        val element = (attr match {
            case Id(value) => source.getElementById2(value)
            case Name(value) => findByXpath(".//*[@name='" + value + "']", source.dom).head
            case Class(value) => findByXpath(".//*[@class='" + value + "']", source.dom).head
            case XPath(xpath) => findByXpath(xpath, source.dom).head
        })
        (toNode(element.asXml)\"body")(0).child(0)
    }

    protected def toNode(src:String) = {
        import java.io.StringReader
        import scala.xml.parsing.NoBindingFactoryAdapter
        import nu.validator.htmlparser.sax.HtmlParser
        import nu.validator.htmlparser.common.XmlViolationPolicy
        import org.xml.sax.InputSource

        val hp = new HtmlParser
        hp.setNamePolicy(XmlViolationPolicy.ALLOW)

        val saxer = new NoBindingFactoryAdapter
        hp.setContentHandler(saxer)
        hp.parse(new InputSource(new StringReader(src)))
        saxer.rootElem
    }

    protected def findByXpath(xpathValue:String, node:org.w3c.dom.Node):List[HtmlUnitElement] = {
        import javax.xml.xpath._
        import org.w3c.dom._

        val xpathParser = XPathFactory.newInstance().newXPath().compile(xpathValue)
        val nodelist = xpathParser.evaluate(node, XPathConstants.NODESET).asInstanceOf[NodeList]
        (0 to nodelist.getLength).map( i => nodelist.item(i).asInstanceOf[HtmlUnitElement]).toList
    }
}

class HtmlPage(val page:HtmlUnitPage) extends HtmlBase with HtmlPageType {
    def source = page

    def title:String =  page.getTitleText
    def url:URL = page.getUrl
    def forms:List[HtmlForm] = {
        page.getForms.map(new HtmlForm(_)).toList
    }
    def links:List[HtmlLink] = {
        page.getAnchors.map(new HtmlLink(_)).toList
    }
    def images:List[HtmlImage] = {
        page.getElementsByTagName("img").map(e => new HtmlImage(e.asInstanceOf[HtmlUnitImage])).toList
    }
}

trait BinaryContents {
    def saveAs(file:File):Option[File]
    def save(dir:File)(implicit fileName:String):Option[File] =
        saveAs(new File(dir, fileName))
}


class BinaryPage(val page:HtmlUnitBinaryPage) extends BinaryPageType with BinaryContents {
    implicit val filename:String = page.getUrl.toString.split("/").last

    def saveAs(file:File):Option[File] = {
        allCatch.opt{
            (new BufferedInputStream(page.getInputStream), new BufferedOutputStream(new FileOutputStream(file)))
        } flatMap { e =>
            val input = e._1
            val output = e._2

            allCatch.andFinally {
                if(input != null) input.close()
                if(output != null) output.close()
            } opt {
                val buff:Array[Byte] = Array.fill(1024)(0)
                var size = input.read(buff)
                while(size != -1) {
                    output.write(buff, 0, size);
                    size = input.read(buff)
                }
                file
            }
        }
    }
}


class HtmlForm(val form:HtmlUnitForm) extends HtmlBase {
    def source = form

    def name:String = form.getNameAttribute
    def method:HttpMethod = {
        form.getMethodAttribute.toUpperCase match {
          case "GET" => Get
          case "POST" => Post
          case _ => Get
        }
    }
    def submit():HtmlPage = new HtmlPage(form.click.asInstanceOf[HtmlUnitPage])

    def field(id:String):HtmlField = {
        new HtmlField(form.getElementById(id).asInstanceOf[HtmlUnitInput])
    }
    def fields_with(attr:FieldAttribute):List[HtmlField] = {
        (attr match {
          case Name(value) => findByXpath(".//input[@name='" + value + "']", form)
          case Class(value) => findByXpath(".//input[@class='" + value + "']", form)
          case Type(value) => findByXpath(".//input[@type='" + value + "']", form)
          case XPath(xpath) => findByXpath(xpath, form)
        }).map( node => new HtmlField(node.asInstanceOf[HtmlUnitInput]))
    }

}

class HtmlField(val field:HtmlUnitInput) extends HtmlBase {
    def source = field

    def name():String = field.getNameAttribute
    def value:String = field.getValueAttribute
    def value_=(value:String) = new HtmlPage(field.setValueAttribute(value).asInstanceOf[HtmlUnitPage])
}

object Page {
    def apply[A <: IPage](page:A):PageType = page.getClass match {
        case x if x == classOf[HtmlUnitPage] => new HtmlPage(page.asInstanceOf[HtmlUnitPage])
        case x if x == classOf[HtmlUnitBinaryPage] => new BinaryPage(page.asInstanceOf[HtmlUnitBinaryPage])
    }
}

trait PageType
trait HtmlPageType extends PageType
trait BinaryPageType extends PageType

class HtmlLink(val link:HtmlUnitLink) extends HtmlBase {
    def source = link
    def href:String = link.getHrefAttribute()
    //def click:HtmlPage = new HtmlPage(link.click.asInstanceOf[HtmlUnitPage])
    def click(pf:PartialFunction[PageType, Boolean] = { case _ => true }):Option[PageType] = {
        val allDeny:PartialFunction[PageType, Boolean] = { case _ => false }
        val f = pf orElse allDeny
        val page = Page(link.click)
        if(f(page)) {
            Some(page)
        } else {
            None
        }
    }
    def text:String = link.getTextContent
}

class HtmlImage(val image:HtmlUnitImage) extends HtmlBase with BinaryContents{
    implicit val filename:String = src.split("/").last

    def source = image
    def actualWidth:Int = image.getWidth
    def actualHeight:Int = image.getHeight
    def height:String = image.getHeightAttribute
    def width:String = image.getWidthAttribute
    def name:String = image.getNameAttribute
    def src:String = image.getSrcAttribute
    def saveAs(file:File):Option[File] = {
        allCatch.opt{
            image.saveAs(file)
            file
        }
    }
}


// vim: set ts=4 sw=4 et:
