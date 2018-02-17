package se.chimps.embeddable.web.framework.bridge

import java.util.concurrent.TimeUnit
import java.util.function.Consumer

import io.undertow.util.HttpString
import se.chimps.cameltow.framework._
import se.chimps.embeddable.web.framework.api.{Bytes, HttpRequest, HttpResponse}
import se.chimps.embeddable.web.framework.api.{Form => FormData}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait CameltowBridge {

	def bridge:(Request)=>Future[Response] = (req) => {
		val method = req.method
		val path = req.exchange.getRequestPath
		val query = req.exchange.getQueryString.split("&").filter(_.nonEmpty).flatMap(q => {
			val Array(key, value) = q.split("=")
			Map(key -> value)
		}).toMap

		var headers = Map[String, String]()
		req.exchange.getRequestHeaders.getHeaderNames.forEach(new Consumer[HttpString] {
			override def accept(t:HttpString):Unit = headers = headers ++ Map(t.toString -> req.exchange.getRequestHeaders.get(t).toString)
		})

		val body = req.body match {
			case Encoded(bytes) => Some(Bytes(Await.result(bytes, Duration(1L, TimeUnit.SECONDS))))
			case Form(data) => Some(FormData(data.filter(kv => kv._2.count(_.isInstanceOf[FileItem]) > 0).mapValues(_.mkString(", "))))
			case Stream(q) => {
				var body = Array[Byte]()
				var data = q.dequeue()

				while (data.nonEmpty) {
					body = body ++ data
					data = q.dequeue()
				}

				Some(Bytes(body))
			}
			case _ => None
		}

		val resp = ewf(HttpRequest(method, path, query, headers, body))

		resp.map(r => {
			val resBody = if (r.body.nonEmpty) {
				Some(new ByteResponseBody {
					override def contentType:Option[String] = None

					override def apply():Array[Byte] = r.body
				})
			} else {
				None
			}

			Response(r.code, r.headers, Map(), Seq(), resBody)
		})
	}

	def ewf:(HttpRequest) => Future[HttpResponse]

}
