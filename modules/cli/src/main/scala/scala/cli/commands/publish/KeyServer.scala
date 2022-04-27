package scala.cli.commands.publish

import sttp.client3._
import sttp.model.Uri

object KeyServer {

  def add(
    pubKey: String,
    addEndpoint: Uri,
    backend: SttpBackend[Identity, Any]
  ): Either[String, String] = {

    val resp = basicRequest
      .body(Map("keytext" -> pubKey))
      .response(asString)
      .post(addEndpoint)
      .send(backend)

    if (resp.isSuccess)
      Right(resp.body.merge)
    else
      Left(resp.body.merge)
  }

  def check(
    keyId: String,
    lookupEndpoint: Uri,
    backend: SttpBackend[Identity, Any]
  ): Either[String, Either[String, String]] = {
    val resp = basicRequest
      .get(lookupEndpoint.addParam("op", "get").addParam("search", keyId))
      .response(asString)
      .send(backend)
    if (resp.isSuccess)
      Right(Right(resp.body.merge))
    else if (resp.isClientError)
      Right(Left(resp.body.merge))
    else
      Left(resp.body.merge)
  }

}
