package com.rasterfoundry.backsplash.serverless

import cats.data._
import io.github.howardjohn.lambda.http4s.Http4sLambdaHandler
import com.rasterfoundry.backsplash.BacksplashServer

object Handler {
  class Entrypoint extends Http4sLambdaHandler(BacksplashServer.httpApp)
}
