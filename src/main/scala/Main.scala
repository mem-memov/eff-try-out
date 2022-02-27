import cats.data.Reader
import org.atnos.eff._, syntax.all._

// https://www.rea-group.com/about-us/news-and-insights/blog/a-journey-into-extensible-effects-in-scala/
// https://atnos-org.github.io/eff/org.atnos.site.Tutorial.html
// https://github.com/atnos-org/eff

@main def hello: Unit =
  import TryOut.*
  println(getPropertyForUserId(UserId(123000)))



case class UserId(id: Int):
  def get: Int = id
case class PropertyId(id: Int):
  def get: Int = id

case class User(name: String, id: UserId, propertyId: PropertyId)
case class Property(value: String, id: PropertyId)

case class PropertyApiUrl(url: String):
  def get: String = url


object TryOut {
  type AppStack = Fx.fx2[Either[Error, _], Reader[PropertyApiUrl, _]]

  def getPropertyForUserId(id: UserId): Either[Error, Property] = {

    val program: Eff[AppStack, Property] = for {
      user <- getUser[AppStack](id)
      property <- getProperty[AppStack](user.propertyId)
    } yield property

    val result: Either[Error, Property] = program
      .runReader(PropertyApiUrl("https://production.property-api.com"))
      .runEither[Error]
      .run

    result match {
      case Left(e) => println(e) // log errors
      case Right(p) => println(s"User ${id.get} owns Property ${p.id.get}")
    }

    result
  }


  type _either[R] = MemberIn[Either[Error, _], R]

  def getUser[R: _either](id: UserId): Eff[R, User] =
    if (id.get > 1000)
      EitherCreation.right(User("Bob", id, PropertyId(123)))
    else
      EitherCreation.left(Error(s"Id ${id.get} in invalid range"))

  type _readerUrl[R] = MemberIn[Reader[PropertyApiUrl, _], R]

  def getProperty[R: _either : _readerUrl](id: PropertyId): Eff[R, Property] =
    for {
      propertyApiUrl <- ReaderCreation.ask[R, PropertyApiUrl]
      property <- if (propertyApiUrl.get == "https://production.property-api.com")
        EitherCreation.right(Property("Big house!", id))
      else
        EitherCreation.left(Error("Wrong URL!"))
    } yield property
}

object NoEff {
    def getUser(id: UserId): Either[Error, User] =
      if (id.get > 1000)
        Right(User("Bob", id, PropertyId(123)))
      else
        Left(Error(s"Id ${id.get} in invalid range"))

    def getProperty(id: PropertyId): Either[Error, Property] =
      if (id.get > 1000)
        Right(Property("Big house!", id))
      else
        Left(Error("Wrong URL!"))

    def getPropertyForUserId(id: UserId): Either[Error, Property] =
      for {
        user <- getUser(id)
        property <- getProperty(user.propertyId)
      } yield property

    def getProperty_v2(id: PropertyId): Reader[PropertyApiUrl, Either[Error, Property]] =
      Reader {
        propertyApiUrl =>
          if (propertyApiUrl.get == "https://production.property-api.com")
            Right(Property("Big house!", id))
          else
            Left(Error("Wrong URL!"))
      }

  //  def getPropertyForUserId_v2(id: UserId): Either[Error, Property] = {
  //    val errorOrUser: Either[Error, User] = getUser(id)
  //    errorOrUser map { user =>
  //      val readProperty: Reader[PropertyApiUrl, Property] = getProperty_v2(user.propertyId)
  //      readProperty.run("https://production.property-api.com")
  //    }
  //  }
}