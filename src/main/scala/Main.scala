@main def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

object TryOut {
  // https://www.rea-group.com/about-us/news-and-insights/blog/a-journey-into-extensible-effects-in-scala/

  case class UserId(id: Int):
    def get: Int = id
  case class PropertyId(id: Int):
    def get: Int = id

  case class User(name: String, id: UserId, propertyId: PropertyId)
  case class Property(value: String, id: PropertyId)

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
}