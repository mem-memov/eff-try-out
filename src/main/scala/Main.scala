@main def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

object TryOut {

  case class PropertyId(id: Int)
//  case class User(name: String)
//
//  def getUser(id: UserId): Either[Error, User] =
//    if (id.get > 1000)
//      Right(User("Bob", id, PropertyId(123)))
//    else
//      Left(Error(s"Id ${id.get} in invalid range"))
//
//  def getProperty(id: PropertyId): Either[Error, Property] =
//    if (id.get > 1000)
//      Right(Property("Big house!", id))
//    else
//      Left(Error("Wrong URL!"))
}