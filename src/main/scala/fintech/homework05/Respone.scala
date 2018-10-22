package fintech.homework05

sealed trait Response[+T]

case class Success[+T](t: T) extends Response[T]

case class Failure[+T](message: String) extends Response[T]
