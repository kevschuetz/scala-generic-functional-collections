package coll
import persons.*
import coll.FList.*

object FListApp extends App {

  var students: FList[Student] = FNil
  students = students.add(new Student("Hans", "Maier", Study.INF))
  students = students.add(new Student("Franz", "Berger", Study.WIWI))
  students.foreach(println(_))

  var persons : FList[Person] = students.add(new Professor("Niklas", "Wirth", Science.Computer))
  persons.foreach(println(_))

  val builder = new FListBuilder[Student]
  builder.collect(new Student("Hans", "Maier", Study.INF))
  builder.collect(new Student("Franz", "Berger", Study.WIWI))

  students = builder.build
  students.foreach(println(_))

  persons = students.add(new Professor("Niklas", "Wirth", Science.Computer))
  persons.foreach(println(_))

  persons.map(P => P.lastName).foreach(println(_))
  persons.filter(P => P.lastName.contains("Berger")).foreach(println(_))
  println(persons.fold("")((S, Person) => S + "Test"))
  println(persons.count)
  println(persons.toString("; ", "[", "]", E => E.firstName))
}