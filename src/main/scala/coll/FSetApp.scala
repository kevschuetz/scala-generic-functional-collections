package coll

import persons.*
import coll.*

object FSetApp extends App {

  var students: FSet[Student] = new FSet{val tree: HashTree[Nothing] = HashTree.Empty}
  students = students.add(new Student("Hans", "Maier", Study.INF))
  students = students.add(new Student("Franz", "Berger", Study.WIWI))
  students = students.add(new Student("Alois", "Berger", Study.LAW))
  students.foreach(println(_))

  val persons: FSet[Person] = students.add(new Professor("Niklas", "Maier", Science.Computer))
  persons.foreach(println(_))

  HashTree.printTree(persons.tree)
}
