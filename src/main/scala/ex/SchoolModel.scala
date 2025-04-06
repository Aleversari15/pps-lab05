package ex

import util.Sequences.*
import util.Sequences.Sequence.Cons

trait CourseModel:
  def name: String

trait TeacherModel:
  def name: String

  def courses: Sequence[CourseModel]

trait SchoolModel:
  def courses: Sequence[String]
  def teachers: Sequence[String]
  def setTeacherToCourse(teacher: TeacherModel, course: CourseModel): SchoolModel
  def coursesOfATeacher(teacher: TeacherModel): Sequence[CourseModel]
  def hasTeacher(name: String): Boolean
  def hasCourse(name: String): Boolean

object CourseModel:
  def apply(name: String): CourseModel = CourseModelImpl(name)

  private case class CourseModelImpl(override val name: String) extends CourseModel

object TeacherModel:
  def apply(name: String): TeacherModel = TeacherModelImpl(name)
  def apply(name: String, courses: Sequence[CourseModel]): TeacherModel = TeacherModelImpl(name, courses)

  private case class TeacherModelImpl(override val name: String, override val courses: Sequence[CourseModel] = Sequence.apply()) extends TeacherModel

object SchoolModel:
  def apply(teachers: Sequence[TeacherModel]): SchoolModel = SchoolModelImpl(teachers)

  private class SchoolModelImpl(val teachersOfSchool: Sequence[TeacherModel]) extends SchoolModel:
    def courses: Sequence[String] = teachersOfSchool.flatMap(_.courses).map(_.name)

    def teachers: Sequence[String] = teachersOfSchool.map(_.name)

    //To fix
    def setTeacherToCourse(teacher: TeacherModel, course: CourseModel): SchoolModel =
      if teachersOfSchool.map(_.name).contains(teacher.name) then
        val updatedTeachers = teachersOfSchool.map(t =>
          if !t.courses.contains(course) then
            TeacherModel(t.name, t.courses.concat(Sequence(course)))
          else
            t)
        SchoolModel.apply(updatedTeachers)
      else
        SchoolModel(Sequence.apply(TeacherModel(teacher.name, Sequence.apply(course))).concat(teachersOfSchool))

    def coursesOfATeacher(teacher: TeacherModel): Sequence[CourseModel] = teacher.courses

    def hasTeacher(name: String): Boolean = teachers.contains(name)

    def hasCourse(name: String): Boolean = courses.contains(name)


@main def examples(): Unit =
  import SchoolModel.*
  import TeacherModel.*
  import CourseModel.*

  val school = SchoolModel.apply(Sequence.apply())
  println(school.teachers) // Nil()
  println(school.courses) // Nil()
  println(school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = TeacherModel.apply("John")
  val math = CourseModel.apply("Math")
  val italian = CourseModel.apply("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // Cons("John", Nil())
  println(school2.courses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // Cons("John", Nil())
  println(school3.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))