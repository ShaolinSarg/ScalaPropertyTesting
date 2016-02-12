import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class TestSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {


  case class Person(forename: String, surname: String, age: Int) {
    def fullName: String = s"$forename $surname"
  }


	def myDoublingFunction(input: Int): Int = input match {
    case x if x < 0 => throw new IllegalArgumentException("Cannot be less than zero")
    case _ => input * 2
  }

	describe("Property bases testing") {
		describe("can use built in type generators") {
			it ("should generate me a integers as input for my function and give me back the outputs") {

        forAll("inputNumber") { (n: Int) =>

          n match {
            case x if x < 0 => an [IllegalArgumentException] should be thrownBy myDoublingFunction(n)
            case _ => myDoublingFunction(n) shouldBe 3*n
          }

        }

			}
		}
		describe("you can also create you own generator using the Gen object for simple types") {
			it("should generate me even integers between -2000 and 2000") {
				
				val evenInts = for (n <- Gen.choose(-1000, 1000)) yield 2 * n

				forAll((evenInts, "inputNumber")) { (n: Int) =>

          whenever (n > 1000 && n < 2001) {
          	val result = myDoublingFunction(n)

          	result shouldBe 2*n // my unit test condition
       		}
      	}

			}
		}
    describe("you can also create you own generator to create your own types") {
      it("should generate people with different details") {

        val genPerson = for {
          forename <- Gen.oneOf("Dave", "Suzi", "Steve", "Dan", "Miffy")
          surname <- Gen.alphaStr
          age <- Gen.chooseNum(18, 80)
        } yield Person(forename, surname, age)

        forAll((genPerson, "person")) { (person: Person) =>

          whenever(person.age >= 18 && person.age <= 80) {
            val result = person.fullName
            println(person)
            result shouldBe person.forename + " " + person.surname
            result.trim.length shouldBe (person.forename + " " + person.surname).length //uncomment this to see the error case
          }
        }

      }
      it("should also let us generate json objects with generated field values") {

        val genPerson = for {
          forename <- Gen.oneOf("Dave", "Suzi", "Steve", "Dan", "Miffy")
          surname <- Gen.alphaStr
          age <- Gen.chooseNum(18, 80)
        } yield Json.obj(
          "forename" -> forename,
          "surname" -> surname,
          "age" -> age
        )

      }
    }
	}
}