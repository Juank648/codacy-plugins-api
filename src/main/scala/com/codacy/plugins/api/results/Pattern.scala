package com.codacy.plugins.api.results

import com.codacy.plugins.api.languages.Language

object Pattern {

  case class Id(value: String) extends AnyVal {
    override def toString: String = value
  }

  case class Title(value: String) extends AnyVal {
    override def toString: String = value
  }

  case class DescriptionText(value: String) extends AnyVal {
    override def toString: String = value
  }

  case class TimeToFix(value: Int) extends AnyVal {
    override def toString: String = value.toString
  }

  case class Description(patternId: Pattern.Id,
                         title: Pattern.Title,
                         description: Option[Pattern.DescriptionText],
                         timeToFix: Option[Pattern.TimeToFix],
                         parameters: Option[Set[Parameter.Description]])

  case class Definition(patternId: Pattern.Id, parameters: Option[Set[Parameter.Definition]])

  case class Specification(patternId: Pattern.Id,
                           level: Result.Level,
                           category: Category,
                           parameters: Option[Set[Parameter.Specification]],
                           languages: Option[Set[Language]] = None)

  sealed trait Category

  object Category {
    sealed trait SecuritySubcategory
    object SecuritySubcategory {
      case object Injection extends SecuritySubcategory
      case object BrokenAuth extends SecuritySubcategory
      case object SensitiveData extends SecuritySubcategory
      case object XXE extends SecuritySubcategory
      case object BrokenAccess extends SecuritySubcategory
      case object Misconfiguration extends SecuritySubcategory
      case object XSS extends SecuritySubcategory
      case object BadDeserialization extends SecuritySubcategory
      case object VulnerableComponent extends SecuritySubcategory
      case object NoLogging extends SecuritySubcategory
    }

    case class Security(subcategory: Option[SecuritySubcategory]) extends Category
    case object CodeStyle extends Category
    case object ErrorProne extends Category
    case object Performance extends Category
    case object Compatibility extends Category
    case object UnusedCode extends Category
    case object Complexity extends Category
    case object BestPractice extends Category
    case object Comprehensibility extends Category
    case object Duplication extends Category
    case object Documentation extends Category
  }

  import play.api.libs.json._
  implicit val specificationFormat = new Reads[Specification] {
    def reads(json: JsValue): JsResult[Specification] = {
      val category: JsResult[Category] =
        (json \ "category") match {
          case JsDefined(JsString(value)) =>
            value match {
              case "Security" =>
                val subcategory = json \ "subcategory"
                val eitherCategory: Option[JsResult[Category.SecuritySubcategory]] = subcategory.toOption.map {
                  case JsString(value) =>
                    value match {
                      case "Injection"           => JsSuccess(Category.SecuritySubcategory.Injection)
                      case "BrokenAuth"          => JsSuccess(Category.SecuritySubcategory.BrokenAuth)
                      case "SensitiveData"       => JsSuccess(Category.SecuritySubcategory.SensitiveData)
                      case "XXE"                 => JsSuccess(Category.SecuritySubcategory.XXE)
                      case "BrokenAccess"        => JsSuccess(Category.SecuritySubcategory.BrokenAccess)
                      case "Misconfiguration"    => JsSuccess(Category.SecuritySubcategory.Misconfiguration)
                      case "XSS"                 => JsSuccess(Category.SecuritySubcategory.XSS)
                      case "BadDeserialization"  => JsSuccess(Category.SecuritySubcategory.BadDeserialization)
                      case "VulnerableComponent" => JsSuccess(Category.SecuritySubcategory.VulnerableComponent)
                      case "NoLogging"           => JsSuccess(Category.SecuritySubcategory.NoLogging)
                      case _                     => JsError("Wrong subcategory")
                    }
                  case _ => JsError("Subcategory should be a string")
                }
                val res: JsResult[Option[Category.SecuritySubcategory]] = eitherCategory match {
                  case None                    => JsSuccess(None)
                  case Some(JsSuccess(sub, _)) => JsSuccess(Some(sub))
                  case Some(JsError(err))      => JsError(err)
                }
                res.map(Category.Security.apply)
              case "CodeStyle"         => JsSuccess(Category.CodeStyle)
              case "ErrorProne"        => JsSuccess(Category.ErrorProne)
              case "Performance"       => JsSuccess(Category.Performance)
              case "Compatibility"     => JsSuccess(Category.Compatibility)
              case "UnusedCode"        => JsSuccess(Category.UnusedCode)
              case "Complexity"        => JsSuccess(Category.Complexity)
              case "BestPractice"      => JsSuccess(Category.BestPractice)
              case "Comprehensibility" => JsSuccess(Category.Comprehensibility)
              case "Duplication"       => JsSuccess(Category.Duplication)
              case "Documentation"     => JsSuccess(Category.Documentation)
            }
          case _ =>
            JsError("Category not correct")
        }
        category.map(Specification(???, ???, _, ???, ???))
    }
  }
}
