import sbt.util.Logger

import scala.xml.{Node, NodeSeq, Elem}
import scala.xml.transform.{RewriteRule, RuleTransformer}

object PomPostProcessor {

  private def isTestDependency(elem: Elem): Boolean = {
    def isDependency: Boolean = elem.label == "dependency"
    def isTestScopeOnly: Boolean = {
      val scopes = elem.child.filter(_.label == "scope")
      scopes.nonEmpty && scopes.forall(_.text == "test")
    }
    isDependency && isTestScopeOnly
  }

  /**
   * Helper for excluding test dependencies from the pom.xml.
   *
   * (This is intended to ensure that stuff like munit is not listed as dependency in any form.)
   */
  def removeTestDependencies(logger: Logger)(node: Node): Node =
    // Solution idea is based on this StackOverflow answer:
    // https://stackoverflow.com/a/78567397/6152669
    // (This is not intended to be a legally binding copyright attribution because applying an idea
    // that was expressed in less than 15 lines of example code within similar code that only makes
    // a few straightforward calls to a standard API is not legally significant.)
    new RuleTransformer(new RewriteRule {
      override def transform(node: Node): NodeSeq = node match {
        case elem: Elem if isTestDependency(elem) =>
          def txt(label: String): String =
            elem.child.filter(_.label == label).flatMap(_.text).mkString
          def artifact: String =
            s"${txt("groupId")} % ${txt("artifactId")} % ${txt("version")}"
          logger.info(s"""test dependency "$artifact" has been omitted """)
          NodeSeq.Empty
        case _ => node
      }
    }).transform(node).head

}
