import sbt.util.Logger

import scala.xml.{Node, NodeSeq, Elem}
import scala.xml.transform.{RewriteRule, RuleTransformer}

object PomPostProcessor {

  /**
   * Helper for excluding scoped dependencies from the pom.xml.
   *
   * Based on this StackOverflow-answer: [[https://stackoverflow.com/a/51416386]].
   *
   * (This is intended to ensure that munit is not listed as dependency in any form.)
   */
  def removeScopedDependencies(logger: Logger)(node: Node): Node =
    new RuleTransformer(new RewriteRule {
      override def transform(node: Node): NodeSeq = node match {
        case e: Elem
            if e.label == "dependency" && e.child.exists(child => child.label == "scope") =>
          def txt(label: String): String =
            "\"" + e.child.filter(_.label == label).flatMap(_.text).mkString + "\""
          logger.info(s"""scoped dependency ${txt("groupId")} % ${txt("artifactId")} % ${txt(
              "version"
            )} % ${txt("scope")} has been omitted """)
          NodeSeq.Empty
        case _ => node
      }
    }).transform(node).head

}
