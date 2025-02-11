import munit._

type AdNodeString = AdNode

class DAGSuite extends FunSuite {

  test("Add and retrieve nodes") {
    val dag = new DAG()
    dag.addStringNode("A")
    dag.addStringNode("B")

    assertEquals(
      dag.AdNodes.asInstanceOf[Set[DebugNode]],
      Set(DebugNode("A"), DebugNode("B"))
    )
  }

  test("Add edges and check existence") {
    val dag = new DAG()
    dag.addStringNode("A")
    dag.addStringNode("B")
    dag.addEdge(DebugNode("A"), DebugNode("B"))

    assert(dag.hasEdge(DebugNode("A"), DebugNode("B")))
    assert(!dag.hasEdge(DebugNode("B"), DebugNode("A")))
  }

  test("Remove nodes and check graph persistence") {
    val dag = new DAG()
    dag.addStringNode("A")
    dag.addStringNode("B")
    dag.addSedge("A", "B")
    dag.removeNode(DebugNode("A"))

    assertEquals(dag.AdNodes, Set[AdNode](DebugNode("B")))
    assert(!dag.hasEdge(DebugNode("A"), DebugNode("B")))
  }

  test("Remove edges and confirm") {
    val dag = new DAG()
    dag.addStringNode("A")
    dag.addStringNode("B")
    dag.addSedge("A", "B")
    dag.removeSedge("A", "B")

    assert(!dag.hasEdge(DebugNode("A"), DebugNode("B")))
  }

  test("Topological sort with acyclic graph") {
    val dag = new DAG()
    dag.addStringNode("A")
    dag.addStringNode("B")
    dag.addStringNode("C")
    dag.addSedge("A", "B")
    dag.addSedge("B", "C")

    val sorted = dag.toposort
    assertEquals(sorted, List("A", "B", "C").map(DebugNode(_)))
  }

  test("Topological sort with cyclic graph throws exception") {
    val dag = new DAG()
    dag.addStringNode("A")
    dag.addStringNode("B") // fix: use addStringNode instead of addNode
    dag.addSedge("A", "B") // fix: use addSedge instead of addEdge
    dag.addSedge(
      "B",
      "A"
    ) // fix: use addSedge instead of addEdge to introduce a cycle

    intercept[IllegalArgumentException] {
      dag.toposort
    }
  }

  test("Graphviz representation correctness") {
    val dag = new DAG()
    dag.addStringNode("A")
    dag.addStringNode("B")
    dag.addSedge("A", "B")
    val expectedGraphviz =
      """digraph {
        |  "B";
        |  "A" -> "B";
        |}""".stripMargin

    assertNoDiff(dag.toGraphviz.trim, expectedGraphviz.trim)
  }

  test("Graph is empty") {
    val dag = new DAG()
    assert(dag.isEmpty)

    dag.addStringNode("A")
    assert(!dag.isEmpty)

    dag.removeNode(DebugNode("A"))
    assert(dag.isEmpty)
  }
}
