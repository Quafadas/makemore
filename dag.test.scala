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
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)

    assert(dag.hasEdge(dna, dnb))
    assert(!dag.hasEdge(dnb, dna))
  }

  test("Remove nodes and check graph persistence") {
    val dag = new DAG()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)
    dag.removeNode(dna)

    assertEquals(dag.AdNodes, Set[AdNode](dnb))
    assert(!dag.hasEdge(dna, dnb))
  }

  test("Remove edges and confirm") {
    val dag = new DAG()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)
    dag.removeEdge(dna, dnb)

    assert(!dag.hasEdge(dna, dnb))
  }

  test("Topological sort with acyclic graph") {
    val dag = new DAG()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    val dnc = DebugNode("C")
    dag.addNode(dnc)
    dag.addNode(dnb)
    dag.addNode(dna)
    dag.addEdge(dna, dnb)
    dag.addEdge(dnb, dnc)

    val sorted = dag.toposort
    assertEquals(sorted, List(dna, dnb, dnc))
  }

  test("Topological sort with cyclic graph throws exception") {
    val dag = new DAG()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)
    dag.addEdge(dnb, dna)

    intercept[IllegalArgumentException] {
      dag.toposort
    }
  }

  test("Graphviz representation correctness") {
    val dag = new DAG()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)

    // println(dag.toGraphviz.trim)

    val expectedGraphviz =
      """digraph {
        |  "A" -> "B";
        |  "B";
        |}""".stripMargin

    assertNoDiff(dag.toGraphviz.trim, expectedGraphviz.trim)
  }

  test("Graph is empty") {
    val dag = new DAG()
    val dna = DebugNode("A")
    assert(dag.isEmpty)

    dag.addNode(dna)
    assert(!dag.isEmpty)

    dag.removeNode(dna)
    assert(dag.isEmpty)
  }
}
