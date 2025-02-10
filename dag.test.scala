import munit._
  
type AdNodeString = AdNode

class DAGSuite extends FunSuite {  
  
  test("Add and retrieve nodes") {  
    val dag = new DAG[String]  
    dag.addNode("A")  
    dag.addNode("B")  
  
    assertEquals(dag.nodes, Set("A", "B"))  
  }  
  
  test("Add edges and check existence") {  
    val dag = new DAG[String]  
    dag.addNode("A")  
    dag.addNode("B")  
    dag.addEdge("A", "B")  
      
    assert(dag.hasEdge("A", "B"))  
    assert(!dag.hasEdge("B", "A"))  
  }  
  
  test("Remove nodes and check graph persistence") {  
    val dag = new DAG[String]  
    dag.addNode("A")  
    dag.addNode("B")  
    dag.addEdge("A", "B")  
    dag.removeNode("A")  
      
    assertEquals(dag.nodes, Set("B"))  
    assert(!dag.hasEdge("A", "B"))  
  }  
  
  test("Remove edges and confirm") {  
    val dag = new DAG[String]  
    dag.addNode("A")  
    dag.addNode("B")  
    dag.addEdge("A", "B")  
    dag.removeEdge("A", "B")  
      
    assert(!dag.hasEdge("A", "B"))  
  }  
  
  test("Topological sort with acyclic graph") {  
    val dag = new DAG[String]  
    dag.addNode("A")  
    dag.addNode("B")  
    dag.addNode("C")  
    dag.addEdge("A", "B")  
    dag.addEdge("B", "C")  
      
    val sorted = dag.toposort  
    assertEquals(sorted, List("A", "B", "C"))  
  }  
  
  test("Topological sort with cyclic graph throws exception") {  
    val dag = new DAG[String]  
    dag.addNode("A")  
    dag.addNode("B")  
    dag.addEdge("A", "B")  
    dag.addEdge("B", "A")  // introduces a cycle  
  
    intercept[IllegalArgumentException] {  
      dag.toposort  
    }  
  }  
  
  test("Graphviz representation correctness") {  
    val dag = new DAG[String]  
    dag.addNode("A")  
    dag.addNode("B")  
    dag.addEdge("A", "B")  
    val expectedGraphviz =   
      """digraph {
        |  "A" -> "B";
        |  "B";
        |}""".stripMargin  

    assertNoDiff(dag.toGraphviz.trim, expectedGraphviz.trim)  
  }  
  
  test("Graph is empty") {  
    val dag = new DAG[String]  
    assert(dag.isEmpty)  
  
    dag.addNode("A")  
    assert(!dag.isEmpty)  
  
    dag.removeNode("A")  
    assert(dag.isEmpty)  
  }  
}  