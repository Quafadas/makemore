import scala.collection.mutable
import java.util.UUID
import java.{util => ju}
import algebra.ring.Field
import algebra.ring.Ring
import spire.algebra.NRoot
import spire.algebra.Trig
import spire.implicits.*
import scala.reflect.ClassTag

trait AdNode[T: Field] {
  def id: UUID
  val realValue: T
  val infinitesimal: Array[T]
  var grad: T = summon[Field[T]].zero // Gradient accumulator
  def backward(using td: TejDim[T]): Unit
}
case class DebugNode[T: Field: ClassTag](msg: String) extends AdNode[T] {

  override val realValue: T = summon[Field[T]].zero

  override def backward(using td: TejDim[T]): Unit = {}

  val n = UUID.randomUUID()
  override inline def id: UUID = n
  val infinitesimal: Array[T] = Array.empty
}
case class TejNode[T: Field](tej: Tej[T]) extends AdNode[T] {
  override def id: UUID = tej.nodeId

  override val realValue: T = tej.j.real
  override val infinitesimal: Array[T] = tej.j.infinitesimal

  override def toString(): String =
    s"const \n v:$realValue g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def backward(using td: TejDim[T]): Unit = ()

}

enum UrnaryOps:
  case Sin, Cos, Tan, Exp, Log, Sinh, Cosh, Tanh, Neg, Sqrt, Abs

case class TejOpUrnary[T: Field: Trig](
    op: UrnaryOps,
    value: Tej[T],
    dep: UUID
) extends AdNode[T] {
  override def id: UUID = value.nodeId

  override val realValue: T = value.j.real
  override val infinitesimal: Array[T] = value.j.infinitesimal

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${value.nodeId.toString().takeRight(4)})"

  override def backward(using td: TejDim[T]): Unit =
    val n = td.dag.getNode(dep)
    op match
      // case UrnaryOps.Sin  => n.grad = n.grad + this.grad * cos(n.realValue)
      // case UrnaryOps.Cos  => n.grad = n.grad - this.grad * sin(n.realValue)
      // case UrnaryOps.Tan  => n.grad = n.grad + this.grad / (cos(n.realValue) * cos(n.realValue))
      case UrnaryOps.Exp => n.grad = n.grad + this.grad * n.realValue.exp
      case UrnaryOps.Log =>
        n.grad = n.grad + this.grad / n.realValue
      // case UrnaryOps.Sinh => n.grad = n.grad + this.grad * cosh(n.realValue)
      // case UrnaryOps.Cosh => n.grad = n.grad + this.grad * sinh(n.realValue)
      // case UrnaryOps.Tanh => n.grad = n.grad + this.grad / (cosh(n.realValue) * cosh(n.realValue))
      // case UrnaryOps.Neg  => n.grad = n.grad - this.grad
      // case UrnaryOps.Sqrt => n.grad = n.grad + this.grad / (2 * sqrt(n.realValue))
      case _ => ???

    // n.grad = td.dag.getNode(dep).grad + this.grad
}

enum BinaryOps:
  case Add, Sub, Mul, Div

case class TejOpBinary[T: Field](
    op: BinaryOps,
    value: Tej[T],
    left: UUID,
    right: UUID
) extends AdNode[T] {
  override def id: UUID = value.nodeId

  override val realValue: T = value.j.real
  override val infinitesimal: Array[T] = value.j.infinitesimal

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${value.nodeId.toString().takeRight(4)})"

  override def backward(using td: TejDim[T]): Unit = {
    val leftN = td.dag.getNode(left)
    val rightN = td.dag.getNode(right)
    op match
      case BinaryOps.Add =>
        leftN.grad = leftN.grad + this.grad
        rightN.grad = rightN.grad + this.grad
      case BinaryOps.Sub =>
        leftN.grad += this.grad
        rightN.grad -= this.grad

      case BinaryOps.Mul =>
        leftN.grad += this.grad * rightN.realValue
        rightN.grad += this.grad * leftN.realValue

      case BinaryOps.Div =>
        leftN.grad += this.grad / rightN.realValue
        rightN.grad -= this.grad * leftN.realValue / (rightN.realValue * rightN.realValue)

  }
}

def graphShow(adNode: AdNode[?]): String = adNode match
  case DebugNode(msg) => msg
  case TejNode(tej)   => tej.toString
  case TejOpUrnary(op, value, dep) =>
    s"$op \n $value \n (_id: ${(value.nodeId.toString().takeRight(4))})"
  case TejOpBinary(op, value, left, right) =>
    s"$op \n $value \n (_id: ${(value.nodeId.toString().takeRight(4))})"

class DAG[T: Field: ClassTag: Trig] {
  private val adjacencyList: mutable.Map[UUID, mutable.Set[UUID]] =
    mutable.Map.empty
  private val nodeMap: mutable.Map[UUID, AdNode[T]] = mutable.Map.empty

  inline def getAllNodes: Set[AdNode[T]] = {
    nodeMap.values.toSet
  }

  inline def getAllEdges: Set[(AdNode[T], AdNode[T])] = {
    adjacencyList.flatMap { case (fromId, toIds) =>
      toIds.map(toId => (nodeMap(fromId), nodeMap(toId)))
    }.toSet
  }

  inline def getNode(id: UUID): AdNode[T] = {
    nodeMap.getOrElse(
      id,
      throw new NoSuchElementException(s"Node with id $id not found")
    )
  }

  inline def addAdNode(adNode: AdNode[T]): Unit = {
    if (!adjacencyList.contains(adNode.id)) {
      adjacencyList(adNode.id) = mutable.Set.empty
      nodeMap(adNode.id) = adNode
    }
  }

  inline def addNode(adNode: AdNode[T]): Unit = {
    addAdNode(adNode)
  }

  inline def addStringNode(str: String): Unit = {
    val adNode = DebugNode(str)
    addAdNode(adNode)
  }

  inline def addTejNode(tej: Tej[T]): Unit = {
    val adNode = TejNode(tej)
    addAdNode(adNode)
  }

  inline def addOpNodeUrnary(
      op: UrnaryOps,
      t: Tej[T],
      dep: UUID
  ): Unit = {
    val adNode = TejOpUrnary(op, t, dep)
    addAdNode(adNode)
  }

  inline def addEdge[T](from: AdNode[T], to: AdNode[T]): Unit = {
    require(adjacencyList.contains(from.id), s"AdNode $from does not exist.")
    require(adjacencyList.contains(to.id), s"AdNode $to does not exist.")
    // println(s"Adding edge from ${from} to ${to}")
    adjacencyList(from.id) += to.id
  }

  inline def addTedge(from: Tej[T], to: Tej[T]): Unit = {
    val fromNode = TejNode(from)
    val toNode = TejNode(to)
    addEdge(toNode, fromNode)
  }

  inline def addUrnaryEdge(
      from: Tej[T],
      to: TejOpUrnary[T]
  ): Unit = {
    val fromNode = TejNode(from)
    addEdge(to, fromNode)
  }

  inline def addBinaryEdge(
      left: Tej[T],
      right: Tej[T],
      to: TejOpBinary[T]
  ): Unit = {
    val fromNode = TejNode(to.value)
    addEdge(to, TejNode(left))
    addEdge(to, TejNode(right))
  }

  inline def addSedge[T](from: String, to: String): Unit = {
    val fromNode = DebugNode(from)
    val toNode = DebugNode(to)
    addEdge(fromNode, toNode)
  }

  inline def removeNode(adNode: AdNode[T]): Unit = {
    adjacencyList -= adNode.id
    nodeMap -= adNode.id
    adjacencyList.values.foreach(_ -= adNode.id)
  }

  inline def removeEdge(from: AdNode[T], to: AdNode[T]): Unit = {
    adjacencyList.get(from.id).foreach(_ -= to.id)
  }

  inline def removeSedge(from: String, to: String): Unit = {
    val fromNode = DebugNode(from)
    val toNode = DebugNode(to)
    adjacencyList.get(fromNode.id).foreach(_ -= toNode.id)
  }

  inline def neighbors(adNode: AdNode[T]): Set[AdNode[T]] = {
    adjacencyList.getOrElse(adNode.id, Set.empty).flatMap(nodeMap.get).toSet
  }

  inline def hasEdge(from: AdNode[T], to: AdNode[T]): Boolean = {
    adjacencyList.get(from.id).exists(_.contains(to.id))
  }

  inline def isEmpty: Boolean = adjacencyList.isEmpty

  def toposort: List[AdNode[T]] = {
    val adj = mutable.Map.empty[AdNode[T], mutable.ListBuffer[AdNode[T]]]
    val indegree = mutable.Map.empty[AdNode[T], Int].withDefaultValue(0)

    // Build adjacency list and indegree map
    for ((from, to) <- getAllEdges) {
      adj.getOrElseUpdate(from, mutable.ListBuffer()) += to
      indegree(to) += 1
      adj.getOrElseUpdate(
        to,
        mutable.ListBuffer()
      ) // Ensure all nodes are in adj
    }

    // Initialize queue with nodes having indegree 0
    val q = mutable.Queue[AdNode[T]]()
    for (node <- adj.keys if indegree(node) == 0) {
      q.enqueue(node)
    }

    val result = mutable.ListBuffer[AdNode[T]]()
    while (q.nonEmpty) {
      val node = q.dequeue()
      result += node

      for (neighbor <- adj(node)) {
        indegree(neighbor) -= 1
        if (indegree(neighbor) == 0) {
          q.enqueue(neighbor)
        }
      }
    }

    // Check for cycle
    if (result.size != adj.size) {
      throw new IllegalArgumentException("Graph contains cycle!")
    }

    result.toList
  }

  inline def toGraphviz: String = {
    val sb = new StringBuilder
    sb.append("digraph {\n")

    adjacencyList.foreach { case (node, neighbors) =>
      if (neighbors.isEmpty) {
        sb.append(s"  \"${graphShow(nodeMap(node))}\";\n")
      } else {
        neighbors.foreach { neighbor =>
          sb.append(
            s"  \"${graphShow(nodeMap(node))}\" -> \"${graphShow(nodeMap(neighbor))}\";\n"
          )
        }
      }
    }

    sb.append("}")
    sb.toString()
  }
}
