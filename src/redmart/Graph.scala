package redmart

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Graph[V] {

  private val _vertices: mutable.Map[String, Vertex] = new mutable.HashMap[String, Vertex]()
  private val _edges: mutable.ListBuffer[Edge] = new ListBuffer[Edge]()

  def addVertex(vertex: Vertex): Unit = {
    _vertices.put(vertex.id, vertex)
  }

  def addEdge(fromId: String, toId: String): Unit = {
    val from = _vertices(fromId)
    val to = _vertices(toId)
    val edge = Edge(from, to)
    _edges.append(edge)
    from._outEdges.append(edge)
    to._inEdges.append(edge)
  }

  def foldPaths[T](initialT: T)(edgeFolder: (T, Edge) => T, reducer: (T, T) => T): T = {
    case class VisitState(edge: Edge, visited: Set[Vertex], t: T)

    vertices.foldLeft(initialT)((oldT, vertex) => {
      val stack = new mutable.Stack[VisitState]
      val result = new ListBuffer[T]
      result.append(oldT)

      for (outEdge <- vertex.outEdges) {
        val state = VisitState(outEdge, Set(outEdge.from), initialT)
        stack.push(state)
      }

      while (!stack.isEmpty) {
        val visitState = stack.pop()
        val newT = edgeFolder(visitState.t, visitState.edge)
        val outEdges = visitState.edge.to.outEdges
        if (outEdges.length == 0 || outEdges.forall(edge => visitState.visited(edge.to))) {
          result.append(newT)
        } else {
          for (outEdge <- outEdges) {
            val toVisit = outEdge.to
            if (!visitState.visited(toVisit)) {
              stack.push(VisitState(outEdge, visitState.visited + toVisit, newT))
            }
          }
        }
      }
      result.reduce(reducer)
    })
  }

  def vertices = _vertices.values.toList
  def edges = _edges.toList

  case class Vertex(id: String, value: V) {
    private[Graph] val _outEdges: mutable.ListBuffer[Edge] = new ListBuffer[Edge]
    private[Graph] val _inEdges: mutable.ListBuffer[Edge] = new ListBuffer[Edge]
    def outEdges = _outEdges.toList
    def inEdges = _inEdges.toList
  }
  case class Edge(from: Vertex, to: Vertex)
  case class Path(edges: List[Edge]) {
    def addEdge(edge: Edge): Path = {
      Path(edges :+ edge)
    }
    def vertices: List[Vertex] = edges.head.from +: edges.map(_.to)
    def numVertices = edges.length + 1
  }

}
