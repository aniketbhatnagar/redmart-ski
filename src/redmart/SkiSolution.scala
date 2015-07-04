package redmart

import java.io._

object SkiSolution {

  def buildGraph(inputStream: InputStream): Graph[Long] = {
    val graph = new Graph[Long]
    val reader = new BufferedReader(new InputStreamReader(inputStream))
    val sizeStr = reader.readLine()
    val sizeSplits = sizeStr.split(" ")
    val numRows = sizeSplits(0).toInt
    val numCols = sizeSplits(1).toInt
    val map = Array.fill(numRows, numCols)(0L)
    for (i <- 0 until numRows) {
      val row = reader.readLine()
      val rowSplits  = row.split(" ")
      for (j <- 0 until rowSplits.length) {
        val elevation = rowSplits(j).toLong
        map(i)(j) = elevation
        graph.addVertex(graph.Vertex(vertexId(i, j), elevation))
      }
    }
    reader.close()
    for (i <- 0 until numRows; j <- 0 until numCols) {
      val elevation = map(i)(j)
      def addToGraph(x: Int, y: Int): Unit = {
        if (x >= 0 && y >= 0 && x < numRows && y < numCols && map(x)(y) < elevation) {
          graph.addEdge(vertexId(i, j), vertexId(x, y))
        }
      }
      addToGraph(i - 1, j)
      addToGraph(i, j - 1)
      addToGraph(i + 1, j)
      addToGraph(i, j + 1)
    }
    graph
  }

  def findLongest(graph: Graph[Long]): graph.Path  = {
    graph.foldPaths(graph.Path(List()))(
      edgeFolder = (pathSoFar, edge) => {
        pathSoFar.addEdge(edge)
      },
      reducer = (left, right) => {
        if (left.edges.size < right.edges.size) {
          right
        } else if (left.edges.size > right.edges.size){
          left
        } else {
          val leftElevation = elevation(graph)(left)
          val rightElevation = elevation(graph)(right)
          if (leftElevation < rightElevation) {
            right
          } else {
            left
          }
        }
      })
  }

  def elevation(graph: Graph[Long])(path: graph.Path) = {
    path.edges.head.from.value - path.edges.last.to.value
  }

  private def vertexId(i: Int, j: Int): String = i + "-" + j
}

object SkiSolutionMain extends App {
  val inputStream = new FileInputStream("map.txt")
  val graph = SkiSolution.buildGraph(inputStream)
  val startTime = System.currentTimeMillis()
  val longestPath = SkiSolution.findLongest(graph)
  val time = System.currentTimeMillis() - startTime
  println(longestPath.vertices.map(_.value).mkString(" -> "))
  println("Length - " + longestPath.numVertices)
  println("Elevation - " + SkiSolution.elevation(graph)(longestPath))
  println("Took - " + time + " ms")
}
