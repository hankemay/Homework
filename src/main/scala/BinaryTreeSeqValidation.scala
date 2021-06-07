/**
  *
  * Description:
  * BinaryTreeSeqValidation
  *
  * @author hmxiao
  * @version 1.0
  */
import scala.collection.mutable.{HashMap, ArrayBuffer}


case class TreeNode(left: TreeNode = null, right: TreeNode = null, value: Int)

case class BinaryTreeSeqValidation(root: TreeNode) {

  var lu = new HashMap[Int, Int]()

  def init(): Unit = {
    var postOrder = new ArrayBuffer[Int]()

    //traverse the tree in post order
    traverse(root, postOrder)
    //postOrder.foreach(println)
    //build the lookup table by post order
    buildLookUp(postOrder)
  }


  def traverse(root: TreeNode, order: ArrayBuffer[Int]): Unit = {
    if (root == null) return
    //only put the leaf node
    if (root.left == null && root.right == null) order += root.value

    if (root.left != null) traverse(root.left, order)
    if (root.right != null) traverse(root.right, order)

  }

  def buildLookUp(order: ArrayBuffer[Int]): Unit = {
    for (i <- 0 until order.length) lu.put(order(i), i)
  }

  def validate(input: Array[Int]): Boolean = {
    val seq = for (e <- input) yield lu.getOrElse(e, -1)

    for (i <- 1 until seq.length) {
      //disorder or not exist which is not valid input
      if (seq(i) == -1 || seq(i) < seq(i - 1)) return false
    }

    return true
  }
}


object UTForBinaryTree {

  def main(args: Array[String]): Unit = {
    println("unit test")
    val leaf1 = TreeNode(value = 10)
    val leaf2 = TreeNode(value = 12)
    val leaf3 = TreeNode(value = 11)
    val leaf4 = TreeNode(value = 4)
    val leaf5 = TreeNode(value = 8)
    val leaf6 = TreeNode(value = 9)
    val leaf7 = TreeNode(value = 7)

    val root = TreeNode(
      left = TreeNode(
        left = TreeNode(
          left = leaf1,
          right = TreeNode(
            left = leaf2,
            right = leaf3,
            value = 13
          ),
          value = 5
        ),
        right = leaf4,
        value = 3
      ),
      right = TreeNode(
        left = TreeNode(
          right = leaf5,
          left = leaf6,
          value = 6
        ),
        right = leaf7,
        value = 2
      ),
      value = 1
    )

    val binaryTreeSeqValidation = BinaryTreeSeqValidation(root)
    binaryTreeSeqValidation.init()

    val a = Array(10, 12, 11)
    assert(binaryTreeSeqValidation.validate(a) == true)

    val b = Array(12, 10, 11)
    assert(binaryTreeSeqValidation.validate(b) == false)

    val c = Array(8, 4, 9)
    assert(binaryTreeSeqValidation.validate(c) == false)

    val d = Array(10, 9, 2)
    assert(binaryTreeSeqValidation.validate(d) == false)

    val f = Array(10, 11, 9, 8, 7)
    assert(binaryTreeSeqValidation.validate(f) == true)

    val g = Array(10)
    assert(binaryTreeSeqValidation.validate(g) == true)

    println("unit test passed!")
  }

}
