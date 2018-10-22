

package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}
import Tree.max, Tree.depth, Tree.map

class TreeSpec extends FlatSpec with Matchers {

  val extraLittleTree : Leaf[Int] = Leaf[Int](1)
  val littleTree : Branch[Int] = Branch[Int](Leaf[Int](7), Leaf[Int](8))
  val biggerTree : Branch[Int] = Branch[Int](Leaf[Int](2), littleTree)
  val binaryTree : Branch[Int] = Branch[Int](Branch[Int](Leaf(2), Leaf(3)), Branch[Int](Leaf(71), Leaf(8)))
  val tree : Branch[Int] = Branch[Int](Branch[Int](Leaf[Int](9), Leaf[Int](1)),
    Branch[Int](littleTree, biggerTree))
  val bigTree : Branch[Int] = Branch[Int](binaryTree, tree)


  it should "work well with max function" in {
    max(extraLittleTree) should be(1)
    max(littleTree) should be(8)
    max(biggerTree) should be(8)
    max(tree) should be(9)
    max(binaryTree) should be(71)
    max(bigTree) should be(71)
  }

  it should "work well with size function" in {
    Tree.size(extraLittleTree) should be(1)
    Tree.size(littleTree) should be(3)
    Tree.size(biggerTree) should be(5)
    Tree.size(tree) should be(13)
    Tree.size(binaryTree) should be(7)
    Tree.size(bigTree) should be(21)
  }

  it should "work well with size function and not depend on the type" in {
    val littleTree: Branch[Seq[Char]] = Branch[Seq[Char]](Leaf[Seq[Char]](List('a', 'o')),
      Leaf[Seq[Char]](List('y', 'i')))

    val biggerTree: Branch[Any] = Branch[Any](Leaf[String]("2uwihi"), littleTree)
    val tree : Branch[Any] = Branch[Any](Branch[Any](Leaf[Any](9), Leaf[Any](1)),
      Branch[Any](littleTree, biggerTree))

    Tree.size(littleTree) should be(3)
    Tree.size(biggerTree) should be(5)
    Tree.size(tree) should be(13)
  }

  it should "work well with depth function" in {
    depth(extraLittleTree) should be(0)
    depth(littleTree) should be(1)
    depth(biggerTree) should be(2)
    depth(tree) should be(4)
    depth(binaryTree) should be(2)
    depth(bigTree) should be(5)
  }

  it should "work well with depth function and not depend on the type" in {
    val littleTree: Branch[Seq[Char]] = Branch[Seq[Char]](Leaf[Seq[Char]](List('a', 'o')),
      Leaf[Seq[Char]](List('y', 'i')))

    val biggerTree: Branch[Any] = Branch[Any](Leaf[String]("2uwihi"), littleTree)
    val tree : Branch[Any] = Branch[Any](Branch[Any](Leaf[Any](9), Leaf[Any](1)),
      Branch[Any](littleTree, biggerTree))

    depth(littleTree) should be(1)
    depth(biggerTree) should be(2)
    depth(tree) should be(4)
  }

  it should "map trees well" in {
    val withDoubleLittleTree = map(littleTree)((x : Int) => x.toDouble)

    withDoubleLittleTree match {
      case Branch(left, right) => left should be(Leaf(7.0)); right should be(Leaf(8.0))
      case _ => throw new AssertionError()
    }

    val withDoubleBigTree = map(bigTree)((x : Int) => x.toDouble)

    withDoubleBigTree match {
      case Branch(left, right) => left should be(
        Branch[Double](Branch[Double](Leaf(2.0), Leaf(3.0)), Branch[Double](Leaf(71.0), Leaf(8.0))))

        val littleDoubleTree = Branch[Double](Leaf[Double](7), Leaf[Double](8))

        right should be(Branch[Double](Branch[Double](Leaf[Double](9), Leaf[Double](1)),
          Branch[Double](littleDoubleTree,
            Branch[Double](Leaf[Double](2), littleDoubleTree))))

      case _ => throw new AssertionError("something goes wrong!")
    }
  }
}
