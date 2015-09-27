package fp_in_scala.chapter_03

import org.scalatest.{Matchers, FlatSpec}

class TreeSpec extends FlatSpec with Matchers {

  val leaf1 = Leaf(1)
  val leaf2 = Leaf(2)
  val branch = Branch(leaf1, leaf2)
  val secondLevelBranch = Branch(branch, branch)
  val thirdLevelBranch = Branch(secondLevelBranch, secondLevelBranch)

  it should "calculate the size of a tree" in {
    leaf1.size should be (1)
    branch.size should be (3)
    branch.size should be (3)
    secondLevelBranch.size should be (7)
    thirdLevelBranch.size should be (15)

    Tree.sizeWithFold(leaf1) should be (1)
    Tree.sizeWithFold(branch) should be (3)
    Tree.sizeWithFold(branch) should be (3)
    Tree.sizeWithFold(secondLevelBranch) should be (7)
    Tree.sizeWithFold(thirdLevelBranch) should be (15)
  }

  it should "compute the maximum value of a leaf" in {
    Tree.maximum(leaf1) should be (1)
    Tree.maximum(leaf2) should be (2)

    Tree.maximumWithFold(leaf1) should be (1)
    Tree.maximumWithFold(leaf2) should be (2)
  }

  it should "compute the maximum value of a branch" in {
    Tree.maximum(branch) should be (2)
    Tree.maximumWithFold(branch) should be (2)
  }

  it should "compute the maximum value of nested branches" in {
    val tree = Branch(
      Branch(
        Branch(
          Branch(
            Branch(
              Branch(
                Branch(Leaf(1), Leaf(2)),
                Branch(Leaf(1), Leaf(2))
              ),
              Branch(
                Branch(Leaf(1), Leaf(3)),
                Branch(Leaf(1), Leaf(2))
              )
            ),
            Branch(Leaf(1), Leaf(4))
          ),
          Branch(
            Branch(Leaf(1), Leaf(2)),
            Branch(Leaf(1), Leaf(2))
          )
        ),
        Branch(Leaf(1), Leaf(2))
      ),
      Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(Leaf(1), Leaf(2))
      )
    )

    Tree.maximum(tree) should be (4)
    Tree.maximumWithFold(tree) should be (4)
  }

  it should "compute the depth value of a leaf" in {
    leaf1.depth should be (1)
    leaf2.depth should be (1)

    Tree.depthWithFold(leaf1) should be (1)
    Tree.depthWithFold(leaf2) should be (1)
  }

  it should "compute the depth value of a branch" in {
    branch.depth should be (2)
    Tree.depthWithFold(branch) should be (2)
  }

  it should "compute the depth value of nested branches" in {
    val tree = Branch(
      Branch(
        Branch(
          Branch(
            Branch(
              Branch(
                Branch(Leaf(1), Leaf(2)),
                Branch(Leaf(1), Leaf(2))
              ),
              Branch(
                Branch(Leaf(1), Leaf(3)),
                Branch(Leaf(1), Leaf(2))
              )
            ),
            Branch(Leaf(1), Leaf(4))
          ),
          Branch(
            Branch(Leaf(1), Leaf(2)),
            Branch(Leaf(1), Leaf(2))
          )
        ),
        Branch(Leaf(1), Leaf(2))
      ),
      Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(Leaf(1), Leaf(2))
      )
    )

    tree.depth should be (8)
  }

  it should "map the value of a leaf" in {
    leaf1.map(_.toString) should be (Leaf("1"))
    leaf2.map(_.toString) should be (Leaf("2"))

    Tree.mapWithFold(leaf1)(_.toString) should be (Leaf("1"))
    Tree.mapWithFold(leaf2)(_.toString) should be (Leaf("2"))
  }

  it should "map the values of a branch" in {
    Tree.mapWithFold(branch)(_.toString) should be (Branch(Leaf("1"), Leaf("2")))
  }

  it should "map the values of deeper branches" in {
    Tree.mapWithFold(Branch(branch, branch))(_.toString) should be (Branch(
      Branch(Leaf("1"), Leaf("2")),
      Branch(Leaf("1"), Leaf("2"))
    ))
  }
}
