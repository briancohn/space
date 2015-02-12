package bbdl.space

import org.scalatest.FunSpec

class HelloWorldSpec extends FunSpec {
  describe("Adding 1 to 1") {
    it("should equals 2"){
      assert(1+1 == 2)
    }
  }
}
// v = desired_force_output
// generators = DenseMatrix[]

		// next_random_point(
		// 	random_point_on_line_segment(
		// 		get_endpoints( //
		// 			get_random_direction( // may will make unit tests
		// 				get_basis(generators, v)), //may will make unit tests
		// 			get_first_inner_point(generators, v) //brian will make unit tests
		// 		)
		// 	)
		// )


// def next_random_point(
// 	p: DenseVector[Double],
//  	generators: DenseMatrix[Double],
//   	v: DenseVector[double],
//    	basis: DenseMatrix[Double]) = {
// 	p_new
// }


// input: [2,3,4; 8, 7, 3] output [1,2]





