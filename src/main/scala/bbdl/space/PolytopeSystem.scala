package bbdl.space

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by B on 5/20/2015.
 * It represents the set of generators and constraints for a static posture for a multi-link tendon-driven system. A is
 * the set of A constraints, b is the vector of corresponding inequalities, and deltas is of the same length of A.cols,
 * representing the furthest that activation can change until the next time step.
 */
case class GeneratorSystem(A: DenseMatrix[Double], b: DenseVector[Double], deltas: DenseVector[Double])
case class KGeneratorSystems(KSystemArray: Array[GeneratorSystem])
case class LinSystem(A: DenseMatrix[Double], b: DenseVector[Double]) // for use in linear programming- a set of constraints where Ax <= b