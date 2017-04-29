/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkRieszRotationMatrix.h"
#include <complex>
#include "itkMath.h"

template <unsigned int VDimension>
int
runRieszRotationMatrixTest()
{
  bool                                                        testPassed = true;
  const unsigned int                                          Dimension = VDimension;
  typedef double                                              ValueType;
  typedef itk::RieszRotationMatrix<ValueType, Dimension>      SteerableMatrix;
  typedef typename SteerableMatrix::SpatialRotationMatrixType SpatialRotationMatrix;

  // Define a spatial rotation matrix.
  SpatialRotationMatrix R;
  // double angle = itk::Math::pi_over_4;
  double angle = itk::Math::pi_over_2;
  double cosA = cos(angle);
  double sinA = sin(angle);
  double C = 1 - cosA;
  if (Dimension == 2)
  {
    R[0][0] = cosA;
    R[0][1] = -sinA;
    R[1][0] = sinA;
    R[1][1] = cosA;
  }

  itk::FixedArray<double, Dimension> dir;
  dir.Fill(0);
  if (Dimension == 3)
  {
    double x = dir[0] = 0;
    double y = dir[1] = 0;
    double z = dir[2] = 1;
    R[0][0] = x * x * C + cosA;
    R[0][1] = x * y * C - z * sinA;
    R[0][2] = x * z * C + y * sinA;

    R[1][0] = y * x * C + z * sinA;
    R[1][1] = y * y * C + cosA;
    R[1][2] = y * z * C - x * sinA;

    R[2][0] = z * x * C - y * sinA;
    R[2][1] = z * y * C + x * sinA;
    R[2][2] = z * z * C + cosA;
  }
  // Check constructors of SteerableMatrix.
  // default
  SteerableMatrix    Sdefault;
  const unsigned int order1 = 1;
  Sdefault.SetOrder(order1);
  Sdefault.SetSpatialRotationMatrix(R);
  // compute
  SteerableMatrix S(R, order1);
  // copy
  SteerableMatrix Scopy(S);
  Scopy.SetOrder(2);
  Scopy.SetDebugOn();
  Scopy.ComputeSteerableMatrix();

  std::cout << "Rotation Matrix:" << std::endl;
  std::cout << R << std::endl;

  std::cout << "Matrix: Order 2" << std::endl;
  std::cout << Scopy << std::endl;

  typename SteerableMatrix::InternalMatrixType transposeMatrix = Scopy.GetTranspose();
  typename SteerableMatrix::InternalMatrixType identityMatrix = Scopy.GetVnlMatrix();
  identityMatrix.set_identity();
  typename SteerableMatrix::InternalMatrixType SmultST = Scopy.GetVnlMatrix() * transposeMatrix;

  if (SmultST != identityMatrix)
  {
    testPassed = false;
    std::cout << "test failed!" << std::endl;
    std::cerr << "S * S_transpose != identity" << std::endl;
    std::cout << "Matrix: S*S_transpose" << std::endl;
    std::cout << SmultST << std::endl;
  }

  // For angle = pi/2
  if (itk::Math::AlmostEquals(angle, itk::Math::pi_over_2))
  {
    if (Dimension == 2)
    {
      // f: order 2 dim 2:
      typename SteerableMatrix::InternalMatrixType g(3, 3);
      g(0, 0) = 0;
      g(0, 1) = 0;
      g(0, 2) = 1;
      g(1, 0) = 0;
      g(1, 1) = -1;
      g(1, 2) = 0;
      g(2, 0) = 1;
      g(2, 1) = 0;
      g(2, 2) = 0;
      if (Scopy.GetVnlMatrix() != g)
      {
        testPassed = false;
        std::cout << "test failed!" << std::endl;
        std::cerr << "expected result:" << std::endl;
        std::cerr << g << std::endl;
        std::cerr << "but got:" << std::endl;
        std::cerr << Scopy.GetVnlMatrix() << std::endl;
      }
    }
    if (Dimension == 3)
    {
      // f: order 2 dim 3:
      typename SteerableMatrix::InternalMatrixType f(6, 6);
      f(0, 0) = 0;
      f(0, 1) = 0;
      f(0, 2) = 0;
      f(0, 3) = 1;
      f(0, 4) = 0;
      f(0, 5) = 0;
      f(1, 0) = 0;
      f(1, 1) = -1;
      f(1, 2) = 0;
      f(1, 3) = 0;
      f(1, 4) = 0;
      f(1, 5) = 0;
      f(2, 0) = 0;
      f(2, 1) = 0;
      f(2, 2) = 0;
      f(2, 3) = 0;
      f(2, 4) = -1;
      f(2, 5) = 0;
      f(3, 0) = 1;
      f(3, 1) = 0;
      f(3, 2) = 0;
      f(3, 3) = 0;
      f(3, 4) = 0;
      f(3, 5) = 0;
      f(4, 0) = 0;
      f(4, 1) = 0;
      f(4, 2) = 1;
      f(4, 3) = 0;
      f(4, 4) = 0;
      f(4, 5) = 0;
      f(5, 0) = 0;
      f(5, 1) = 0;
      f(5, 2) = 0;
      f(5, 3) = 0;
      f(5, 4) = 0;
      f(5, 5) = 1;
      if (Scopy.GetVnlMatrix() != f)
      {
        testPassed = false;
        std::cout << "test failed!" << std::endl;
        std::cerr << "expected result:" << std::endl;
        std::cerr << f << std::endl;
        std::cerr << "but got:" << std::endl;
        std::cerr << Scopy.GetVnlMatrix() << std::endl;
      }
    }
  }
  std::cout << "Matrix: Order 1" << std::endl;
  std::cout << S << std::endl;

  // unsigned int highOrder = 10;
  // Sdefault.SetOrder(highOrder);
  // Sdefault.ComputeSteerableMatrix();
  // std::cout << "Matrix: Order " << highOrder << std::endl;
  // std::cout << "Matrix: size " << Sdefault.Rows() << std::endl;
  // std::cout << Sdefault << std::endl;

  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}

int
itkRieszRotationMatrixTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << argv[0] << "dimension" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int dimension = atoi(argv[1]);
  if (dimension == 2)
  {
    return runRieszRotationMatrixTest<2>();
  }
  else if (dimension == 3)
  {
    return runRieszRotationMatrixTest<3>();
  }
  else
  {
    std::cerr << "dimension = " << dimension << " . But test only work for 2 or 3 dimension." << std::endl;
    return EXIT_FAILURE;
  }
}
