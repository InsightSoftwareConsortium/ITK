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
  const unsigned int                                          Dimension = VDimension;
  typedef double                                              ValueType;
  typedef itk::RieszRotationMatrix<ValueType, Dimension>      SteerableMatrix;
  typedef typename SteerableMatrix::SpatialRotationMatrixType SpatialRotationMatrix;

  // Define a spatial rotation matrix.
  SpatialRotationMatrix R;
  double                angle = itk::Math::pi_over_4;
  double                cosA = cos(angle);
  double                sinA = sin(angle);
  double                C = 1 - cosA;
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
  SteerableMatrix S(R, 1);
  // copy
  SteerableMatrix Scopy(S);
  Scopy.SetOrder(2);
  Scopy.ComputeSteerableMatrix();
  std::cout << Scopy << std::endl;

  return EXIT_SUCCESS;
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
