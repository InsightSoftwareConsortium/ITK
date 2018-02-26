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

#include "itkInd2Sub.h"
#include "itkTestingMacros.h"

#include <vector>
#include <iostream>

int
itkInd2SubTest(int, char *[])
{
  // Used for  initializing FixedArray.
  constexpr unsigned int D3 = 3;

  using Array3DType = itk::FixedArray<unsigned int, D3>;
  Array3DType ass3D; // from assigner3D c++98
  ass3D[0] = 2;
  ass3D[1] = 2;
  ass3D[2] = 2;
  itk::FixedArray<unsigned int, D3> sizes3(ass3D);
  std::vector<Array3DType>          storeOutput3D;
  std::cout << "sizes: (2,2,2) " << std::endl;
  for (unsigned int n = 0; n < sizes3[0] * sizes3[1] * sizes3[2]; ++n)
  {
    std::cout << "index " << n << " -> ";
    Array3DType out = itk::Ind2Sub<D3>(n, sizes3);
    storeOutput3D.push_back(out);
    for (unsigned int i = 0; i < D3; ++i)
    {
      std::cout << out[i] << " , ";
    }
    std::cout << std::endl;
  }
  // Check results: I have to like c++11 initializer lists.
  std::vector<Array3DType> expectedResult3D;
  ass3D[0] = 0;
  ass3D[1] = 0;
  ass3D[2] = 0; // {0,0,0};
  expectedResult3D.push_back(ass3D);
  ass3D[0] = 1;
  ass3D[1] = 0;
  ass3D[2] = 0; // {1,0,0};
  expectedResult3D.push_back(ass3D);
  ass3D[0] = 0;
  ass3D[1] = 1;
  ass3D[2] = 0; // {0,1,0};
  expectedResult3D.push_back(ass3D);
  ass3D[0] = 1;
  ass3D[1] = 1;
  ass3D[2] = 0; // {1,1,0};
  expectedResult3D.push_back(ass3D);
  ass3D[0] = 0;
  ass3D[1] = 0;
  ass3D[2] = 1; // {0,0,1};
  expectedResult3D.push_back(ass3D);
  ass3D[0] = 1;
  ass3D[1] = 0;
  ass3D[2] = 1; // {1,0,1};
  expectedResult3D.push_back(ass3D);
  ass3D[0] = 0;
  ass3D[1] = 1;
  ass3D[2] = 1; // {0,1,1};
  expectedResult3D.push_back(ass3D);
  ass3D[0] = 1;
  ass3D[1] = 1;
  ass3D[2] = 1; // {1,1,1};
  expectedResult3D.push_back(ass3D);
  if (storeOutput3D != expectedResult3D)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "3D Failure " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int D2 = 2;
  using Array2DType = itk::FixedArray<unsigned int, D2>;
  Array2DType ass2D;
  ass2D[0] = 3;
  ass2D[1] = 4;
  itk::FixedArray<unsigned int, D2> sizes2(ass2D);
  std::vector<Array2DType>          storeOutput2D;
  std::cout << "sizes: (" << sizes2[0] << "," << sizes2[1] << ")" << std::endl;
  std::cout << "Max linear index: (" << sizes2[0] * sizes2[1] - 1 << ")" << std::endl;
  for (unsigned int n = 0; n < sizes2[0] * sizes2[1]; ++n)
  {
    std::cout << "index " << n << " -> ";
    Array2DType out = itk::Ind2Sub<D2>(n, sizes2);
    storeOutput2D.push_back(out);
    for (unsigned int i = 0; i < D2; ++i)
    {
      std::cout << out[i] << " , ";
    }
    std::cout << std::endl;
  }

  // Check results
  std::vector<Array2DType> expectedResult2D;
  ass2D[0] = 0;
  ass2D[1] = 0;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 1;
  ass2D[1] = 0;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 2;
  ass2D[1] = 0;
  expectedResult2D.push_back(ass2D);

  ass2D[0] = 0;
  ass2D[1] = 1;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 1;
  ass2D[1] = 1;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 2;
  ass2D[1] = 1;
  expectedResult2D.push_back(ass2D);

  ass2D[0] = 0;
  ass2D[1] = 2;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 1;
  ass2D[1] = 2;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 2;
  ass2D[1] = 2;
  expectedResult2D.push_back(ass2D);

  ass2D[0] = 0;
  ass2D[1] = 3;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 1;
  ass2D[1] = 3;
  expectedResult2D.push_back(ass2D);
  ass2D[0] = 2;
  ass2D[1] = 3;
  expectedResult2D.push_back(ass2D);

  if (storeOutput2D != expectedResult2D)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "2D Failure " << std::endl;
    return EXIT_FAILURE;
  }

  // ITK index/size interface
  using Index2DType = itk::Index<D2>;
  itk::Size<D2> sizesItk2;
  sizesItk2[0] = 3;
  sizesItk2[1] = 4;
  std::vector<Array2DType> storeOutputIndex2D;
  for (unsigned int n = 0; n < sizesItk2[0] * sizesItk2[1]; ++n)
  {
    std::cout << "indexITK " << n << " -> ";
    Index2DType out = itk::Ind2Sub<D2>(n, sizesItk2);
    ass2D[0] = static_cast<unsigned int>(out[0]);
    ass2D[1] = static_cast<unsigned int>(out[1]);
    storeOutputIndex2D.push_back(ass2D);
    for (unsigned int i = 0; i < D2; ++i)
    {
      std::cout << out[i] << " , ";
    }
    std::cout << std::endl;
  }

  if (storeOutputIndex2D != expectedResult2D)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
