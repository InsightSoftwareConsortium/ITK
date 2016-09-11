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
#include <vector>
#include <iostream>
using namespace std;
using namespace itk;

int
itkInd2SubTest(int, char **)
{
  // Used for  initializing FixedArray.
  typedef unsigned int                 CArrayT[];
  const unsigned int                   D3 = 3;
  typedef FixedArray<unsigned int, D3> Array3DType;
  FixedArray<unsigned int, D3>         sizes3 = CArrayT{ 2, 2, 2 };
  std::vector<Array3DType>             storeOutput3D;
  std::cout << "sizes: (2,2,2) " << std::endl;
  for (unsigned int n = 0; n < sizes3[0] * sizes3[1] * sizes3[2]; ++n)
  {
    std::cout << "index " << n << " -> ";
    Array3DType out = Ind2Sub<D3>(n, sizes3);
    storeOutput3D.push_back(out);
    for (unsigned int i = 0; i < D3; ++i)
      std::cout << out[i] << " , ";
    std::cout << '\n';
  }
  // Check results:
  std::vector<Array3DType> expectedResult3D = { { CArrayT{ 0, 0, 0 },
                                                  CArrayT{ 1, 0, 0 },
                                                  CArrayT{ 0, 1, 0 },
                                                  CArrayT{ 1, 1, 0 },
                                                  CArrayT{ 0, 0, 1 },
                                                  CArrayT{ 1, 0, 1 },
                                                  CArrayT{ 0, 1, 1 },
                                                  CArrayT{ 1, 1, 1 } } };
  if (storeOutput3D != expectedResult3D)
    return EXIT_FAILURE;

  /*******************************/

  const unsigned int                   D2 = 2;
  typedef FixedArray<unsigned int, D2> Array2DType;
  FixedArray<unsigned int, D2>         sizes2 = CArrayT{ 3, 4 };
  std::vector<Array2DType>             storeOutput2D;
  std::cout << "sizes: (" << sizes2[0] << "," << sizes2[1] << ")" << std::endl;
  std::cout << "max_linear_index: (" << sizes2[0] * sizes2[1] - 1 << ")" << std::endl;
  for (unsigned int n = 0; n < sizes2[0] * sizes2[1]; ++n)
  {
    std::cout << "index " << n << " -> ";
    Array2DType out = Ind2Sub<D2>(n, sizes2);
    storeOutput2D.push_back(out);
    for (unsigned int i = 0; i < D2; ++i)
      std::cout << out[i] << " , ";
    std::cout << '\n';
  }
  // Check results:
  std::vector<Array2DType> expectedResult2D = { { CArrayT{ 0, 0 },
                                                  CArrayT{ 1, 0 },
                                                  CArrayT{ 2, 0 },
                                                  CArrayT{ 0, 1 },
                                                  CArrayT{ 1, 1 },
                                                  CArrayT{ 2, 1 },
                                                  CArrayT{ 0, 2 },
                                                  CArrayT{ 1, 2 },
                                                  CArrayT{ 2, 2 },
                                                  CArrayT{ 0, 3 },
                                                  CArrayT{ 1, 3 },
                                                  CArrayT{ 2, 3 } } };
  if (storeOutput2D != expectedResult2D)
    return EXIT_FAILURE;

  /*******************************/
  // Itk index/size interface:
  typedef itk::Index<D2>   Index2DType;
  itk::Size<D2>            sizesItk2 = { { 3, 4 } };
  std::vector<Array2DType> storeOutputIndex2D;
  for (unsigned int n = 0; n < sizesItk2[0] * sizesItk2[1]; ++n)
  {
    std::cout << "indexITK " << n << " -> ";
    Index2DType out = Ind2Sub<D2>(n, sizesItk2);
    storeOutputIndex2D.push_back(CArrayT{ static_cast<unsigned int>(out[0]), static_cast<unsigned int>(out[1]) });
    for (unsigned int i = 0; i < D2; ++i)
      std::cout << out[i] << " , ";
    std::cout << '\n';
  }
  if (storeOutputIndex2D != expectedResult2D)
    return EXIT_FAILURE;

  return EXIT_SUCCESS;
}
