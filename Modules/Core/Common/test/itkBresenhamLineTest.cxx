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

#include <iostream>

#include "itkBresenhamLine.h"


int itkBresenhamLineTest( int, char * [] )
{
  {
  // Test BuildLine(Vector, distance)
  itk::Vector<float, 2> v;
  v[0] = 1;
  v[1] = 1;

  itk::BresenhamLine<2> line;
  std::vector< itk::Offset<2> > offsets = line.BuildLine(v, 4);

  if(offsets.size() != 4)
    {
    std::cerr << "Test failed! 'offsets' should be length 4 and it is " << offsets.size() << std::endl;
    return EXIT_FAILURE;
    }

  for(int i = 0; i < 4; i++)
    {
    if(offsets[i][0] != i || offsets[i][1] != i)
      {
      std::cerr << "Test failed! offsets[" << i << "] should be (" << i << ", " << i << ") and it is " << offsets[i] << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  {
  // Test BuildLine(Index, Index)
  itk::Index<2> p0;
  p0[0] = 0;
  p0[1] = 0;

  itk::Index<2> p1;
  p1[0] = 3;
  p1[1] = 3;

  itk::BresenhamLine<2> line;
  std::vector< itk::Index<2> > indices = line.BuildLine(p0, p1);

  if(indices.size() != 4)
    {
    std::cerr << "Test failed! 'indices' should be length 4 and it is " << indices.size() << std::endl;
    return EXIT_FAILURE;
    }

  for(int i = 0; i < 4; i++)
    {
    if(indices[i][0] != i || indices[i][1] != i)
      {
      std::cerr << "Test failed! indices[" << i << "] should be (" << i << ", " << i << ") and it is " << indices[i] << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  std::cout << "Test Passed !" << std::endl;
  return EXIT_SUCCESS;
}
