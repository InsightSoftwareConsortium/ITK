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

#include "itkMapContainer.h"
#include "itkPoint.h"

#include <iostream>

/**
 * Some typedefs to make things easier.
 */
typedef   itk::Point<float,3>     PointType;
typedef   itk::Vector<float,3>    VectorType;

typedef itk::MapContainer< unsigned long,
                           PointType >     ContainerType;
typedef ContainerType::Pointer             ContainerPointer;

int itkMapContainerTest(int, char* [] )
{

  /**
   * Create the Container
   */
  ContainerPointer  container = ContainerType::New();

  PointType pointA;
  PointType pointB;
  PointType pointC;
  PointType pointD;

  VectorType displacement;

  displacement[0] = 2;
  displacement[1] = 5;
  displacement[2] = 9;

  pointA.Fill( 0.0 );
  pointB = pointA + displacement;
  pointC = pointB + displacement;
  pointD = pointC + displacement;

  container->SetElement( 0, pointA );
  container->SetElement( 1, pointB );
  container->SetElement( 2, pointC );
  container->SetElement( 3, pointD );

  ContainerType::Iterator p = container->Begin();

  while( p != container->End() )
   {
   std::cout << p.Value() << std::endl;
   p++;
   }

  container->Initialize();
  if( container->Size() != 0 )
    {
    std::cerr << "Initialize() didn't get rid of elements" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
