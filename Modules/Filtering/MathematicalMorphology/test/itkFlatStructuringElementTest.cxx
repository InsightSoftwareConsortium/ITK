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

#include "itkFlatStructuringElement.h"

int itkFlatStructuringElementTest(int, char *[])
{
  typedef itk::FlatStructuringElement< 2 > SE2Type;
  SE2Type::RadiusType r2;
  r2.Fill( 3 );
  SE2Type k2;

  k2 = SE2Type::Box( r2 );
  k2.Print(std::cout);

  k2 = SE2Type::Ball( r2 );
  k2.Print(std::cout);

  k2 = SE2Type::Polygon( r2, 2 );
  k2.Print(std::cout);
  k2 = SE2Type::Polygon( r2, 3 );
  k2.Print(std::cout);
  k2 = SE2Type::Polygon( r2, 4 );
  k2.Print(std::cout);
  k2 = SE2Type::Polygon( r2, 5 );

  typedef itk::FlatStructuringElement< 3 > SE3Type;
  SE3Type::RadiusType r3;
  r3.Fill( 3 );
  SE3Type k3;

  k3 = SE3Type::Box( r3 );
  k3.Print(std::cout);

  k3 = SE3Type::Ball( r3 );
  k3.Print(std::cout);

  k3 = SE3Type::Polygon( r3, 6 );
  k3.Print(std::cout);
  k3 = SE3Type::Polygon( r3, 7 );
  k3.Print(std::cout);
  k3 = SE3Type::Polygon( r3, 10 );
  k3.Print(std::cout);
  k3 = SE3Type::Polygon( r3, 16 );
  k3.Print(std::cout);

  bool catched = false;
  try
    {
    k3 = SE3Type::Polygon( r3, 200 );
    }
  catch(...)
    {
    catched = true;
    std::cout << "expected exception catched." << std::endl;
    }
  if( !catched )
    {
    std::cout << "expected exception NOT catched." << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::FlatStructuringElement< 4 > SE4Type;
  SE4Type::RadiusType r4;
  r4.Fill( 3 );
  SE4Type k4;

  k4 = SE4Type::Box( r4 );
  k4.Print(std::cout);

  k4 = SE4Type::Ball( r4 );
  k4.Print(std::cout);

  catched = false;
  try
    {
    k4 = SE4Type::Polygon( r4, 2 );
    }
  catch(...)
    {
    catched = true;
    std::cout << "expected exception catched." << std::endl;
    }
  if( !catched )
    {
    std::cout << "expected exception NOT catched." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
