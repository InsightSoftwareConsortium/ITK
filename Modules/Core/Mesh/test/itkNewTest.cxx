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
#include "itkMesh.h"

  class Bogus
  {
   public:
    // typedef Bogus Self;
    // typedef SmartPointer<Self> Pointer;
    // itkNewMacro(Self);
//     static Bogus* New() { return new Bogus(); };
//     void Register() {};
//     void UnRegister() {};

    float operator() ( double d, double ) { return (float) d; }
    void Visit ( int, Bogus* ) {}
    int GetCellTopologyId() { return 1; }
    int GetTopologyId() { return 1; }
    Bogus() {}
    virtual ~Bogus() {}
  };

int itkNewTest ( int , char* [] )
{
  // Call New and Print on as many classes as possible

  // CellInterfaceVisitorImplementation
  itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellTraits, Bogus, Bogus>::Pointer CIVI
    = itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellTraits, Bogus, Bogus>::New();
  if(CIVI.IsNull())
    {
    return EXIT_FAILURE;
    }

  // CreateObjectFunction
  // itk::CreateObjectFunction<itk::Mesh<int> >::Pointer COF = itk::CreateObjectFunction<itk::Mesh<int> >::New();
  // itk::Mesh<int>::Pointer B = COF->CreateObject();

  // PixelAccessor
  // unused: float f = 100.0;
  // unused: double d = itk::PixelAccessor<float, double>::Get ( f );

  // BackwardDifferenceOperator
  // Bad
  // typedef itk::BackwardDifferenceOperator<double> iDHBO;
  // iDHBO::Pointer dhbo = iDHBO::New();

  // ForwardDifferenceOperator
  // Bad
  // typedef itk::ForwardDifferenceOperator<double> iDHFO;
  // iDHFO::Pointer dhfo = iDHFO::New();

  return EXIT_SUCCESS;
}
