/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
  // using Self = Bogus;
  // using Pointer =  SmartPointer<Self>;
  // itkNewMacro(Self);
  //     static Bogus* New() { return new Bogus(); };
  //     void Register() {};
  //     void UnRegister() {};

  float
  operator()(double d, double)
  {
    return static_cast<float>(d);
  }
  void
  Visit(int, Bogus *)
  {}
  itk::CellGeometryEnum
  GetCellTopologyId()
  {
    return itk::CellGeometryEnum::HEXAHEDRON_CELL;
  }
  itk::CellGeometryEnum
  GetTopologyId()
  {
    return itk::CellGeometryEnum::HEXAHEDRON_CELL;
  }
  Bogus() = default;
  virtual ~Bogus() = default;
};

int
itkNewTest(int, char *[])
{
  // Call New and Print on as many classes as possible

  // CellInterfaceVisitorImplementation
  itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellTraits, Bogus, Bogus>::Pointer CIVI =
    itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellTraits, Bogus, Bogus>::New();
  if (CIVI.IsNull())
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
  // using iDHBO = itk::BackwardDifferenceOperator<double>;
  // auto dhbo = iDHBO::New();

  // ForwardDifferenceOperator
  // Bad
  // using iDHFO = itk::ForwardDifferenceOperator<double>;
  // auto dhfo = iDHFO::New();

  return EXIT_SUCCESS;
}
