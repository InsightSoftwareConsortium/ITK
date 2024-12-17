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

#include "itkSphereMeshSource.h"
#include "itkTestingMacros.h"
#include <iostream>

int
itkSphereMeshSourceTest(int, char *[])
{

  using fPointType = itk::Point<float, 3>;
  using fSphereMeshSourceType = itk::SphereMeshSource<itk::Mesh<float>>;
  auto mySphereMeshSource = fSphereMeshSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mySphereMeshSource, SphereMeshSource, MeshSource);


  constexpr fPointType            center{};
  constexpr fPointType::ValueType scaleInit[3] = { 1, 1, 1 };
  const fPointType                scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolutionX(1);
  mySphereMeshSource->SetResolutionY(10);
  mySphereMeshSource->SetScale(scale);

  constexpr double squareness1 = 1.0;
  mySphereMeshSource->SetSquareness1(squareness1);

  constexpr double squareness2 = 1.0;
  mySphereMeshSource->SetSquareness2(squareness2);

  mySphereMeshSource->Modified();
  mySphereMeshSource->Update();

  using IPT = itk::Mesh<float>::PointType;
  //  itk::Mesh<float>::PointsContainerPointer      myoutput = mySphereMeshSource->GetOutput()->GetPoints();
  //  itk::Mesh<float>::PointsContainer::Iterator   m_output = myoutput->Begin();

  IPT   pt{};
  IPT * pt_ptr = &pt;

  for (int i = 0; i < 12; ++i)
  {
    mySphereMeshSource->GetOutput()->GetPoint(i, pt_ptr);
    std::cout << "Point1: " << pt[0] << ", " << pt[1] << ", " << pt[2] << std::endl;
  }
  std::cout << "Test End " << std::endl;
  return EXIT_SUCCESS;
}
