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

#include "itkSphereMeshSource.h"
#include <iostream>

int itkSphereMeshSourceTest(int, char* [] ){

  typedef itk::Point<float,3>                       fPointType;
  typedef itk::SphereMeshSource<itk::Mesh<float> >  fSphereMeshSourceType;
  fSphereMeshSourceType::Pointer  mySphereMeshSource = fSphereMeshSourceType::New();
  fPointType center; center.Fill(0);
  fPointType::ValueType scaleInit[3] = {1,1,1};
  fPointType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolutionX(1);
  mySphereMeshSource->SetResolutionY(10);
  mySphereMeshSource->SetScale(scale);

  mySphereMeshSource->Modified();
  mySphereMeshSource->Update();

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;

  typedef itk::Mesh<float>::PointType   IPT;
//  itk::Mesh<float>::PointsContainerPointer      myoutput = mySphereMeshSource->GetOutput()->GetPoints();
//  itk::Mesh<float>::PointsContainer::Iterator   m_output = myoutput->Begin();

  IPT*  pt_ptr;
  IPT   pt; pt.Fill(0.0);
  pt_ptr = &pt;

  std::cout << "Testing itk::SphereMeshSource "<< std::endl;
  for(int i=0; i<12; i++)
    {
    mySphereMeshSource->GetOutput()->GetPoint(i, pt_ptr);
    std::cout << "Point1: " << pt[0] << ", " << pt[1] << ", "<< pt[2] << std::endl;
    }
  std::cout << "Test End "<< std::endl;
  return EXIT_SUCCESS;

}
