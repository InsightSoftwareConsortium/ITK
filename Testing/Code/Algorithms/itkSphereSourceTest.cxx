/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSourceTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkSphereSource.h"
#include <iostream>
#include <string>
#include <math.h>

int main(){

  typedef itk::Point<float,3>  fPointType;
  typedef itk::SphereSource<itk::Mesh<float> >  fSphereSourceType;
  fSphereSourceType::Pointer  mySphereSource = fSphereSourceType::New();
  fPointType center; center.Fill(0);
  fPointType scale; scale = 1,1,1;
  
  mySphereSource->SetCenter(center);
  mySphereSource->SetResolutionX(1);
  mySphereSource->SetResolutionY(10);
  mySphereSource->SetScale(scale);

  mySphereSource->Modified();
  mySphereSource->Update();

  std::cout << "mySphereSource: " << mySphereSource;

  typedef itk::Mesh<float>::PointType   IPT;
//  itk::Mesh<float>::PointsContainerPointer      myoutput = mySphereSource->GetOutput()->GetPoints();
//  itk::Mesh<float>::PointsContainer::Iterator   m_output = myoutput->Begin();

  IPT*  pt_ptr;
  IPT   pt;
  pt_ptr = &pt;

  std::cout << "Testing itk::SphereSource "<< std::endl;
  for(int i=0; i<12; i++) {
  mySphereSource->GetOutput()->GetPoint(i, pt_ptr);
  std::cout << "Point1: " << pt[0] << ", " << pt[1] << ", "<< pt[2] << std::endl;
  }
  std::cout << "Test End "<< std::endl;
  return 0;

}

