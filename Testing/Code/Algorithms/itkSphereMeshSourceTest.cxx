/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereMeshSourceTest.cxx
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
#include "itkSphereMeshSource.h"
#include <iostream>
#include <string>
#include <math.h>

int itkSphereMeshSourceTest(int, char**){

  typedef itk::Point<float,3>  fPointType;
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
  IPT   pt;
  pt_ptr = &pt;

  std::cout << "Testing itk::SphereMeshSource "<< std::endl;
  for(int i=0; i<12; i++) {
  mySphereMeshSource->GetOutput()->GetPoint(i, pt_ptr);
  std::cout << "Point1: " << pt[0] << ", " << pt[1] << ", "<< pt[2] << std::endl;
  }
  std::cout << "Test End "<< std::endl;
  return 0;

}

