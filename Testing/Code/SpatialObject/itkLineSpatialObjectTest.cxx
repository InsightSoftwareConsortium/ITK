/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineSpatialObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

/**
 * This is a test file for the itkLineSpatialObject class.
 */

#include "itkLineSpatialObject.h"
#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"

int itkLineSpatialObjectTest(int, char**)
{
  typedef itk::LineSpatialObject<3>      LineType;
  typedef LineType::Pointer              LinePointer;
  typedef LineType::LinePointType        LinePointType;
  typedef itk::CovariantVector<double,3> VectorType;

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing LineSpatialObject:"<<std::endl<<std::endl;

  LineType::PointListType list;

  for( unsigned int i=0; i<10; i++)
  {
    LinePointType p;
    p.SetPosition(i,i+1,i+2);
    VectorType normal1;
    VectorType normal2;
    for(unsigned int j=0;j<3;j++)
    {
      normal1[j]=j;
      normal2[j]=j*2;
    }
    
    p.SetNormal(normal1,0);
    p.SetNormal(normal2,1);
    list.push_back(p);
  }

  // Create a Line Spatial Object
  LinePointer Line = LineType::New();
  Line->GetProperty()->SetName("Line 1");
  Line->SetId(1);
  Line->SetPoints(list);
  Line->ComputeBoundingBox();

 // Number of points
  std::cout << "Testing Consistency: " << std::endl;
  std::cout << "Number of Points: ";

  if(Line->GetPoints().size() != 10)
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }
  else
  { 
    std::cout<<"[PASSED]"<<std::endl;
  }


  // Point consistency
  std::cout << "Point consistency: ";

  LineType::PointListType::const_iterator it = Line->GetPoints().begin();

  unsigned int i=0;
  while(it != Line->GetPoints().end())
  {
    for(unsigned int d=0;d<3;d++)
    {
      if((*it).GetPosition()[d]!=i+d)
      {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
      }

      if(((*it).GetNormal(0))[d]!=d)
      {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
      }
      
      if(((*it).GetNormal(1))[d]!=2*d)
      {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
      }

    }
    it++;
    i++;
  }

  std::cout<<"[PASSED]"<<std::endl;

  // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double,3> in;
  in[0]=1;in[1]=2;in[2]=3;
  itk::Point<double,3> out;
  out[0]=0;out[1]=0;out[2]=0;

  if(!Line->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(Line->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;
  return EXIT_SUCCESS;

}
