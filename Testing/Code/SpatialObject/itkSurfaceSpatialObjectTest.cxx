/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSurfaceSpatialObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


/**
 * This is a test file for the itkSurfaceSpatialObject class.
 */

#include "itkSurfaceSpatialObject.h"
#include "itkSurfaceSpatialObjectPoint.h"
#include "itkCovariantVector.h"

int itkSurfaceSpatialObjectTest(int, char**)
{
  typedef itk::SurfaceSpatialObject<3>      SurfaceType;
  typedef SurfaceType::Pointer              SurfacePointer;
  typedef itk::SurfaceSpatialObjectPoint<3> SurfacePointType;
  typedef itk::CovariantVector<double,3>    VectorType;

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing SurfaceSpatialObject:"<<std::endl<<std::endl;

  SurfaceType::PointListType list;

  for( unsigned int i=0; i<10; i++)
  {
    SurfacePointType p;
    p.SetPosition(i,i+1,i+2);
    VectorType normal;
    for(unsigned int j=0;j<3;j++)
    {
      normal[j]=j;
    }
    p.SetNormal(normal);
    list.push_back(p);
  }

  // Create a Surface Spatial Object
  SurfacePointer Surface = SurfaceType::New();
  Surface->GetProperty()->SetName("Surface 1");
  Surface->SetId(1);
  Surface->SetPoints(list);

  Surface->ComputeBounds();

 // Number of points
  std::cout << "Testing Consistency: " << std::endl;
  std::cout << "Number of Points: ";

  if(Surface->GetPoints().size() != 10)
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

  SurfaceType::PointListType::const_iterator it = Surface->GetPoints().begin();

  unsigned int i=0;
  while(it != Surface->GetPoints().end())
  {
    for(unsigned int d=0;d<3;d++)
    {
      if((*it).GetPosition()[d]!=i+d)
      {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
      }

      if((*it).GetNormal()[d]!=d)
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

  if(!Surface->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(Surface->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;
  return EXIT_SUCCESS;

}
