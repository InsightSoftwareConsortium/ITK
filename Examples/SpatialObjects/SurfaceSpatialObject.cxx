/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SurfaceSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// \index{itk::SurfaceSpatialObject}
// SurfaceSpatialObject defines a surface in a N-dimensional space.
// A Surface is defined as a list of points which lie on the surface. Each point has a position and
// a unique normal.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkSurfaceSpatialObject.h"
#include "itkSurfaceSpatialObjectPoint.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// SurfaceSpatialObject is templated over the dimension of the space.
// A SurfaceSpatialObject contains a list of SurfaceSpatialObjectPoints.
// A SurfaceSpatialObjectPoint has a position, a normal and a color.
//
// First we define some type definitions
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::SurfaceSpatialObject<3>        SurfaceType;
  typedef SurfaceType::Pointer                SurfacePointer;
  typedef itk::SurfaceSpatialObjectPoint<3>   SurfacePointType;
  typedef itk::CovariantVector<double,3>      VectorType;

  SurfacePointer Surface = SurfaceType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We create a point list and we set the position of each point in the local
// coordinate system using the SetPosition() method. We also set the color of each
// point to red.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
 SurfaceType::PointListType list;

  for( unsigned int i=0; i<3; i++)
    {
    SurfacePointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetColor(1,0,0,1);
    VectorType normal;
    for(unsigned int j=0;j<3;j++)
    {
      normal[j]=j;
    }
    p.SetNormal(normal);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we create the surface and set his name using SetName(). We also set its Identification
// number with SetId() and we add the list of points previously created.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  Surface->GetProperty()->SetName("Surface1");
  Surface->SetId(1);
  Surface->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The GetPoints() method returns a reference to the internal list of points of the object.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
   SurfaceType::PointListType pointList = Surface->GetPoints();
   std::cout << "Number of points representing the surface: ";
   std::cout << pointList.size() << std::endl;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Then we can access the points using standard STL iterators.
// GetPosition() and GetColor() functions return respectively the position and the color
// of the point. GetNormal() returns the normal as a \doxygen{CovariantVector}.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  SurfaceType::PointListType::const_iterator it = Surface->GetPoints().begin(); 
  while(it != Surface->GetPoints().end())
    {
    std::cout << "Position = " << (*it).GetPosition() << std::endl;
    std::cout << "Normal = " << (*it).GetNormal() << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl; 
    std::cout << std::endl;
    it++;
    }
// Software Guide : EndCodeSnippet

  return 0;
}
