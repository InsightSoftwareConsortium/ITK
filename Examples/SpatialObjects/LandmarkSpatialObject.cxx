/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    LandmarkSpatialObject.cxx
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
// \index{itk::LandmarkSpatialObject}
// LandmarkSpatialObject contains a list of \doxygen{SpatialObjectPoint} which have
// a position and a color.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkLandmarkSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// LandmarkSpatialObject is templated over the dimension of the space.
//
// Here we create a 3D Landmark.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  typedef itk::LandmarkSpatialObject<3>  LandmarkType;
  typedef LandmarkType::Pointer          LandmarkPointer;
  typedef itk::SpatialObjectPoint<3>     LandmarkPointType;

  LandmarkPointer landmark = LandmarkType::New();
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// Next, we set some properties of the object like its name and its identification number.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet  
  landmark->GetProperty()->SetName("Landmark1");
  landmark->SetId(1);
// Software Guide : EndCodeSnippet
  
// Software Guide : BeginLatex
//
// We are now ready to add points into the LandmarkSpatialObject.
// We first create a list of SpatialObjectPoint and for each point we set the position and the color.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  LandmarkType::PointListType list;

  for( unsigned int i=0; i<5; i++)
    {
    LandmarkPointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetColor(1,0,0,1);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
//  Then we add  the list to the object using the SetPoints() method. 
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  landmark->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The current point list can be accessed using the GetPoints() method.
// The method returns a reference to the (STL) list.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  unsigned int nPoints = landmark->GetPoints().size();
  std::cout << "Number of Points in the landmark: " << nPoints << std::endl;

  LandmarkType::PointListType::const_iterator it = landmark->GetPoints().begin();
  while(it != landmark->GetPoints().end())
    {
    std::cout << "Position: " << (*it).GetPosition() << std::endl;  
    std::cout << "Color: " << (*it).GetColor() << std::endl;  
    it++;
    }
// Software Guide : EndCodeSnippet

  return 0;
}
