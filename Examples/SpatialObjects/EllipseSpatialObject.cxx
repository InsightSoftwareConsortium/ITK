/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    EllipseSpatialObject.cxx
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
// \index{itk::EllipseSpatialObject}
// EllipseSpatialObject defines an N-Dimensional ellipse.
//
// Like other SpatialObjects this class derives from \doxygen{itkSpatialObject}.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkEllipseSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int argc, char *argv[] )
{
// Software Guide : BeginLatex
//
// Like most of the SpatialObjects, the EllipseSpatialObject is templated over the dimension of the space.
//
// Here we create a 3D ellipse.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3>   EllipseType;
  EllipseType::Pointer myEllipse = EllipseType::New();
// Software Guide : EndCodeSnippet
  
// Software Guide : BeginLatex
//
// Then ,we set a radius for each dimension
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  EllipseType::ArrayType radius;

  for(unsigned int i = 0; i<3 ;i++)
  {
    radius[i] = i;
  }
    
  myEllipse->SetRadius(radius);
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
// 
// Or if we have the same radius in each dimension we can do
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  myEllipse->SetRadius(2.0);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// 
// We can then display the current radius by using the GetRadius()
// function:
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  EllipseType::ArrayType myCurrentRadius = myEllipse->GetRadius();
  std::cout << "Current radius is " << myCurrentRadius << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// 
// Like other SpatialObjects, we can query the object if a point is inside
// the object by using the IsInside(itk::Point) function. This function expects 
// the point to be in world coordinates.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  itk::Point<double,3> insidePoint;
  insidePoint.Fill(1.0);
  if(myEllipse->IsInside(insidePoint))
    {
    std::cout << "The point " << insidePoint;
    std::cout << " is really inside the ellipse" << std::endl;
    }

  itk::Point<double,3> outsidePoint;
  outsidePoint.Fill(3.0);
  if(!myEllipse->IsInside(outsidePoint))
    {
    std::cout << "The point " << outsidePoint;
    std::cout << " is really outside the ellipse" << std::endl;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// 
// All SpatialObjects can be queried for a value at a point.
// The IsEvaluableAt() function returns a boolean to know if the object is evaluable at a
// particular point.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
   if(myEllipse->IsEvaluableAt(insidePoint))
    {
    std::cout << "The point " << insidePoint;
    std::cout << " is evaluable at the point " << insidePoint << std::endl;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// 
//  If the object is evaluable at that point, the ValueAt() function returns the current value at that position.
//  Most of the objects returns a boolean value which is set to true when the point is inside the object
//  and false when it is outside. However, for some objects, it is more interesting to return a
//  value representing, for instance, the distance from the center of the object or the distance from
//  from the boundary.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  double value;
  myEllipse->ValueAt(insidePoint,value);
  std::cout << "The value inside the ellipse is: " << value << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// 
// Like other SpatialObjects, we can also query the bounding box of the object by using
// GetBoundingBox()
// The resulting bounding box is expressed in the local frame.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  myEllipse->ComputeBoundingBox();
  EllipseType::BoundingBoxType * boundingBox = myEllipse->GetBoundingBox();
  std::cout << "Bounding Box: " << boundingBox->GetBounds() << std::endl;
// Software Guide : EndCodeSnippet

  return 0;
}
