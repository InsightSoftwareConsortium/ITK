/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CylinderSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// \index{itk::CylinderSpatialObject}
//
// This example shows how to create a \doxygen{CylinderSpatialObject}.
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCylinderSpatialObject.h"
// Software Guide : EndCodeSnippet

int main(int, char* [])
{
  // Software Guide : BeginLatex
  // An \doxygen{CylinderSpatialObject} exists only in 3D, therefore, it is not templated.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef itk::CylinderSpatialObject   CylinderType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create a cylinder using the standard smart pointers.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  CylinderType::Pointer myCylinder = CylinderType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The radius of the cylinder is set using the \code{SetRadius()} function.
  // By default the radius is set to 1.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  double radius = 3.0;
  myCylinder->SetRadius(radius);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The height of the cylinder is set using the \code{SetHeight()} function.
  // By default the cylinder is defined along the X axis (first dimension).
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  double height = 12.0; 
  myCylinder->SetHeight(height);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Like any other \doxygen{SpatialObject}s, the \code{IsInside()} function can
  // be used to query if a point is inside or outside the cylinder.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  itk::Point<double,3> insidePoint;
  insidePoint[0]=1;
  insidePoint[1]=2;
  insidePoint[2]=0;
  std::cout << "Is my point "<< insidePoint << " inside the cylinder? : "  << myCylinder->IsInside(insidePoint) << std::endl;
  // Software Guide : EndCodeSnippet
   
  // Software Guide : BeginLatex
  // We can print the cylinder information using the \code{Print()} function.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  myCylinder->Print(std::cout);
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
