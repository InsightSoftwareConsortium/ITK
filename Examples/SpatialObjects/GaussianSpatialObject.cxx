/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    GaussianSpatialObject.cxx
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
// \index{itk::GaussianSpatialObject}
//
// A \doxygen{GaussianSpatialObject} 
// This example shows how to create a GaussianSpatiaObject which
// defines a gaussian in space. This object is particularly usefull
// to query the ValueAt() a point in physical space
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkGaussianSpatialObject.h"
// Software Guide : EndCodeSnippet

int main(int, char* [])
{
  // Software Guide : BeginLatex
  // The \doxygen{GaussianSpatialObject} is templated
  // over the dimensionality of the object. 
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef itk::GaussianSpatialObject<3>   GaussianType;
  GaussianType::Pointer myGaussian = GaussianType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The SetMaximum() function is used to set the maximum
  // value of the gaussian.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  myGaussian->SetMaximum(2); 
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginLatex
  // SetRadius() is used to defined the radius of the gaussian.
  // By default the radius is set to one.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  myGaussian->SetRadius(3);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The standard ValueAt() function is used to determine the value
  // of the gaussian at a particular point in physical space.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  itk::Point<double,3> pt;
  pt[0]=1; 
  pt[1]=2; 
  pt[2]=1;
  double value;
  myGaussian->ValueAt(pt, value);
  std::cout << "ValueAt(" << pt << ") = " << value << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}
