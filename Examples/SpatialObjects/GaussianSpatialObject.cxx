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
// This example shows how to create a \doxygen{GaussianSpatialObject} which
// defines a gaussian in a N-dimensional space. This object is particularly useful
// to query the value at a point in physical space.
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
  // The \code{SetMaximum()} function is used to set the maximum
  // value of the gaussian.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  myGaussian->SetMaximum(2); 
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginLatex
  // The radius of the gaussian is defined by the \code{SetRadius()} method.
  // By default the radius is set to 1.0.
  // Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  myGaussian->SetRadius(3);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The standard \code{ValueAt()} function is used to determine the value
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
