/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageSpatialObject.cxx
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
// \index{itk::ImageSpatialObject}
// An ImageSpatialObject contains an \doxygen{itkImage} but adds the notion
// of spatial transformations and parent-child hierarchy.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

#include <itkImageRegionIterator.h>

// Software Guide : BeginCodeSnippet
#include "itkImageSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int argc, char *argv[] )
{
// Software Guide : BeginLatex
//
//  We first create a simple 2D image of size 10 by 10 pixels.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Image<short,2> Image;
  Image::Pointer image = Image::New();
  Image::SizeType size = {{ 10, 10 }};
  Image::RegionType region;
  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
//  Next we fill the image with increasing values
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageRegionIterator<Image> Iterator;
  Iterator it(image,region);
  short pixelValue =0;
  it.GoToBegin();
  for(; !it.IsAtEnd(); ++it, ++pixelValue)
    {
    it.Set(pixelValue);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We can now define the ImageSpatialObject which is templated over the dimension
// and the pixel type of the image.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageSpatialObject<2,short> ImageSpatialObject;
  ImageSpatialObject::Pointer imageSO = ImageSpatialObject::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we set the itkImage to the ImageSpatialObject by using the SetImage() function.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  imageSO->SetImage(image);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// At this point we can use IsInside(), ValueAt() and DerivativeAt() functions inherent in
// SpatialObjects.
// The IsInside() value can be useful when dealing with registration.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Point<double,2> Point;
  Point insidePoint; 
  insidePoint.Fill(9);

  if( imageSO->IsInside(insidePoint) )
    {
    std::cout << insidePoint << " is inside the image." << std::endl;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
//  The ValueAt() returns the value of the closest pixel, i.e no interpolation, to
//  a given physical point.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  double returnedValue;
  imageSO->ValueAt(insidePoint,returnedValue);
 
  std::cout << "ValueAt(" << insidePoint << ") = " << returnedValue << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
//  The derivative at a specified position in space can be computed using the 
//  DerivativeAt() function. The first argument is the point in physical coordinates
//  where we are evaluating the derivatives. The second argument is the order of the 
//  derivation, and the third argument is the result expressed as a \doxygen{itkVector}.
//  Derivatives are computed iteratively using finite differences and, like the ValueAt(), no
//  interpolator is used.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  ImageSpatialObject::OutputVectorType returnedDerivative;
  imageSO->DerivativeAt(insidePoint,1,returnedDerivative);
  std::cout << "First derivative at " << insidePoint;
  std::cout << " = " << returnedDerivative << std::endl;
// Software Guide : EndCodeSnippet

  return 0;
}
