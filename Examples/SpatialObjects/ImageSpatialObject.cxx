/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// Software Guide : BeginLatex
//
// \index{itk::ImageSpatialObject}
//
// An \doxygen{ImageSpatialObject} contains an \doxygen{Image} but adds the
// notion of spatial transformations and parent-child hierarchy. Let's begin
// the next example by including the appropriate header file.
//
// Software Guide : EndLatex

#include "itkImageRegionIterator.h"

// Software Guide : BeginCodeSnippet
#include "itkImageSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
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
//  Next we fill the image with increasing values.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageRegionIterator<Image> Iterator;
  Iterator it(image,region);
  short pixelValue =0;

  for(it.GoToBegin(); !it.IsAtEnd(); ++it, ++pixelValue)
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
// Then we set the itkImage to the ImageSpatialObject by using the
// \code{SetImage()} function.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  imageSO->SetImage(image);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// At this point we can use \code{IsInside()}, \code{ValueAt()} and
// \code{DerivativeAt()} functions inherent in SpatialObjects.  The
// \code{IsInside()} value can be useful when dealing with registration.
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
//  The \code{ValueAt()} returns the value of the closest pixel, i.e no interpolation, to
//  a given physical point.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  double returnedValue;
  imageSO->ValueAt(insidePoint,returnedValue);
  std::cout << "ValueAt(" << insidePoint << ") = " << returnedValue
            << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
//  The derivative at a specified position in space can be computed using the
//  \code{DerivativeAt()} function. The first argument is the point in
//  physical coordinates where we are evaluating the derivatives. The second
//  argument is the order of the derivation, and the third argument is the
//  result expressed as a \doxygen{Vector}.  Derivatives are computed
//  iteratively using finite differences and, like the \code{ValueAt()}, no
//  interpolator is used.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageSpatialObject::OutputVectorType returnedDerivative;
  imageSO->DerivativeAt(insidePoint,1,returnedDerivative);
  std::cout << "First derivative at " << insidePoint;
  std::cout << " = " << returnedDerivative << std::endl;
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
