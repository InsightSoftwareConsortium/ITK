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
// Disable warning for long symbol names in this file only

// Software Guide : BeginLatex
//
// \index{itk::ImageMaskSpatialObject}
//
// An \doxygen{ImageMaskSpatialObject} is similar to the \doxygen{ImageSpatialObject}
// and derived from it.
// However, the main difference is that the \code{IsInside()} returns true if the pixel
// intensity in the image is not zero.
//
// The supported pixel types does not include \doxygen{RGBPixel}, \doxygen{RGBAPixel}, etc...
// So far it only allows to manage images of simple types like unsigned short,
// unsigned int, or \doxygen{Vector}.
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageMaskSpatialObject.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkAffineTransform.h"

int main(int, char* [])
{
// Software Guide : BeginLatex
//
// The ImageMaskSpatialObject is templated over the dimensionality.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageMaskSpatialObject<3> ImageMaskSpatialObject;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we create an \doxygen{Image} of size 50x50x50 filled with zeros
// except a bright square in the middle which defines the mask.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef ImageMaskSpatialObject::PixelType     PixelType;
  typedef ImageMaskSpatialObject::ImageType     ImageType;
  typedef itk::ImageRegionIterator< ImageType > Iterator;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{ 50, 50, 50 }};
  ImageType::IndexType index = {{ 0, 0, 0 }};
  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  ImageType::RegionType insideRegion;
  ImageType::SizeType insideSize   = {{ 30, 30, 30 }};
  ImageType::IndexType insideIndex = {{ 10, 10, 10 }};
  insideRegion.SetSize( insideSize );
  insideRegion.SetIndex( insideIndex );

  Iterator it( image, insideRegion );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( itk::NumericTraits< PixelType >::max() );
    ++it;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then, we create an ImageMaskSpatialObject.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageMaskSpatialObject::Pointer maskSO = ImageMaskSpatialObject::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We then pass the corresponding pointer to the image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  maskSO->SetImage(image);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We can then test if a physical \doxygen{Point} is inside or outside the mask image.
// This is particularly useful during the registration process when only a part
// of the image should be used to compute the metric.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageMaskSpatialObject::PointType  inside;
  inside.Fill(20);
  std::cout << "Is my point " << inside << " inside my mask? "
    << maskSO->IsInside(inside) << std::endl;
  ImageMaskSpatialObject::PointType  outside;
  outside.Fill(45);
  std::cout << "Is my point " << outside << " outside my mask? "
    << !maskSO->IsInside(outside) << std::endl;
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
