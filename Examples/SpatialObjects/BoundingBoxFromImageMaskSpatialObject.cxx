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
// \index{itk::ImageMaskSpatialObject!GetValue()}
//
// This example illustrates how to compute the bounding box around a binary
// contained in an ImageMaskSpatialObject. This is typically useful for
// extracting the region of interest of a segmented object and ignoring the
// larger region of the image that is not occupied by the segmentation.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageMaskSpatialObject.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"

int main( int argc, char * argv [] )
{

  if ( argc < 2 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputBinaryImageFile  ";
    return EXIT_FAILURE;
    }

  typedef itk::ImageMaskSpatialObject<3>      ImageMaskSpatialObject;

  typedef ImageMaskSpatialObject::ImageType   ImageType;
  typedef ImageType::RegionType               RegionType;
  typedef itk::ImageFileReader< ImageType >   ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  ImageMaskSpatialObject::Pointer maskSO = ImageMaskSpatialObject::New();

  maskSO->SetImage ( reader->GetOutput() );

  RegionType boundingBoxRegion  = maskSO->GetAxisAlignedBoundingBoxRegion();

  std::cout << "Bounding Box Region: " << boundingBoxRegion << std::endl;

  return EXIT_SUCCESS;
}
