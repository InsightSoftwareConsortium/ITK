/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    BoundingBoxFromImageMaskSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


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
