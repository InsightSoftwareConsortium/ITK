/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    IsoSurfaceExtraction.cxx
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


//  Software Guide : BeginLatex
//
//  Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageRegionIterator.h"
#include "itkMesh.h"
#include "itkBinaryMask3DMeshSource.h"


int main(int argc, char * argv[] ) 
{

  if( argc < 3 )
    {
    std::cerr << "Usage: IsoSurfaceExtraction  inputImageFile   isoValue " << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginCodeSnippet
  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Define the pixel type of the input image
  typedef unsigned short  PixelType;

  // Declare the types of the output images
  typedef itk::Image< PixelType, Dimension >   ImageType;

  // Declare the reader type
  typedef itk::ImageFileReader< ImageType >    ReaderType;

  // Instantiate the reader object and attempt to read the image
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }


  // Declare the type of the Mesh
  typedef itk::Mesh<double>                         MeshType;

  // Declare the Filter for Iso-surface extraction
  typedef itk::BinaryMask3DMeshSource< MeshType >   MeshSourceType;

  MeshSourceType::Pointer meshSource = MeshSourceType::New();

  const PixelType isovalue = static_cast<PixelType>(atof( argv[2] ));

  meshSource->SetBinaryImage( reader->GetOutput() );

  meshSource->SetObjectValue( isovalue );

  try
    {
    meshSource->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during Update() " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << meshSource->GetNumberOfNodes() << std::endl;
  std::cout << meshSource->GetNumberOfCells() << std::endl;

  return EXIT_SUCCESS;

// Software Guide : EndCodeSnippet

}




