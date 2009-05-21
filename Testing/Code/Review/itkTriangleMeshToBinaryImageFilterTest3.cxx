/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleMeshToBinaryImageFilterTest3.cxx
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

#include "itkImage.h"
#include "itkMesh.h"
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkVTKPolyDataReader.h"

int itkTriangleMeshToBinaryImageFilterTest3( int argc, char * argv [] )
{ 

  if( argc != 12 )
    {
    std::cerr << "Usage: itkVTKPolyDataReaderTest ";
    std::cerr << " inputFilename.vtk outputImageMask";
    std::cerr << " imageSizeX imageSizeY imageSizeZ ";
    std::cerr << " imageOriginX imageOriginY imageOriginZ ";
    std::cerr << " imageSpacingX imageSpacingY imageSpacingZ ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;

  typedef itk::Mesh<float, Dimension>           MeshType;
  typedef itk::VTKPolyDataReader< MeshType >    ReaderType;

  ReaderType::Pointer  polyDataReader = ReaderType::New();

  polyDataReader->SetFileName(argv[1]);

  try
    {
    polyDataReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned char, 3> ImageType;
 
  typedef itk::TriangleMeshToBinaryImageFilter< MeshType, ImageType >  TriangleImageType;

  TriangleImageType::Pointer imageFilter = TriangleImageType::New();
  
  imageFilter->SetInput( polyDataReader->GetOutput() );
  
  ImageType::SizeType size;
  
  size[0] = atoi( argv[3] );
  size[1] = atoi( argv[4] );
  size[2] = atoi( argv[5] );

  imageFilter->SetSize( size );
  
  std::cout << "[PASSED]" << std::endl; 

  // Testing PrintSelf
  std::cout << imageFilter <<std::endl;

  //Update the filter
  imageFilter->Update();

  typedef itk::ImageFileWriter<ImageType > WriterType;

  WriterType::Pointer ImageWriter = WriterType::New();
  ImageWriter->SetInput(imageFilter->GetOutput() );
  ImageWriter->SetFileName( argv[2] );
  ImageWriter->Update();

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}
