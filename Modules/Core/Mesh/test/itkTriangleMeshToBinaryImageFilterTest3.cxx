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

#include "itkImage.h"
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

  ImageType::PointType origin;

  origin[0] = atof( argv[6] );
  origin[1] = atof( argv[7] );
  origin[2] = atof( argv[8] );

  imageFilter->SetOrigin( origin );

  ImageType::SpacingType spacing;

  spacing[0] = atof( argv[9] );
  spacing[1] = atof( argv[10] );
  spacing[2] = atof( argv[11] );

  imageFilter->SetSpacing( spacing );

  std::cout << "[PASSED]" << std::endl;

  // Testing PrintSelf
  std::cout << imageFilter <<std::endl;

  //Update the filter
  imageFilter->Update();

  typedef itk::ImageFileWriter<ImageType > WriterType;

  WriterType::Pointer imageWriter = WriterType::New();
  imageWriter->SetInput(imageFilter->GetOutput() );
  imageWriter->SetFileName( argv[2] );
  imageWriter->UseCompressionOn();
  imageWriter->Update();

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}
