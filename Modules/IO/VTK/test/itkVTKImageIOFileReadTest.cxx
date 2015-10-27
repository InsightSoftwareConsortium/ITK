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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageIOBase.h"
#include "itkVTKImageIO.h"

template< class TImage >
int ReadImage( const std::string fileName,
               typename TImage::Pointer image )
{

  typedef itk::VTKImageIO                         IOType;
  IOType::Pointer vtkIO                   =  IOType::New();

  if (!vtkIO->CanReadFile(fileName.c_str()))
    {
    return EXIT_FAILURE;
    }

  typedef TImage                             ImageType;
  typedef itk::ImageFileReader< ImageType >  ImageReaderType;

  typename ImageReaderType::Pointer reader = ImageReaderType::New();
  reader->SetImageIO(vtkIO);
  reader->SetFileName( fileName );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject& e )
    {
    std::cerr << e.what() << std::endl;
    return EXIT_FAILURE;
    }

  image->Graft( reader->GetOutput() );

  return EXIT_SUCCESS;
}

int itkVTKImageIOFileReadTest( int argc, char* argv[] )
{
  if( argc != 3 )
    {
    std::cerr << "Usage: "<< std::endl;
    std::cerr << argv[0];
    std::cerr << "matrix.vtk ironProt.vtk";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  // Read matrix.vtk file
  typedef itk::Image<float, 2>               matrixImageType;
  matrixImageType::Pointer matriximage     = matrixImageType::New();

  if( ReadImage< matrixImageType >( argv[1], matriximage ) == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }

  std::cout << matriximage << std::endl;

  // Read ironProt.vtk file
  typedef itk::Image<unsigned char, 3>       ironProtImageType;
  ironProtImageType::Pointer ironProtimage = ironProtImageType::New();

  if( ReadImage< ironProtImageType >( argv[2], ironProtimage ) == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }

  std::cout << ironProtimage << std::endl;

  return EXIT_SUCCESS;
}
