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

#include "itkPolylineMask2DImageFilter.h"
#include "itkPolyLineParametricPath.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkPolylineMask2DImageFilterTest(int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputFilename outputFilename " << std::endl;
    return EXIT_FAILURE;
    }

  // Declare the types of the images
  const unsigned int Dimension = 2;
  typedef unsigned char PixelType;

  typedef itk::Image<PixelType, Dimension>        InputImageType;
  typedef itk::Image<PixelType, Dimension>        OutputImageType;
  typedef itk::PolyLineParametricPath<Dimension>  InputPolylineType;

  // Read input image
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();

  std::cout << "Input filename = " << argv[1] << std::endl;

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an unexpected exception. " << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  InputImageType::ConstPointer inputImage    = reader->GetOutput();

  // Create polyline
  InputPolylineType::Pointer inputPolyline   = InputPolylineType::New();

  // We expect as input an image of 256 x 256 pixels with spacing 1,1.

  // Initialize the polyline
  typedef InputPolylineType::VertexType VertexType;

  // Add vertices to the polyline
  VertexType v0;
  v0[0] =  64.0;
  v0[1] = 128.0;
  inputPolyline->AddVertex(v0);

  VertexType v1;
  v1[0] = 128.0;
  v1[1] = 192.0;
  inputPolyline->AddVertex(v1);

  VertexType v2;
  v2[0] = 192.0;
  v2[1] = 128.0;
  inputPolyline->AddVertex(v2);

  VertexType v3;
  v3[0] = 128.0;
  v3[1] =  64.0;
  inputPolyline->AddVertex(v3);


  // Declare the type for the Mask image filter
  typedef itk::PolylineMask2DImageFilter<
                           InputImageType, InputPolylineType,
                           OutputImageType  >     InputFilterType;


  // Create a mask  Filter
  InputFilterType::Pointer filter = InputFilterType::New();

  // Connect the input image
  filter->SetInput1    ( inputImage );

  // Connect the Polyline
  filter->SetInput2    ( inputPolyline );

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  std::cout << "Output filename = " << argv[2] << std::endl;

  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an unexpected exception. " << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Output image has been saved" << std::endl;
  std::cout << std::endl;

  // Now cause and exception
  // Put a vertex outside of the image
  VertexType ve;
  ve[0] = 1000.0;
  ve[1] = 128.0;
  inputPolyline->AddVertex(ve);

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an expected exception. " << std::endl;
    std::cout << err << std::endl;
    return EXIT_SUCCESS;
    }
  return EXIT_FAILURE;

}
