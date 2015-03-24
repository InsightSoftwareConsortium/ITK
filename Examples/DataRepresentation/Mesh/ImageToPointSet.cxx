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

//  Software Guide : BeginLatex
//
//  This example illustrates how to convert an ITK Image into a PointSet.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


#include "itkImage.h"
#include "itkPointSet.h"
#include "itkImageRegionConstIterator.h"


int main( int argc, char * argv[] )
{
  // Verify the number of parameters in the command line
  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  " << std::endl;
    return EXIT_FAILURE;
    }


  typedef unsigned char      PixelType;
  const   unsigned int       Dimension = 2;

  typedef itk::Image< PixelType, Dimension >    ImageType;
  typedef itk::PointSet< PixelType, Dimension > PointSetType;
  typedef itk::ImageFileReader< ImageType >     ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  const char * inputFilename  = argv[1];
  reader->SetFileName( inputFilename  );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  PointSetType::Pointer  pointSet = PointSetType::New();


  typedef itk::ImageRegionConstIterator< ImageType > IteratorType;

  const ImageType * image = reader->GetOutput();

  IteratorType it( image, image->GetBufferedRegion() );

  it.GoToBegin();


  typedef PointSetType::PointType     PointType;
  PointType point;

  unsigned long pointId = 0;

  while( !it.IsAtEnd() )
    {

    // Convert the pixel position into a Point
    image->TransformIndexToPhysicalPoint( it.GetIndex() , point );
    pointSet->SetPoint( pointId, point );

    // Transfer the pixel data to the value associated with the point.
    pointSet->SetPointData( pointId, it.Get() );

    ++it;
    ++pointId;
    }


  std::cout << "Number Of Points = ";
  std::cout << pointSet->GetNumberOfPoints() << std::endl;


  // Software Guide : EndCodeSnippet
  return EXIT_SUCCESS;
}
