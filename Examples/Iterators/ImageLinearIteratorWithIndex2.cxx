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
// This example shows how to use the \doxygen{ImageLinearIteratorWithIndex} for
// computing the mean across time of a 4D image where the first three
// dimensions correspond to spatial coordinates and the fourth dimension
// corresponds to time. The result of the mean across time is to be stored in a
// 3D image.
//
// \index{Iterators!and 4D images}
// \index{ImageLinearIteratorWithIndex!4D images}
//
// Software Guide : EndLatex

#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkImageLinearConstIteratorWithIndex.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char *argv[] )
{
  // Verify the number of parameters on the command line.
  if ( argc < 3 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " input4DImageFile output3DImageFile"
              << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
//
// First we declare the types of the images, the 3D and 4D readers.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef unsigned char               PixelType;
  typedef itk::Image< PixelType, 3 >  Image3DType;
  typedef itk::Image< PixelType, 4 >  Image4DType;

  typedef itk::ImageFileReader< Image4DType > Reader4DType;
  typedef itk::ImageFileWriter< Image3DType > Writer3DType;
// Software Guide : EndCodeSnippet

  Reader4DType::Pointer reader4D = Reader4DType::New();
  reader4D->SetFileName( argv[1] );

  try
    {
    reader4D->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error reading the image" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  Image4DType::ConstPointer image4D = reader4D->GetOutput();

// Software Guide : BeginLatex
//
// Next, define the necessary types for indices, points, spacings, and size.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  Image3DType::Pointer image3D = Image3DType::New();
  typedef Image3DType::IndexType    Index3DType;
  typedef Image3DType::SizeType     Size3DType;
  typedef Image3DType::RegionType   Region3DType;
  typedef Image3DType::SpacingType  Spacing3DType;
  typedef Image3DType::PointType    Origin3DType;

  typedef Image4DType::IndexType    Index4DType;
  typedef Image4DType::SizeType     Size4DType;
  typedef Image4DType::SpacingType  Spacing4DType;
  typedef Image4DType::PointType    Origin4DType;
// Software Guide : EndCodeSnippet

  Index3DType       index3D;
  Size3DType        size3D;
  Spacing3DType     spacing3D;
  Origin3DType      origin3D;

  Image4DType::RegionType region4D = image4D->GetBufferedRegion();

  Index4DType       index4D   = region4D.GetIndex();
  Size4DType        size4D    = region4D.GetSize();
  Spacing4DType     spacing4D = image4D->GetSpacing();
  Origin4DType      origin4D  = image4D->GetOrigin();

// Software Guide : BeginLatex
//
// Here we make sure that the values for our resultant 3D mean image
// match up with the input 4D image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  for( unsigned int i=0; i < 3; i++)
    {
    size3D[i]    = size4D[i];
    index3D[i]   = index4D[i];
    spacing3D[i] = spacing4D[i];
    origin3D[i]  = origin4D[i];
    }

  image3D->SetSpacing( spacing3D );
  image3D->SetOrigin(  origin3D  );

  Region3DType region3D;
  region3D.SetIndex( index3D );
  region3D.SetSize( size3D );

  image3D->SetRegions( region3D  );
  image3D->Allocate();
// Software Guide : EndCodeSnippet

  typedef itk::NumericTraits< PixelType >::AccumulateType    SumType;
  typedef itk::NumericTraits< SumType   >::RealType          MeanType;

  const unsigned int timeLength = region4D.GetSize()[3];

  typedef itk::ImageLinearConstIteratorWithIndex<
                                  Image4DType > IteratorType;

// Software Guide : BeginLatex
//
// Next we iterate over time in the input image series, compute the average,
// and store that value in the corresponding pixel of the output 3D image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  IteratorType it( image4D, region4D );
  it.SetDirection( 3 ); // Walk along time dimension
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    SumType sum = itk::NumericTraits< SumType >::ZeroValue();
    it.GoToBeginOfLine();
    index4D = it.GetIndex();
    while( !it.IsAtEndOfLine() )
      {
      sum += it.Get();
      ++it;
      }
    MeanType mean = static_cast< MeanType >( sum ) /
                    static_cast< MeanType >( timeLength );

    index3D[0] = index4D[0];
    index3D[1] = index4D[1];
    index3D[2] = index4D[2];

    image3D->SetPixel( index3D, static_cast< PixelType >( mean ) );
    it.NextLine();
    }
// Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // As you can see, we avoid to use a 3D iterator to walk
  // over the mean image. The reason is that there is no
  // guarantee that the 3D iterator will walk in the same
  // order as the 4D. Iterators just adhere to their contract
  // of visiting every pixel, but do not enforce any particular
  // order for the visits.  The linear iterator guarantees it will
  // visit the pixels along a line of the image in the order
  // in which they are placed in the line, but does not state
  // in what order one line will be visited with respect to
  // other lines.  Here we simply take advantage of knowing
  // the first three components of the 4D iterator index,
  // and use them to place the resulting mean value in the
  // output 3D image.
  //
  // Software Guide : EndLatex

  Writer3DType::Pointer writer3D = Writer3DType::New();
  writer3D->SetFileName( argv[2] );
  writer3D->SetInput( image3D );

  try
    {
    writer3D->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error writing the image" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
