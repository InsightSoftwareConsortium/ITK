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

#include <iostream>
#include "itkVectorImage.h"
#include "itkGradientImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

namespace
{

template <typename TInputImage>
int DoIt( const std::string &infname,
          const std::string &outfname )
{
  typedef TInputImage InputImageType;

  const unsigned int ImageDimension = InputImageType::ImageDimension;
  typedef typename InputImageType::PixelType InputPixelType;

  typedef itk::ImageFileReader<InputImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( infname );

  typedef itk::GradientImageFilter<InputImageType > FilterType;
  typename FilterType::Pointer filter = FilterType::New();

  filter->SetInput( reader->GetOutput() );

  typedef typename FilterType::OutputImageType  OutputImageType;
  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( outfname );
  writer->SetNumberOfStreamDivisions( 5 );
  writer->Update();

  std::cout << filter;

  typedef itk::VectorImage<InputPixelType, ImageDimension> VectorImageType;

  typedef itk::GradientImageFilter<InputImageType, float, float, VectorImageType> VectorFilterType;
  typename VectorFilterType::Pointer vectorFilter = VectorFilterType::New();
  vectorFilter->SetInput( reader->GetOutput() );
  vectorFilter->Update();

  filter->UpdateLargestPossibleRegion();

  itk::ImageRegionConstIterator<OutputImageType> iter( filter->GetOutput(),
    filter->GetOutput()->GetBufferedRegion() );
  itk::ImageRegionConstIterator<VectorImageType> viter( vectorFilter->GetOutput(),
    vectorFilter->GetOutput()->GetBufferedRegion() );

  // Check the filter output
  bool diff = false;
  while( !iter.IsAtEnd() )
    {

    for( unsigned int i = 0; i < ImageDimension; ++i )
      {
      if ( ! itk::Math::FloatAlmostEqual( iter.Get()[i], viter.Get()[i] ) )
        {
        diff = true;
        }
      }

    ++viter;
    ++iter;
    }

  if ( diff )
    {
    std::cerr << "VectorImage output does not match covariant!" << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;

}

}

int itkGradientImageFilterTest2(int argc, char * argv[] )
{

  if ( argc < 3 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string infname = argv[1];
  const std::string outfname = argv[2];

  itk::ImageIOBase::Pointer iobase =
    itk::ImageIOFactory::CreateImageIO( infname.c_str(), itk::ImageIOFactory::ReadMode);

  if ( iobase.IsNull() )
    {
    itkGenericExceptionMacro( "Unable to determine ImageIO reader for \"" << infname << "\"" );
    }

  typedef itk::Image<short,3>                      TestImageType;
  typedef itk::GradientImageFilter<TestImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  EXERCISE_BASIC_OBJECT_METHODS( filter, GradientImageFilter, ImageToImageFilter );

  const unsigned int dimension = iobase->GetNumberOfDimensions();

  if ( dimension == 2 )
    return DoIt< itk::Image<float, 2> >( infname, outfname );
  else if ( dimension == 3 )
    return DoIt< itk::Image<float, 3> >( infname, outfname );

  return EXIT_FAILURE;
}
