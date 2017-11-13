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

#include "itkTestingMacros.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVotingBinaryImageFilter.h"


namespace
{

template<typename  TInputImageType >
int itkVotingBinaryImageFilterTestImp( const std::string &infname,
                                       const std::string &outfname,
                                       itk::SizeValueType radius,
                                       long foregroundValue,
                                       long backgroundValue,
                                       unsigned int birthThreshold = 1,
                                       unsigned int survivalThreshold = 1)
{
  typedef TInputImageType InputImageType;
  typedef TInputImageType OutputImageType;

  typedef typename TInputImageType::PixelType InputPixelType;

  typedef itk::ImageFileReader<InputImageType>  ReaderType;
  typedef itk::ImageFileWriter<OutputImageType> WriterType;

  typedef itk::VotingBinaryImageFilter<InputImageType, OutputImageType> FilterType;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( infname );

  typename FilterType::Pointer filter = FilterType::New();

  filter->SetInput( reader->GetOutput() );

  typename FilterType::InputSizeType R;
  R.Fill( itk::Math::CastWithRangeCheck<itk::SizeValueType>(radius) );
  filter->SetRadius( R );

  filter->SetForegroundValue( itk::Math::CastWithRangeCheck<InputPixelType>( foregroundValue ) );
  filter->SetBackgroundValue( itk::Math::CastWithRangeCheck<InputPixelType>( backgroundValue ) );
  filter->SetBirthThreshold( birthThreshold );
  filter->SetSurvivalThreshold( survivalThreshold );

  TEST_SET_GET_VALUE( R, filter->GetRadius() );
  TEST_SET_GET_VALUE( itk::Math::CastWithRangeCheck<InputPixelType>( foregroundValue ), filter->GetForegroundValue() );
  TEST_SET_GET_VALUE( itk::Math::CastWithRangeCheck<InputPixelType>( backgroundValue ), filter->GetBackgroundValue() );
  TEST_SET_GET_VALUE( birthThreshold, filter->GetBirthThreshold() );
  TEST_SET_GET_VALUE( survivalThreshold, filter->GetSurvivalThreshold() );

  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( outfname );
  writer->SetNumberOfStreamDivisions( 5 );
  writer->Update();

  std::cout << filter;

  return EXIT_SUCCESS;
}

}


int itkVotingBinaryImageFilterTest(int argc, char* argv[] )
{

  if ( argc < 6 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage radius ForegroundValue BackgroundValue" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string infname = argv[1];
  const std::string outfname = argv[2];
  const unsigned int radius = atoi( argv[3] );
  const long foregroundValue = atol( argv[4] );
  const long backgroundValue = atol( argv[5] );


  itk::ImageIOBase::Pointer iobase =
    itk::ImageIOFactory::CreateImageIO( infname.c_str(), itk::ImageIOFactory::ReadMode);

  if ( iobase.IsNull() )
    {
    itkGenericExceptionMacro( "Unable to determine ImageIO reader for \"" << infname << "\"" );
    }


  //const itk::ImageIOBase::IOPixelType pixelType = iobase->GetPixelType();
  const itk::ImageIOBase::IOComponentType componentType = iobase->GetComponentType();
  const unsigned int dimension = iobase->GetNumberOfDimensions();

  typedef itk::Image<short,3>                                     TestImageType;
  typedef itk::VotingBinaryImageFilter<TestImageType, TestImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();
  EXERCISE_BASIC_OBJECT_METHODS( filter, VotingBinaryImageFilter, ImageToImageFilter );

  switch(componentType)
    {
    case itk::ImageIOBase::CHAR:
    case itk::ImageIOBase::UCHAR:
    case itk::ImageIOBase::SHORT:
      if ( dimension == 2 )
        return itkVotingBinaryImageFilterTestImp< itk::Image<short, 2> >( infname, outfname, radius, foregroundValue, backgroundValue );
      else if ( dimension == 3 )
        return itkVotingBinaryImageFilterTestImp< itk::Image<short, 3> >( infname, outfname, radius, foregroundValue, backgroundValue );
      break;
    case itk::ImageIOBase::USHORT:
    case itk::ImageIOBase::INT:
      if ( dimension == 2 )
        return itkVotingBinaryImageFilterTestImp< itk::Image<int, 2> >( infname, outfname, radius, foregroundValue, backgroundValue );
      else if ( dimension == 3 )
        return itkVotingBinaryImageFilterTestImp< itk::Image<int, 3> >( infname, outfname, radius, foregroundValue, backgroundValue );
      break;
    case itk::ImageIOBase::UINT:
      if ( dimension == 2 )
        return itkVotingBinaryImageFilterTestImp< itk::Image<unsigned int, 2> >( infname, outfname, radius, foregroundValue, backgroundValue );
      else if ( dimension == 3 )
        return itkVotingBinaryImageFilterTestImp< itk::Image<unsigned int, 3> >( infname, outfname, radius, foregroundValue, backgroundValue );
      break;
    case itk::ImageIOBase::ULONG:
    case itk::ImageIOBase::LONG:
    case itk::ImageIOBase::ULONGLONG:
    case itk::ImageIOBase::LONGLONG:
    case itk::ImageIOBase::FLOAT:
    case itk::ImageIOBase::DOUBLE:
    case itk::ImageIOBase::UNKNOWNCOMPONENTTYPE:
    default:
      itkGenericExceptionMacro( "Input image is a real, long, long long, or an unknown component type" );
    }

  std::cerr << "Unexcpected program flow!" << std::endl;
  return EXIT_FAILURE;
}
