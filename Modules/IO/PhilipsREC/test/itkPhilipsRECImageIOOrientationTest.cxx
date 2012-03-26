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
#include "itkImage.h"
#include "itkAffineTransform.h"
#include "itkResampleImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkSubtractImageFilter.h"

#include "itkPhilipsRECImageIO.h"

int itkPhilipsRECImageIOOrientationTest( int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] << " ReferenceImage TargetImage ";
    std::cerr << "OutputImage" << std::endl;
    return EXIT_FAILURE;
    }

  typedef short                                 PixelType;
  typedef double                                ScalarType;
  typedef itk::Image< PixelType, 3 >            ImageType;
  typedef itk::PhilipsRECImageIO                PhilipsRECImageIOType;

  typedef itk::ImageFileReader< ImageType >     ReaderType;
  typedef itk::ImageFileWriter< ImageType >     WriterType;

  typedef itk::AffineTransform< ScalarType, 3 > AffineTransformType;
  typedef itk::ResampleImageFilter< ImageType, ImageType, ScalarType >
                                                ResampleImageFilterType;
  typedef itk::SubtractImageFilter< ImageType, ImageType, ImageType >
                                                SubtractImageFilterType;
  typedef itk::NearestNeighborInterpolateImageFunction< ImageType, ScalarType >
                                                NearestInterpType;

  ReaderType::Pointer referenceReader = ReaderType::New();
  referenceReader->SetImageIO(PhilipsRECImageIOType::New());
  referenceReader->SetFileName( argv[1] );

  ReaderType::Pointer targetReader = ReaderType::New();
  targetReader->SetImageIO(PhilipsRECImageIOType::New());
  targetReader->SetFileName( argv[2] );

  AffineTransformType::Pointer transform = AffineTransformType::New();
  transform->SetIdentity();

  ResampleImageFilterType::Pointer resample = ResampleImageFilterType::New();
  resample->SetInput( referenceReader->GetOutput() );
  resample->SetTransform( transform );
  resample->SetInterpolator( NearestInterpType::New() );
  resample->UseReferenceImageOn();
  resample->SetReferenceImage( targetReader->GetOutput() );

  SubtractImageFilterType::Pointer subtract = SubtractImageFilterType::New();
  subtract->SetInput1( targetReader->GetOutput() );
  subtract->SetInput2( resample->GetOutput() );

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( subtract->GetOutput() );
  writer->UseCompressionOn();

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
