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

#include "itkAffineTransform.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkNearestNeighborExtrapolateImageFunction.h"
#include "itkTestingMacros.h"

/* Further testing of itkResampleImageFilter
 * Output is compared with baseline image using the cmake itk_add_test
 * '--compare' option.
 */

namespace {

template<typename TCoordRepType, unsigned int NDimensions>
class NonlinearAffineTransform:
  public itk::AffineTransform<TCoordRepType,NDimensions>
{
public:
  /** Standard class typedefs.   */
  typedef NonlinearAffineTransform                           Self;
  typedef itk::AffineTransform< TCoordRepType, NDimensions > Superclass;
  typedef itk::SmartPointer< Self >                          Pointer;
  typedef itk::SmartPointer< const Self >                    ConstPointer;

  /** New macro for creation of through a smart pointer. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonlinearAffineTransform, AffineTransform);

  /** Override this. See test below. */
    virtual bool IsLinear() const ITK_OVERRIDE { return false; }
};
}

int itkResampleImageTest2(int argc, char * argv [] )
{

  if( argc < 5 )
    {
    std::cerr << "Missing arguments ! " << std::endl;
    std::cerr << "Usage : " << std::endl;
    std::cerr << argv[0] << "inputImage referenceImage "
              << "resampledImageLinear resampledImageNonLinear "
              << "resampledImageLinearNearestExtrapolate"
              << "resampledImageNonLinearNearestExtrapolate";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int NDimensions = 2;

  typedef unsigned char                          PixelType;
  typedef itk::Image<PixelType, NDimensions>     ImageType;
  typedef double                                 CoordRepType;

  typedef itk::AffineTransform<CoordRepType,NDimensions>
                                                 AffineTransformType;
  typedef NonlinearAffineTransform<CoordRepType,NDimensions>
                                                 NonlinearAffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>
                                                 InterpolatorType;
  typedef itk::NearestNeighborExtrapolateImageFunction<ImageType,CoordRepType>
                                                 ExtrapolatorType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();
  ReaderType::Pointer reader3 = ReaderType::New();
  ReaderType::Pointer reader4 = ReaderType::New();

  WriterType::Pointer writer1 = WriterType::New();
  WriterType::Pointer writer2 = WriterType::New();
  WriterType::Pointer writer3 = WriterType::New();
  WriterType::Pointer writer4 = WriterType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );
  reader3->SetFileName( argv[3] );
  reader4->SetFileName( argv[4] );

  writer1->SetFileName( argv[3] );
  writer2->SetFileName( argv[4] );
  writer3->SetFileName( argv[5] );
  writer4->SetFileName( argv[6] );

  // Create an affine transformation
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->Scale(2.0);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  // Create a nearest neighbor extrapolate image function
  ExtrapolatorType::Pointer extrapolator = ExtrapolatorType::New();

  // Create and configure a resampling filter
  typedef itk::ResampleImageFilter< ImageType, ImageType > ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( resample, ResampleImageFilter, ImageToImageFilter );

  resample->SetInput( reader1->GetOutput() );
  TEST_SET_GET_VALUE( reader1->GetOutput(), resample->GetInput() );

  resample->SetReferenceImage( reader2->GetOutput() );
  TEST_SET_GET_VALUE( reader2->GetOutput(), resample->GetReferenceImage() );

  resample->UseReferenceImageOn();
  TEST_EXPECT_TRUE( resample->GetUseReferenceImage() );

  resample->SetTransform( affineTransform );
  TEST_SET_GET_VALUE( affineTransform, resample->GetTransform() );

  resample->SetInterpolator( interpolator );
  TEST_SET_GET_VALUE( interpolator, resample->GetInterpolator() );

  writer1->SetInput( resample->GetOutput() );

  // Check GetReferenceImage
  if( resample->GetReferenceImage() != reader2->GetOutput() )
    {
    std::cerr << "GetReferenceImage() failed ! " << std::endl;
    return EXIT_FAILURE;
    }

  // Run the resampling filter with the normal, linear, affine transform.
  // This will use ResampleImageFilter::LinearThreadedGenerateData().
  std::cout << "Test with normal AffineTransform." << std::endl;
  try
    {
    writer1->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Assign an affine transform that returns
  // false for IsLinear() instead of true, to force
  // the filter to use the NonlinearThreadedGenerateData method
  // instead of LinearThreadedGenerateData. This will test that
  // we get the same results for both methods.
  std::cout << "Test with NonlinearAffineTransform." << std::endl;
  NonlinearAffineTransformType::Pointer nonlinearAffineTransform =
                                    NonlinearAffineTransformType::New();

  nonlinearAffineTransform->Scale(2.0);
  resample->SetTransform( nonlinearAffineTransform );
  writer2->SetInput( resample->GetOutput() );
  try
    {
    writer2->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Instead of using the default pixel when sampling outside the input image,
  // we use a nearest neighbor extrapolator.
  resample->SetTransform( affineTransform );
  resample->SetExtrapolator( extrapolator );
  writer3->SetInput( resample->GetOutput() );
  std::cout << "Test with nearest neighbor extrapolator, affine transform." << std::endl;
  try
    {
    writer3->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Instead of using the default pixel when sampling outside the input image,
  // we use a nearest neighbor extrapolator.
  resample->SetTransform( nonlinearAffineTransform );
  writer4->SetInput( resample->GetOutput() );
  std::cout << "Test with nearest neighbor extrapolator, nonlinear transform." << std::endl;
  try
    {
    writer4->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Check UseReferenceImage methods
  resample->UseReferenceImageOff();
  if( resample->GetUseReferenceImage() )
    {
    std::cerr << "GetUseReferenceImage() or UseReferenceImageOff() failed ! ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  // Check UseReferenceImage methods
  resample->UseReferenceImageOn();
  if( !resample->GetUseReferenceImage() )
    {
    std::cerr << "GetUseReferenceImage() or UseReferenceImageOn() failed ! ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  // Check UseReferenceImage methods
  resample->SetUseReferenceImage( false );
  if( resample->GetUseReferenceImage() )
    {
    std::cerr << "GetUseReferenceImage() or SetUseReferenceImage() failed ! ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }


 std::cout << "Test passed." << std::endl;
 return EXIT_SUCCESS;

}
