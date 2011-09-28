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
#include "itkImageToImageObjectMetric.h"
#include "itkDisplacementFieldTransform.h"
#include "itkTranslationTransform.h"
#include "vnl/vnl_math.h"
#include "itkIntTypes.h"
#include "itkTestingMacros.h"

//We need this as long as we have to define ImageToData as a fwd-declare
// in itkImageToImageObjectMetric.h
#include "itkImageToData.h"

/*
 * This test creates synthetic images and verifies numerical results
 * of metric evaluation.
 *
 * TODO
 * Test with displacement field for fixed image transform.
 * Test evaluating over sub-region, maybe with non-identity tx's.
 * Test assigning different virtual image.
 * Test mask
 * Test with non-identity transforms
 * Exercise other methods
 */

template<class TFixedImage,class TMovingImage,class TVirtualImage>
class ImageToImageObjectMetricTestMetric
  : public itk::ImageToImageObjectMetric<TFixedImage,
                                         TMovingImage,
                                         TVirtualImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageObjectMetricTestMetric                  Self;
  typedef itk::ImageToImageObjectMetric<TFixedImage, TMovingImage,
                                        TVirtualImage>        Superclass;
  typedef itk::SmartPointer<Self>                             Pointer;
  typedef itk::SmartPointer<const Self>                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageObjectMetricTestMetric, ImageToImageObjectMetric);

  /** superclass types */
  typedef typename Superclass::MeasureType                MeasureType;
  typedef typename Superclass::DerivativeType             DerivativeType;
  typedef typename Superclass::VirtualPointType           VirtualPointType;
  typedef typename Superclass::FixedImagePointType        FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType        FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType
                                                      FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType   MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType   MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType
                                                      MovingImageGradientType;

  /* Implement pure virtual methods */
  void Initialize() throw ( itk::ExceptionObject )
  {
    //Be sure to call base class initialize
    Superclass::Initialize();

    //Now do your own initialization here
  }

  /* Provide the worker routine to process each point */
  bool GetValueAndDerivativeProcessPoint(
                    const VirtualPointType &,
                    const FixedImagePointType &,
                    const FixedImagePixelType &        fixedImageValue,
                    const FixedImageGradientType &  fixedImageGradient,
                    const MovingImagePointType &,
                    const MovingImagePixelType &       movingImageValue,
                    const MovingImageGradientType & movingImageGradient,
                    MeasureType &                      metricValueResult,
                    DerivativeType &                   localDerivativeReturn,
                    const itk::ThreadIdType ) const
  {
    /* Just return some test values that can verify proper mechanics */
    metricValueResult = fixedImageValue + movingImageValue;

    for ( unsigned int par = 0;
          par < this->GetNumberOfLocalParameters(); par++ )
      {
      double sum = 0.0;
      for ( unsigned int dim = 0;
            dim < Superclass::MovingImageDimension; dim++ )
        {
        sum += movingImageGradient[dim] + fixedImageGradient[dim];
        }
      localDerivativeReturn[par] = sum;
      }
    //  std::cout << " localDerivativeReturn: " << localDerivativeReturn
    //            << std::endl;

    // Return true if the point was used in evaluation
    return true;
  }

  //This is of one two evaluation methods that the user may call.
  MeasureType GetValue() const
  {
    //TODO
    itkExceptionMacro("GetValue not yet implemented.");
  }

  //This is of one two evaluation methods that the user may call.
  void GetValueAndDerivative( MeasureType & valueReturn,
                              DerivativeType & derivativeReturn) const
  {
    //1) Do any pre-processing required for your metric. To help with
    // threading, you can use ImageToData or Array1DToData classes,
    // or derive your own from ObjectToData.

    //2) Call GetValueAndDerivativeThreadedExecute.
    //This will iterate over virtual image region and call your
    // GetValueAndDerivativeProcessPoint method, see definition in
    // base. Results are written in 'derivativeReturn'.
    this->GetValueAndDerivativeThreadedExecute( derivativeReturn );

    //3) Optionally call GetValueAndDerivativeThreadedPostProcess for
    // default post-processing, which sums up results from each thread,
    // and optionally averages them. It then assigns the results to
    // 'value' and 'derivative', without copying in the case of 'derivative'.
    //Do your own post-processing as needed.
    this->GetValueAndDerivativeThreadedPostProcess( true /*doAverage*/ );

    //4) Return the value result. The derivative result has already been
    // written to derivativeReturn.
    valueReturn = this->GetValueResult();

    //That's it. Easy as 1, 2, 3 (and 4).
  }

protected:
  ImageToImageObjectMetricTestMetric(){};
  virtual ~ImageToImageObjectMetricTestMetric() {}
  void PrintSelf(std::ostream& stream, itk::Indent indent) const
  {
    Superclass::PrintSelf( stream, indent );
  }

private:
  //purposely not implemented
  ImageToImageObjectMetricTestMetric(const Self &);
  //purposely not implemented
  void operator=(const Self &);

}; // Metric ///////////////////////////////////////////////////

template <typename TVector>
bool ImageToImageObjectMetricTestTestArray(
                                      const TVector & v1, const TVector & v2 )
{
  bool pass=true;
  for ( unsigned int i = 0; i < v1.Size(); i++ )
    {
    const double epsilon = 1e-10;
    if( vcl_fabs( v1[i] - v2[i] ) > epsilon )
      pass=false;
    }
  return pass;
}


//Global types
const unsigned int ImageToImageObjectMetricTestImageDimensionality = 2;
typedef itk::Image< double, ImageToImageObjectMetricTestImageDimensionality >
                                      ImageToImageObjectMetricTestImageType;
typedef ImageToImageObjectMetricTestMetric<
                                        ImageToImageObjectMetricTestImageType,
                                        ImageToImageObjectMetricTestImageType,
                                        ImageToImageObjectMetricTestImageType>
                                         ImageToImageObjectMetricTestMetricType;
typedef ImageToImageObjectMetricTestMetricType::Pointer
                                      ImageToImageObjectMetricTestMetricPointer;
//
// Compute truth values for the identity-transform tests
//
void ImageToImageObjectMetricTestComputeIdentityTruthValues(
       const ImageToImageObjectMetricTestMetricPointer & metric,
       const ImageToImageObjectMetricTestImageType::Pointer & fixedImage,
       const ImageToImageObjectMetricTestImageType::Pointer & movingImage,
       ImageToImageObjectMetricTestMetricType::MeasureType& truthValue,
       ImageToImageObjectMetricTestMetricType::DerivativeType& truthDerivative )
{
  // Make sure the metric is initialized
  std::cout << "truth values: Initialize" << std::endl;
  metric->Initialize();
  // Call once to setup gradient images if applicable
  ImageToImageObjectMetricTestMetricType::MeasureType     tempValue;
  ImageToImageObjectMetricTestMetricType::DerivativeType  tempDerivative;

  std::cout << "truth values: GetValueAndDerivative" << std::endl;
  metric->GetValueAndDerivative( tempValue, tempDerivative );

  truthValue = 0;
  truthDerivative.SetSize( metric->GetNumberOfParameters() );
  truthDerivative.Fill( 0 );

  itk::ImageRegionIterator<ImageToImageObjectMetricTestImageType>
                     itFixed( fixedImage, fixedImage->GetRequestedRegion() );
  itk::ImageRegionIterator<ImageToImageObjectMetricTestImageType>
                     itMoving( movingImage, movingImage->GetRequestedRegion() );
  itFixed.GoToBegin();
  itMoving.GoToBegin();
  unsigned int count = 0;
  std::cout << "truth values: Iterate over region" << std::endl;
  while( !itFixed.IsAtEnd() && !itMoving.IsAtEnd() )
    {
    truthValue += itFixed.Get() + itMoving.Get();

    // Get the image derivatives.
    // Because this test is using identity transforms,
    // simply retrieve by index.
    // NOTE: relying on the metric's gradient image isn't a
    // complete test, but it does test the rest of the mechanics.
    ImageToImageObjectMetricTestMetricType::MovingImageGradientType
                                                          movingImageDerivative;
    ImageToImageObjectMetricTestMetricType::FixedImageGradientType
                                                          fixedImageDerivative;
    if( metric->GetUseFixedImageGradientFilter() )
      {
      ImageToImageObjectMetricTestMetricType::
        FixedImageGradientImageType::ConstPointer
                   fixedGradientImage = metric->GetFixedImageGradientImage();
      fixedImageDerivative = fixedGradientImage->GetPixel( itFixed.GetIndex() );
      }
    else
      {
      typedef ImageToImageObjectMetricTestMetricType::
                FixedImageGradientCalculatorType::ConstPointer
                                              FixedGradientCalculatorPointer;
      FixedGradientCalculatorPointer fixedGradientCalculator =
                                        metric->GetFixedImageGradientCalculator();
      fixedImageDerivative =
              fixedGradientCalculator->EvaluateAtIndex( itFixed.GetIndex() );
      // We can skip the call to TransformCovariantVector since we're
      // working with identity transforms only.
      }
    if( metric->GetUseMovingImageGradientFilter() )
      {
      ImageToImageObjectMetricTestMetricType::
        MovingImageGradientImageType::ConstPointer
              movingGradientImage = metric->GetMovingImageGradientImage();
      movingImageDerivative =
                        movingGradientImage->GetPixel( itMoving.GetIndex() );
      }
    else
      {
      typedef ImageToImageObjectMetricTestMetricType::
        MovingImageGradientCalculatorType::ConstPointer
                                              MovingGradientCalculatorPointer;
      MovingGradientCalculatorPointer     movingGradientCalculator;
      movingGradientCalculator = metric->GetMovingImageGradientCalculator();
      movingImageDerivative =
              movingGradientCalculator->EvaluateAtIndex( itMoving.GetIndex() );
      }

    /*
    std::cout << "Truth: " << itMoving.GetIndex() << ": movingImageDerivative:"
              << movingImageDerivative << std::endl
              << "Truth: " << itFixed.GetIndex() << ": fixedImageDerivative: "
              << fixedImageDerivative << std::endl;
    */

    for ( unsigned int par = 0;
          par < metric->GetNumberOfLocalParameters(); par++ )
      {
      double sum = 0.0;
      for ( unsigned int dim = 0;
              dim < ImageToImageObjectMetricTestImageDimensionality; dim++ )
        {
        sum += movingImageDerivative[dim] + fixedImageDerivative[dim];
        }

      if( metric->HasLocalSupport() )
        {
        truthDerivative[ count * metric->GetNumberOfLocalParameters() + par ]
                                                                        = sum;
        }
      else
        {
        truthDerivative[par] += sum;
        }
      }
    count++;
    ++itFixed;
    ++itMoving;
    }

  // Take the averages
  truthValue /= metric->GetNumberOfValidPoints();
  if( ! metric->HasLocalSupport() )
    {
    truthDerivative /= metric->GetNumberOfValidPoints();
    }
}

////////////////////////////////////////////////////////////
//
// Pass true for 'setTruthValues' to have the results of the metric
// call set the return values of truthValue and truthDerivative.
// Useful for establishing a relative truth for multiple runs.
// Otherwise, this will compare the results of calling the metric
// with truthValue and truthDerivative.
int ImageToImageObjectMetricTestRunSingleTest(
       const ImageToImageObjectMetricTestMetricPointer & metric,
       ImageToImageObjectMetricTestMetricType::MeasureType& truthValue,
       ImageToImageObjectMetricTestMetricType::DerivativeType& truthDerivative,
       itk::SizeValueType expectedNumberOfPoints,
       bool setTruthValues )
{
  std::cout << "Pre-warp image: fixed, moving: "
            << metric->GetDoFixedImagePreWarp() << ", "
            << metric->GetDoMovingImagePreWarp() << std::endl
            << "Use gradient filter for: fixed, moving: "
            << metric->GetUseFixedImageGradientFilter()
            << ", "
            << metric->GetUseMovingImageGradientFilter()
            << std::endl;

  // Initialize.
  try
    {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cout << "Caught unexpected exception during Initialize: "
              << exc;
    return EXIT_FAILURE;
    }

  // Evaluate
  ImageToImageObjectMetricTestMetricType::MeasureType valueReturn;
  ImageToImageObjectMetricTestMetricType::DerivativeType derivativeReturn;
  try
    {
    metric->GetValueAndDerivative( valueReturn, derivativeReturn );
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cout << "Caught unexpected exception during GetValueAndDerivative: "
              << exc;
    return EXIT_FAILURE;
    }

  //Check number of threads and valid points
  std::cout << "--Number of threads used: "
            << metric->GetNumberOfThreads() << std::endl;
  if( metric->GetNumberOfValidPoints() != ( expectedNumberOfPoints ) )
    {
    std::cout << "Expected number of valid points to be "
              << expectedNumberOfPoints
              << " but instead got " << metric->GetNumberOfValidPoints()
              << std::endl;
    return EXIT_FAILURE;
    }

  // Return or verify results
  int result = EXIT_SUCCESS;
  if( setTruthValues )
    {
    truthValue = valueReturn;
    truthDerivative = derivativeReturn;
    }
  else
    {
    // Verify results
    const double epsilon = 1e-10;
    if( vcl_fabs( truthValue - valueReturn ) > epsilon )
      {
      std::cout << "-FAILED- truthValue does not equal value: " << std::endl
                << "truthValue: " << truthValue << std::endl
                << "value: " << valueReturn << std::endl;
      result = EXIT_FAILURE;

      }
    if( ! ImageToImageObjectMetricTestTestArray(
                                          truthDerivative, derivativeReturn ) )
      {
      std::cout << "-FAILED- truthDerivative does not equal derivatives:"
                << std::endl
                << "truthDerivative: " << truthDerivative << std::endl
                << "derivatives: " << derivativeReturn << std::endl;
      result = EXIT_FAILURE;
      }
    }
  return result;
}

////////////////////////////////////////////////////////////
int itkImageToImageObjectMetricTest(int, char ** const)
{
  bool origGlobalWarningValue = itk::Object::GetGlobalWarningDisplay();
  itk::Object::SetGlobalWarningDisplay( true );

  const unsigned int imageSize = 10;

  int result = EXIT_SUCCESS;

  ImageToImageObjectMetricTestImageType::SizeType       size = {{imageSize, imageSize}};
  ImageToImageObjectMetricTestImageType::IndexType      index = {{0,0}};
  ImageToImageObjectMetricTestImageType::RegionType     region;
  region.SetSize( size );
  region.SetIndex( index );
  ImageToImageObjectMetricTestImageType::SpacingType    spacing;
  spacing.Fill(1.0);
  ImageToImageObjectMetricTestImageType::PointType      origin;
  origin.Fill(0);
  ImageToImageObjectMetricTestImageType::DirectionType  direction;
  direction.SetIdentity();

  // Create simple test images.
  ImageToImageObjectMetricTestImageType::Pointer fixedImage =
                                  ImageToImageObjectMetricTestImageType::New();
  fixedImage->SetRegions( region );
  fixedImage->SetSpacing( spacing );
  fixedImage->SetOrigin( origin );
  fixedImage->SetDirection( direction );
  fixedImage->Allocate();

  ImageToImageObjectMetricTestImageType::Pointer movingImage =
                                  ImageToImageObjectMetricTestImageType::New();
  movingImage->SetRegions( region );
  movingImage->SetSpacing( spacing );
  movingImage->SetOrigin( origin );
  movingImage->SetDirection( direction );
  movingImage->Allocate();

  // Fill images
  itk::ImageRegionIterator<ImageToImageObjectMetricTestImageType>
                                                  itFixed( fixedImage, region );
  itFixed.GoToBegin();
  unsigned int count = 1;
  while( !itFixed.IsAtEnd() )
    {
    itFixed.Set( count * count );
    count++;
    ++itFixed;
    }
  itk::ImageRegionIteratorWithIndex<ImageToImageObjectMetricTestImageType>
                                                itMoving( movingImage, region );
  itMoving.GoToBegin();
  count = 1;
  while( !itMoving.IsAtEnd() )
    {
    itMoving.Set( count * count / 2.0 );
    count++;
    ++itMoving;
    }

  // Transforms
  typedef itk::TranslationTransform<double,
                               ImageToImageObjectMetricTestImageDimensionality>
                                                            FixedTransformType;
  typedef itk::TranslationTransform<double,
                               ImageToImageObjectMetricTestImageDimensionality>
                                                            MovingTransformType;
  FixedTransformType::Pointer fixedTransform = FixedTransformType::New();
  MovingTransformType::Pointer movingTransform = MovingTransformType::New();
  fixedTransform->SetIdentity();
  movingTransform->SetIdentity();

  // The simplistic test metric
  ImageToImageObjectMetricTestMetricPointer
                        metric = ImageToImageObjectMetricTestMetricType::New();

  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );
  // Tell the metric to compute image gradients for both fixed and moving.
  metric->SetGradientSource(
                ImageToImageObjectMetricTestMetricType::GRADIENT_SOURCE_BOTH );

  // Enable ITK debugging output
  metric->SetDebug( false );

  // Evaluate the metric and verify results, using identity transforms.
  // Test with different numbers of threads.
  // Run through all the permutations of pre-warping and image gradient
  // calculation method.
  ImageToImageObjectMetricTestMetricType::MeasureType     truthValue;
  ImageToImageObjectMetricTestMetricType::DerivativeType  truthDerivative;
  for( itk::ThreadIdType numberOfThreads = 1; numberOfThreads < 6;
                                                            numberOfThreads++ )
    {
    metric->SetNumberOfThreads( numberOfThreads );
    for( char useMovingFilter = 1;
            useMovingFilter >= 0; useMovingFilter-- )
      {
        for( char useFixedFilter = 1;
                useFixedFilter >= 0; useFixedFilter-- )
        {
        //Have to recompute new truth values for each permutation of
        // image gradient calculation options.
        bool computeNewTruthValues = true;
        for( char preWarpFixed = 1; preWarpFixed >= 0; preWarpFixed-- )
          {
          for( char preWarpMoving = 1; preWarpMoving >= 0; preWarpMoving-- )
            {
            metric->SetDoFixedImagePreWarp( preWarpFixed == 1 );
            metric->SetDoMovingImagePreWarp( preWarpMoving == 1 );
            metric->SetUseFixedImageGradientFilter( useFixedFilter == 1 );
            metric->SetUseMovingImageGradientFilter( useMovingFilter == 1 );
            if( computeNewTruthValues )
              {
              ImageToImageObjectMetricTestComputeIdentityTruthValues(
                                          metric, fixedImage, movingImage,
                                          truthValue, truthDerivative );
              }
            std::cout << "* Testing with identity transforms..."
                      << std::endl;
            if( ImageToImageObjectMetricTestRunSingleTest(
                                metric, truthValue, truthDerivative,
                                imageSize * imageSize, false )
                                                              != EXIT_SUCCESS )
              {
              result = EXIT_FAILURE;
              }
            computeNewTruthValues = false;
            }
          }
        }
      } // loop through permutations
    } // loop thru # of threads

  //
  // Test with an identity displacement field transform for moving image
  //

  // Create a displacement field transform
  typedef itk::DisplacementFieldTransform<double,
                              ImageToImageObjectMetricTestImageDimensionality>
                                                      DisplacementTransformType;
  DisplacementTransformType::Pointer displacementTransform =
      DisplacementTransformType::New();
  typedef DisplacementTransformType::DisplacementFieldType FieldType;
  FieldType::Pointer field = FieldType::New(); //This is based on itk::Image

  FieldType::SizeType defsize;
  FieldType::IndexType start;
  FieldType::RegionType defregion;
  defsize.Fill( imageSize );
  start.Fill( 0 );
  defregion.SetSize( defsize );
  defregion.SetIndex( start );
  field->SetRegions( defregion );
  field->Allocate();
  // Fill it with 0's
  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  field->FillBuffer( zeroVector );
  // Assign to transform
  displacementTransform->SetDisplacementField( field );

  // Assign it to the metric
  metric->SetMovingTransform( displacementTransform );

  fixedTransform->SetIdentity();
  metric->SetFixedTransform( fixedTransform );

  metric->SetDoFixedImagePreWarp( true );
  metric->SetDoMovingImagePreWarp( true );
  metric->SetUseFixedImageGradientFilter( true );
  metric->SetUseMovingImageGradientFilter( true );
  // Tell the metric to compute image gradients for both fixed and moving.
  metric->SetGradientSource(
                ImageToImageObjectMetricTestMetricType::GRADIENT_SOURCE_BOTH );

  //Evaluate the metric
  std::cout
    << "* Testing with identity DisplacementFieldTransform for moving image..."
    << std::endl;
  ImageToImageObjectMetricTestComputeIdentityTruthValues(
                                                metric, fixedImage, movingImage,
                                                truthValue, truthDerivative );
  if( ImageToImageObjectMetricTestRunSingleTest( metric,
                      truthValue, truthDerivative,
                      imageSize * imageSize, false ) != EXIT_SUCCESS )
    {
    result = EXIT_FAILURE;
    }

  // Test that using a displacemet field that does not match the virtual
  // domain space will throw an exception.
  field->SetSpacing( fixedImage->GetSpacing() * -1.0 );
  std::cout << "Testing with displacement field in different space than "
            << "fixed image:" << std::endl;
  TRY_EXPECT_EXCEPTION( metric->Initialize() );

  //exercise PrintSelf
  metric->Print( std::cout );

  itk::Object::SetGlobalWarningDisplay( origGlobalWarningValue );

  return result;
}
