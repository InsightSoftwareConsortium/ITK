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
#include "itkImageToImageMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkTestingMacros.h"
#include "itkMath.h"
#include "itkMath.h"

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

/** \class TestImageToImageGetValueAndDerivativeThreader
 * \brief Processes points for ImageToImageTest calculation. */
template < typename TDomainPartitioner, typename TImageToImageMetricv4 >
class TestImageToImageGetValueAndDerivativeThreader
  : public itk::ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetricv4 >
{
public:
  /** Standard class typedefs. */
  typedef TestImageToImageGetValueAndDerivativeThreader  Self;
  typedef itk::ImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetricv4 >
                                                         Superclass;
  typedef itk::SmartPointer< Self >                      Pointer;
  typedef itk::SmartPointer< const Self >                ConstPointer;

  itkTypeMacro( TestImageToImageGetValueAndDerivativeThreader,
    ImageToImageMetricv4GetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::VirtualPointType        VirtualPointType;
  typedef typename Superclass::VirtualIndexType        VirtualIndexType;
  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;

protected:
  TestImageToImageGetValueAndDerivativeThreader() { }

  /* Provide the worker routine to process each point */
  virtual bool ProcessPoint(
        const VirtualIndexType &          itkNotUsed(virtualIndex),
        const VirtualPointType &          itkNotUsed(virtualPoint),
        const FixedImagePointType &       itkNotUsed(mappedFixedPoint),
        const FixedImagePixelType &       mappedFixedPixelValue,
        const FixedImageGradientType &    mappedFixedImageGradient,
        const MovingImagePointType &      itkNotUsed(mappedMovingPoint),
        const MovingImagePixelType &      mappedMovingPixelValue,
        const MovingImageGradientType &   mappedMovingImageGradient,
        MeasureType &                     metricValueResult,
        DerivativeType &                  localDerivativeReturn,
        const itk::ThreadIdType           itkNotUsed(threadId) ) const ITK_OVERRIDE
    {
    /* Just return some test values that can verify proper mechanics */
    metricValueResult = mappedFixedPixelValue + mappedMovingPixelValue;

    //Only do derivative calculations when it is requested in the metric.
    if ( this->GetComputeDerivative() )
      {
      for ( unsigned int par = 0; par < this->m_Associate->GetNumberOfLocalParameters(); par++ )
        {
        double sum = 0.0;
        for ( unsigned int dim = 0; dim < TImageToImageMetricv4::MovingImageDimension; dim++ )
          {
          sum += mappedMovingImageGradient[dim] + mappedFixedImageGradient[dim];
          }
        localDerivativeReturn[par] = sum;
        }
      // Return true if the point was used in evaluation
      }
      return true;
    }

};


template<typename TFixedImage,typename TMovingImage,typename TVirtualImage>
class ImageToImageMetricv4TestMetric
  : public itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageMetricv4TestMetric                      Self;
  typedef itk::ImageToImageMetricv4<TFixedImage, TMovingImage,
                                        TVirtualImage>        Superclass;
  typedef itk::SmartPointer<Self>                             Pointer;
  typedef itk::SmartPointer<const Self>                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageMetricv4TestMetric, ImageToImageMetricv4);

  /** superclass types */
  typedef typename Superclass::MeasureType            MeasureType;
  typedef typename Superclass::DerivativeType         DerivativeType;
  typedef typename Superclass::VirtualPointType       VirtualPointType;
  typedef typename Superclass::FixedImagePointType    FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType    FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType
                                                      FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType   MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType   MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType
                                                      MovingImageGradientType;
  typedef typename Superclass::VirtualImageType       VirtualImageType;
  typedef typename Superclass::VirtualIndexType       VirtualIndexType;
  typedef typename Superclass::VirtualPointSetType
                                                      VirtualPointSetType;

  itkStaticConstMacro(VirtualImageDimension, typename TVirtualImage::ImageDimensionType, TVirtualImage::ImageDimension);
  itkStaticConstMacro(MovingImageDimension,  typename TMovingImage::ImageDimensionType,  TMovingImage::ImageDimension);

protected:
  friend class TestImageToImageGetValueAndDerivativeThreader<itk::ThreadedImageRegionPartitioner< VirtualImageDimension >, Superclass >;
  friend class TestImageToImageGetValueAndDerivativeThreader<itk::ThreadedIndexedContainerPartitioner, Superclass >;

  typedef TestImageToImageGetValueAndDerivativeThreader<itk::ThreadedImageRegionPartitioner< VirtualImageDimension >, Superclass > DenseThreaderType;
  typedef TestImageToImageGetValueAndDerivativeThreader<itk::ThreadedIndexedContainerPartitioner, Superclass >  SparseThreaderType;

  ImageToImageMetricv4TestMetric()
    {
    /* We need threader object instances. */
    this->m_DenseGetValueAndDerivativeThreader  = DenseThreaderType::New();
    this->m_SparseGetValueAndDerivativeThreader = SparseThreaderType::New();
    }
  virtual ~ImageToImageMetricv4TestMetric() ITK_OVERRIDE {}

  void PrintSelf(std::ostream& stream, itk::Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf( stream, indent );
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToImageMetricv4TestMetric);

}; // Metric ///////////////////////////////////////////////////

template <typename TVector>
bool ImageToImageMetricv4TestTestArray(
                                      const TVector & v1, const TVector & v2 )
{
  bool pass=true;
  for ( unsigned int i = 0; i < v1.Size(); i++ )
    {
    const double epsilon = 1e-10;
    if( std::fabs( v1[i] - v2[i] ) > epsilon )
      pass=false;
    }
  return pass;
}


//Global types
const unsigned int ImageToImageMetricv4TestImageDimensionality = 2;
typedef itk::Image< double, ImageToImageMetricv4TestImageDimensionality >
                                      ImageToImageMetricv4TestImageType;
typedef ImageToImageMetricv4TestMetric<
                                        ImageToImageMetricv4TestImageType,
                                        ImageToImageMetricv4TestImageType,
                                        ImageToImageMetricv4TestImageType>
                                         ImageToImageMetricv4TestMetricType;
typedef ImageToImageMetricv4TestMetricType::Pointer
                                      ImageToImageMetricv4TestMetricPointer;
//
// Compute truth values for the identity-transform tests
//
void ImageToImageMetricv4TestComputeIdentityTruthValues(
       const ImageToImageMetricv4TestMetricPointer & metric,
       const ImageToImageMetricv4TestImageType::Pointer & fixedImage,
       const ImageToImageMetricv4TestImageType::Pointer & movingImage,
       ImageToImageMetricv4TestMetricType::MeasureType& truthValue,
       ImageToImageMetricv4TestMetricType::DerivativeType& truthDerivative )
{
  // Make sure the metric is initialized
  std::cout << "truth values: Initialize" << std::endl;
  metric->Initialize();
  // Call once to setup gradient images if applicable
  ImageToImageMetricv4TestMetricType::MeasureType     tempValue;
  ImageToImageMetricv4TestMetricType::DerivativeType  tempDerivative;

  metric->GetValueAndDerivative( tempValue, tempDerivative );

  // Determine truth values
  std::cout << "truth values: GetValueAndDerivative" << std::endl;
  truthValue = 0;
  truthDerivative.SetSize( metric->GetNumberOfParameters() );
  truthDerivative.Fill( 0 );

  typedef ImageToImageMetricv4TestMetricType::MovingTransformType MovingTransformType;

  itk::ImageRegionIterator<ImageToImageMetricv4TestImageType>
                     itFixed( fixedImage, fixedImage->GetRequestedRegion() );
  itk::ImageRegionIterator<ImageToImageMetricv4TestImageType>
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
    ImageToImageMetricv4TestMetricType::MovingImageGradientType movingImageDerivative;
    ImageToImageMetricv4TestMetricType::FixedImageGradientType fixedImageDerivative;
    if( metric->GetUseFixedImageGradientFilter() )
      {
      ImageToImageMetricv4TestMetricType::FixedImageGradientImageType::ConstPointer fixedGradientImage = metric->GetFixedImageGradientImage();
      fixedImageDerivative = fixedGradientImage->GetPixel( itFixed.GetIndex() );
      }
    else
      {
      typedef ImageToImageMetricv4TestMetricType::FixedImageGradientCalculatorType::ConstPointer FixedGradientCalculatorPointer;
      FixedGradientCalculatorPointer fixedGradientCalculator = metric->GetFixedImageGradientCalculator();
      ImageToImageMetricv4TestMetricType::FixedImagePointType point;
      fixedImage->TransformIndexToPhysicalPoint( itFixed.GetIndex(), point );
      fixedImageDerivative = fixedGradientCalculator->Evaluate( point );
      // We can skip the call to TransformCovariantVector since we're
      // working with identity transforms only.
      }
    if( metric->GetUseMovingImageGradientFilter() )
      {
      ImageToImageMetricv4TestMetricType::MovingImageGradientImageType::ConstPointer movingGradientImage = metric->GetMovingImageGradientImage();
      movingImageDerivative = movingGradientImage->GetPixel( itMoving.GetIndex() );
      }
    else
      {
      typedef ImageToImageMetricv4TestMetricType::MovingImageGradientCalculatorType::ConstPointer MovingGradientCalculatorPointer;
      MovingGradientCalculatorPointer     movingGradientCalculator;
      movingGradientCalculator = metric->GetMovingImageGradientCalculator();
      ImageToImageMetricv4TestMetricType::FixedImagePointType point;
      movingImage->TransformIndexToPhysicalPoint( itMoving.GetIndex(), point );
      movingImageDerivative = movingGradientCalculator->Evaluate( point );
      }

    for ( unsigned int par = 0;
          par < metric->GetNumberOfLocalParameters(); par++ )
      {
      double sum = 0.0;
      for ( unsigned int dim = 0;
              dim < ImageToImageMetricv4TestImageDimensionality; dim++ )
        {
        sum += movingImageDerivative[dim] + fixedImageDerivative[dim];
        }

      if( metric->GetMovingTransform()->GetTransformCategory() == MovingTransformType::DisplacementField )
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
  if( metric->GetMovingTransform()->GetTransformCategory() != MovingTransformType::DisplacementField )
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
int ImageToImageMetricv4TestRunSingleTest(
       const ImageToImageMetricv4TestMetricPointer & metric,
       ImageToImageMetricv4TestMetricType::MeasureType& truthValue,
       ImageToImageMetricv4TestMetricType::DerivativeType& truthDerivative,
       itk::SizeValueType expectedNumberOfPoints,
       bool setTruthValues )
{
  int result = EXIT_SUCCESS;

  ImageToImageMetricv4TestMetricType::MeasureType valueReturn1, valueReturn2;
  ImageToImageMetricv4TestMetricType::DerivativeType derivativeReturn;

  // Initialize.
  try
    {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during Initialize: "
              << exc;
    return EXIT_FAILURE;
    }

  // Evaluate using GetValue
  try
    {
    valueReturn1 = metric->GetValue();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during GetValue: "
              << exc;
    return EXIT_FAILURE;
    }

  // Re-initialize.
  try
    {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during re-initialize: "
              << exc;
    return EXIT_FAILURE;
    }
  // Evaluate using GetValueAndDerivative
  try
    {
    metric->GetValueAndDerivative( valueReturn2, derivativeReturn );
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during GetValueAndDerivative: "
              << exc;
    return EXIT_FAILURE;
    }

  // Test same value returned by different methods
  std::cout << "Check Value return values..." << std::endl;
  if( itk::Math::NotExactlyEquals(valueReturn1, valueReturn2) )
    {
    std::cerr << "Results for Value don't match: " << valueReturn1
              << ", " << valueReturn2 << std::endl;
    result = EXIT_FAILURE;
    }

  // Check number of threads and valid points
  std::cout << "--Number of threads used: "
            << metric->GetNumberOfThreadsUsed() << std::endl;
  if( metric->GetNumberOfValidPoints() != ( expectedNumberOfPoints ) )
    {
    std::cerr << "Expected number of valid points to be "
              << expectedNumberOfPoints
              << " but instead got " << metric->GetNumberOfValidPoints()
              << std::endl;
    return EXIT_FAILURE;
    }

  // Return or verify results
  if( setTruthValues )
    {
    truthValue = valueReturn2;
    truthDerivative = derivativeReturn;
    }
  else
    {
    // Verify results
    const double epsilon = 1e-10;
    if( std::fabs( truthValue - valueReturn2 ) > epsilon )
      {
      std::cerr << "-FAILED- truthValue does not equal value: " << std::endl
                << "truthValue: " << truthValue << std::endl
                << "value: " << valueReturn2 << std::endl;
      result = EXIT_FAILURE;

      }
    if( ! ImageToImageMetricv4TestTestArray( truthDerivative, derivativeReturn ) )
      {
      std::cerr << "-FAILED- truthDerivative does not equal derivatives:"
                << std::endl
                << "truthDerivative: " << truthDerivative << std::endl
                << "derivatives: " << derivativeReturn << std::endl;
      result = EXIT_FAILURE;
      }
    }
  return result;
}

////////////////////////////////////////////////////////////
int itkImageToImageMetricv4Test(int, char ** const)
{
  bool origGlobalWarningValue = itk::Object::GetGlobalWarningDisplay();
  itk::Object::SetGlobalWarningDisplay( true );

  typedef unsigned int    DimensionSizeType;
  const DimensionSizeType imageSize = 4;

  ImageToImageMetricv4TestImageType::SizeType       size = {{imageSize, imageSize}};
  ImageToImageMetricv4TestImageType::IndexType      index = {{0,0}};
  ImageToImageMetricv4TestImageType::RegionType     region;
  region.SetSize( size );
  region.SetIndex( index );
  ImageToImageMetricv4TestImageType::SpacingType    spacing;
  spacing.Fill(1.0);
  ImageToImageMetricv4TestImageType::PointType      origin;
  origin.Fill(0);
  ImageToImageMetricv4TestImageType::DirectionType  direction;
  direction.SetIdentity();

  // Create simple test images.
  ImageToImageMetricv4TestImageType::Pointer fixedImage =
                                  ImageToImageMetricv4TestImageType::New();
  fixedImage->SetRegions( region );
  fixedImage->SetSpacing( spacing );
  fixedImage->SetOrigin( origin );
  fixedImage->SetDirection( direction );
  fixedImage->Allocate();

  ImageToImageMetricv4TestImageType::Pointer movingImage =
                                  ImageToImageMetricv4TestImageType::New();
  movingImage->SetRegions( region );
  movingImage->SetSpacing( spacing );
  movingImage->SetOrigin( origin );
  movingImage->SetDirection( direction );
  movingImage->Allocate();

  // Fill images
  itk::ImageRegionIterator<ImageToImageMetricv4TestImageType>
                                                  itFixed( fixedImage, region );
  itFixed.GoToBegin();
  unsigned int count = 1;
  while( !itFixed.IsAtEnd() )
    {
    itFixed.Set( count * count );
    count++;
    ++itFixed;
    }
  itk::ImageRegionIteratorWithIndex<ImageToImageMetricv4TestImageType>
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
                               ImageToImageMetricv4TestImageDimensionality>
                                                            FixedTransformType;
  typedef itk::TranslationTransform<double,
                               ImageToImageMetricv4TestImageDimensionality>
                                                            MovingTransformType;
  FixedTransformType::Pointer fixedTransform = FixedTransformType::New();
  MovingTransformType::Pointer movingTransform = MovingTransformType::New();
  fixedTransform->SetIdentity();
  movingTransform->SetIdentity();

  // The simplistic test metric
  ImageToImageMetricv4TestMetricPointer
                        metric = ImageToImageMetricv4TestMetricType::New();

  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );
  // Tell the metric to compute image gradients for both fixed and moving.
  metric->SetGradientSource(
                ImageToImageMetricv4TestMetricType::GRADIENT_SOURCE_BOTH );

  // Enable ITK debugging output
  metric->SetDebug( false );

  // Evaluate the metric and verify results, using identity transforms.
  // Test with different numbers of threads.
  // Run through all the permutations image gradient calculation method.
  ImageToImageMetricv4TestMetricType::MeasureType     truthValue;
  ImageToImageMetricv4TestMetricType::DerivativeType  truthDerivative;
  for( itk::ThreadIdType numberOfThreads = 1; numberOfThreads < 6;
                                                            numberOfThreads++ )
    {
    metric->SetMaximumNumberOfThreads( numberOfThreads );
    for( signed char useMovingFilter = 1;
            useMovingFilter >= 0; --useMovingFilter )
      {
        for( signed char useFixedFilter = 1;
                useFixedFilter >= 0; --useFixedFilter )
        {
        //Have to recompute new truth values for each permutation of
        // image gradient calculation options.
        bool computeNewTruthValues = true;
        metric->SetUseFixedImageGradientFilter( useFixedFilter == 1 );
        metric->SetUseMovingImageGradientFilter( useMovingFilter == 1 );
        std::cout << "**********************************" << std::endl;
        if( computeNewTruthValues )
          {
          ImageToImageMetricv4TestComputeIdentityTruthValues( metric, fixedImage, movingImage, truthValue, truthDerivative );
          }
        std::cout << "* Testing with identity transforms..."
                  << std::endl;
        if( ImageToImageMetricv4TestRunSingleTest( metric, truthValue, truthDerivative, imageSize * imageSize, false )
                                                          != EXIT_SUCCESS )
          {
          std::cerr << "----------------------------" << std::endl
                    << "Failed for these settings: " << std::endl
                    << "Use gradient filter for: fixed, moving: "
                    << metric->GetUseFixedImageGradientFilter()
                    << ", "
                    << metric->GetUseMovingImageGradientFilter() << std::endl
                    << "----------------------------" << std::endl;
          return EXIT_FAILURE;
          }
        computeNewTruthValues = false;
        }
      } // loop through permutations
    } // loop thru # of threads


  // Test that non-overlapping images will generate a warning
  // and return max value for metric value.
  MovingTransformType::ParametersType parameters(2);
  parameters[0] = 1000;
  parameters[1] = 1000;
  movingTransform->SetParameters( parameters );
  ImageToImageMetricv4TestMetricType::MeasureType expectedMetricMax;
  expectedMetricMax = itk::NumericTraits<ImageToImageMetricv4TestMetricType::MeasureType>::max();
  std::cout << "Testing non-overlapping images. Expect a warning:" << std::endl;
  if( ImageToImageMetricv4TestRunSingleTest( metric, truthValue, truthDerivative, 0, true ) != EXIT_SUCCESS ||
      itk::Math::NotAlmostEquals( metric->GetValue(), expectedMetricMax ) )
    {
    std::cerr << "Failed testing for non-overlapping images. " << std::endl
              << "  Number of valid points: " << metric->GetNumberOfValidPoints() << std::endl
              << "  Metric value: " << metric->GetValue() << std::endl
              << "  Expected metric max value: " << expectedMetricMax << std::endl;
    return EXIT_FAILURE;
    }
  movingTransform->SetIdentity();

  //
  // Test with an identity displacement field transform for moving image
  //

  // Create a displacement field transform
  typedef itk::DisplacementFieldTransform<double,
                              ImageToImageMetricv4TestImageDimensionality>
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

  metric->SetUseFixedImageGradientFilter( true );
  metric->SetUseMovingImageGradientFilter( true );
  // Tell the metric to compute image gradients for both fixed and moving.
  metric->SetGradientSource(
                ImageToImageMetricv4TestMetricType::GRADIENT_SOURCE_BOTH );

  //Evaluate the metric
  std::cout
    << "* Testing with identity DisplacementFieldTransform for moving image..."
    << std::endl;
  ImageToImageMetricv4TestComputeIdentityTruthValues(
                                                metric, fixedImage, movingImage,
                                                truthValue, truthDerivative );
  if( ImageToImageMetricv4TestRunSingleTest( metric,
                      truthValue, truthDerivative,
                      imageSize * imageSize, false ) != EXIT_SUCCESS )
    {
    return EXIT_FAILURE;
    }

  // Test that using a displacemet field that does not match the virtual
  // domain space will throw an exception.
  field->SetSpacing( fixedImage->GetSpacing() * -1.0 );
  std::cout << "Testing with displacement field in different space than "
            << "fixed image:" << std::endl;
  TRY_EXPECT_EXCEPTION( metric->Initialize() );

  //
  // Test with sampled point-set
  //
  std::cout << "Testing with sampled point-set:" << std::endl;
  fixedTransform->SetIdentity();
  movingTransform->SetIdentity();
  metric->SetMovingTransform( movingTransform );
  metric->SetFixedTransform( fixedTransform );
  metric->SetGradientSource(
                ImageToImageMetricv4TestMetricType::GRADIENT_SOURCE_BOTH );
  metric->SetUseFixedImageGradientFilter( false );
  metric->SetUseMovingImageGradientFilter( false );

  // create a point set, size of image for basic testing
  typedef ImageToImageMetricv4TestMetricType::FixedSampledPointSetType
    PointSetType;

  typedef PointSetType::PointType     PointType;
  PointSetType::CoordRepType          testPointCoords[2];
  PointSetType::Pointer               pset(PointSetType::New());

  std::cout << "Creating point set..." << std::endl;
  DimensionSizeType ind = 0;
  for( DimensionSizeType i=0; i < imageSize; i++ )
    {
    for( DimensionSizeType j=0; j < imageSize; j++ )
      {
      testPointCoords[0] = i;
      testPointCoords[1] = j;
      pset->SetPoint(ind, PointType(testPointCoords));
      ind++;
      }
    }

  std::cout << "Setting point set..." << std::endl;
  metric->SetFixedSampledPointSet( pset );
  metric->SetUseFixedSampledPointSet( true );
  std::cout << "Testing metric outpute..." << std::endl;
  ImageToImageMetricv4TestComputeIdentityTruthValues(
                                                metric, fixedImage, movingImage,
                                                truthValue, truthDerivative );
  if( ImageToImageMetricv4TestRunSingleTest( metric,
                      truthValue, truthDerivative,
                      imageSize * imageSize, false ) != EXIT_SUCCESS )
    {
    return EXIT_FAILURE;
    }

  // exercise methods.
  metric->SetUseFloatingPointCorrection( false );
  metric->SetFloatingPointCorrectionResolution( 1 );

  //exercise PrintSelf
  std::cout << std::endl << "PrintSelf: " << std::endl;
  metric->Print( std::cout );

  itk::Object::SetGlobalWarningDisplay( origGlobalWarningValue );

  return EXIT_SUCCESS;
}
