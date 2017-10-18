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
#include "itkMath.h"
#include "itkIntTypes.h"
#include "itkTestingMacros.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkResampleImageFilter.h"
#include "itkAffineTransform.h"
#include "itkContinuousIndex.h"
#include <iomanip>
#include "itkLinearInterpolateImageFunction.h"
#include "itkImageFileWriter.h"
#include "itkImageToImageMetricv4.h"

/*
 * Test moving image gradients computed inside metricv4
 *
 * For simplicity, fixed transform is the identity transform.
 *
 * Theoretical background:
 *
 * x: virtual space, the same as fix space by default
 * y: moving space
 *
 * moving transform (T) gives a transform
 *    from the virtual domain (x) to moving domain (y), i.e.:
 *      y = T (x), x->y
 *
 * This test validates computing Dm from with and w/out gradient filter.
 *
 * Dm is the gradient of moving iamge m at y,
 * and it is the output of:
 *  TransformAndEvaluateMovingPoint
 *
 */
template<unsigned int ImageDimensionality, typename TTransform>
double itkMetricImageGradientTestRunTest( unsigned int imageSize, typename TTransform::Pointer transform, double rotation,
                              bool verbose,
                              std::string & outputPath );

namespace itk
{
/*
 * \class VanilaImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief A vanilla class of metric thread, required to implement the virtual base class
 */

template<typename TDomainPartitioner, typename TImageToImageMetric>
class VanilaImageToImageMetricv4GetValueAndDerivativeThreader
: public ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>
{
public:
  /** Standard class typedefs. */
  typedef VanilaImageToImageMetricv4GetValueAndDerivativeThreader                                     Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>  Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  itkTypeMacro( VanilaImageToImageMetricv4GetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType     DomainType;
  typedef typename Superclass::AssociateType  AssociateType;

  typedef typename Superclass::ImageToImageMetricv4Type         ImageToImageMetricv4Type;
  typedef typename Superclass::VirtualPointType                 VirtualPointType;
  typedef typename Superclass::VirtualIndexType                 VirtualIndexType;
  typedef typename Superclass::FixedImagePointType              FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType              FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType           FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType             MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType             MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType          MovingImageGradientType;
  typedef typename Superclass::MeasureType                      MeasureType;
  typedef typename Superclass::DerivativeType                   DerivativeType;
  typedef typename Superclass::DerivativeValueType              DerivativeValueType;

protected:
  VanilaImageToImageMetricv4GetValueAndDerivativeThreader()
  {
  }

  /** This function computes the local voxel-wise contribution of
   *  the metric to the global integral of the metric/derivative.
   */
  virtual bool ProcessPoint(
                             const VirtualIndexType &         itkNotUsed(virtualIndex),
                             const VirtualPointType &         itkNotUsed(virtualPoint),
                             const FixedImagePointType &      itkNotUsed(mappedFixedPoint),
                             const FixedImagePixelType &      itkNotUsed(mappedFixedPixelValue),
                             const FixedImageGradientType &   itkNotUsed(mappedFixedImageGradient),
                             const MovingImagePointType &     itkNotUsed(mappedMovingPoint),
                             const MovingImagePixelType &     itkNotUsed(mappedMovingPixelValue),
                             const MovingImageGradientType &  itkNotUsed(mappedMovingImageGradient),
                             MeasureType &                    itkNotUsed(metricValueReturn),
                             DerivativeType &                 itkNotUsed(localDerivativeReturn),
                             const ThreadIdType               itkNotUsed(threadId) ) const ITK_OVERRIDE
  {
    return false;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VanilaImageToImageMetricv4GetValueAndDerivativeThreader);
};

/* \class VanillaImageToImageMetricv4
 *
 * \brief A vanilla metric for ImageToImageMetricv4 since we need to
 * access the protected methods by adding friend class.
 */
template<typename TFixedImage, typename TMovingImage, typename TVirtualImage = TFixedImage>
class VanillaImageToImageMetricv4 : public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
{
public:
  /** Standard class typedefs. */
  typedef VanillaImageToImageMetricv4                                     Self;
  typedef ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>  Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VanillaImageToImageMetricv4, ImageToImageMetricv4);

  /** Superclass types */
  typedef typename Superclass::MeasureType    MeasureType;
  typedef typename Superclass::DerivativeType DerivativeType;

  typedef typename Superclass::FixedImagePointType    FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType    FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType FixedImageGradientType;

  typedef typename Superclass::MovingImagePointType     MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType     MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType  MovingImageGradientType;

  typedef typename Superclass::MovingTransformType        MovingTransformType;
  typedef typename Superclass::JacobianType               JacobianType;
  typedef typename Superclass::VirtualImageType           VirtualImageType;
  typedef typename Superclass::VirtualIndexType           VirtualIndexType;
  typedef typename Superclass::VirtualPointType           VirtualPointType;
  typedef typename Superclass::VirtualPointSetType        VirtualPointSetType;

  /* Image dimension accessors */
  itkStaticConstMacro(VirtualImageDimension, typename TVirtualImage::ImageDimensionType, TVirtualImage::ImageDimension);
  itkStaticConstMacro(FixedImageDimension,   typename TFixedImage::ImageDimensionType,   TFixedImage::ImageDimension);
  itkStaticConstMacro(MovingImageDimension,  typename TMovingImage::ImageDimensionType,  TMovingImage::ImageDimension);

protected:
  VanillaImageToImageMetricv4()
  {
    this->m_DenseGetValueAndDerivativeThreader = VanillaDenseGetValueAndDerivativeThreaderType::New();
    this->m_SparseGetValueAndDerivativeThreader = VanillaSparseGetValueAndDerivativeThreaderType::New();
  }

  virtual ~VanillaImageToImageMetricv4() ITK_OVERRIDE {}

  // template <unsigned int VVirtualImageDimension, typename TMovingTransformType>
  // template <>
  friend double ::itkMetricImageGradientTestRunTest<VirtualImageDimension, MovingTransformType>(
      unsigned int imageSize, typename MovingTransformType::Pointer transform, double rotation, bool verbose,
      std::string & outputPath );

  typedef VanilaImageToImageMetricv4GetValueAndDerivativeThreader<
      ThreadedImageRegionPartitioner<Superclass::VirtualImageDimension>, Superclass>
  VanillaDenseGetValueAndDerivativeThreaderType;

  typedef VanilaImageToImageMetricv4GetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner, Superclass>
  VanillaSparseGetValueAndDerivativeThreaderType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VanillaImageToImageMetricv4);
};

}

template<unsigned int ImageDimensionality, typename TTransform>
double itkMetricImageGradientTestRunTest( unsigned int imageSize, typename TTransform::Pointer transform, double rotation,
                              bool verbose,
                              std::string & outputPath )
{
  // verbose = true;

  typedef itk::Image<double, ImageDimensionality> ImageType;

  typename ImageType::SizeType size;
  size.Fill( imageSize );
  typename ImageType::IndexType virtualIndex;
  virtualIndex.Fill( 0 );
  typename ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( virtualIndex );
  typename ImageType::SpacingType spacing;
  spacing.Fill( 1.0 );
  typename ImageType::PointType origin;
  origin.Fill( 0.0 );
  typename ImageType::DirectionType direction;
  direction.SetIdentity();

  // Create simple test images.
  typename ImageType::Pointer image = ImageType::New();
  image->SetRegions( region );
  image->SetSpacing( spacing );
  image->SetOrigin( origin );
  image->SetDirection( direction );
  image->Allocate();

  // Fill images, with a border
  itk::ImageRegionIteratorWithIndex<ImageType> it( image, region );
  it.GoToBegin();
  unsigned int imageBorder = 20;
  while ( !it.IsAtEnd() )
    {
    it.Set( 0 );
    bool awayfromborder = true;
    for ( unsigned int j = 0; j < ImageDimensionality; j++ )
     {
      if ( it.GetIndex()[j] < static_cast<typename ImageType::IndexValueType>(imageBorder)
           || static_cast<unsigned int> (std::abs( static_cast<float> (it.GetIndex()[j]) - static_cast<float>(size[j]) ) ) < imageBorder )
        {
        awayfromborder = false;
        }
      }
    if ( awayfromborder )
      {
      it.Set( 1 );
      }
    ++it;
    }

  // Create a "moving" image
  typedef itk::ResampleImageFilter<ImageType, ImageType> ResampleFilterType;
  typename ResampleFilterType::Pointer resample = ResampleFilterType::New();
  resample->SetTransform( transform );
  resample->SetInput( image );
  resample->SetOutputParametersFromImage( image );
  resample->SetDefaultPixelValue( 0 );
  resample->Update();
  typename ImageType::Pointer movingImage = resample->GetOutput();

  // The inverse of the transform is what we'd be estimating
  // as the moving transform during registration
  //  typename TTransform::InverseTransformBasePointer
  //      movingTransform = transform->GetInverseTransform();

  typename TTransform::Pointer movingTransform =
      dynamic_cast<TTransform *>( transform->GetInverseTransform().GetPointer() );

  // Write out the images if requested, for debugging only
  if (false)
    {
    typedef double OutputPixelType;

    typedef itk::Image<OutputPixelType, ImageDimensionality> OutputImageType;

    typedef itk::ImageFileWriter<OutputImageType> WriterType;

    typename WriterType::Pointer writer = WriterType::New();
    // moving
    writer->SetFileName( outputPath + "_moving.nii.gz" );
    writer->SetInput( movingImage );
    writer->Update();
    }

  virtualIndex.Fill( imageBorder );

  //Image gradient from moving image
  typename ImageType::PointType virtualPoint;
  image->TransformIndexToPhysicalPoint( virtualIndex, virtualPoint );
  typename ImageType::PointType mappedPoint = movingTransform->TransformPoint( virtualPoint );

  typedef itk::VanillaImageToImageMetricv4<ImageType, ImageType> MetricType;

  // Dm: ground truth
  typename MetricType::MovingImageGradientType mappedMovingImageGradientGroundtruth;
  // Dm: will be computed from metric class
  typename MetricType::MovingImageGradientType mappedMovingImageGradient;

  // compute Dm directly from graient image
  typedef itk::CentralDifferenceImageFunction<ImageType, double> CentralDifferenceCalculatorType;
  typename CentralDifferenceCalculatorType::Pointer movingCalculator = CentralDifferenceCalculatorType::New();
  movingCalculator->UseImageDirectionOn();
  movingCalculator->SetInputImage( movingImage );
  mappedMovingImageGradientGroundtruth = movingCalculator->Evaluate( mappedPoint );

  // compute Dm using Metricv4 routine
  typename ImageType::PointType mappedMovingPoint;
  typename ImageType::PixelType mappedMovingPixelValue;
  typename MetricType::Pointer metric = MetricType::New();

  metric->SetFixedImage( image );
  metric->SetMovingImage( movingImage );
  metric->SetMovingTransform( movingTransform );

  // run 0: with gradient filter: on
  // run 1: with gradient filter: off

  double sumc = 0.0;
  for ( unsigned int i = 0; i < 2; i++ )
    {
    bool b2 = false;
    switch ( i )
      {
      case 0:
        b2 = false;
        break;
      case 1:
        b2 = true;
        break;
      }

    metric->SetUseMovingImageGradientFilter( b2 );
    metric->Initialize();

    bool b = metric->TransformAndEvaluateMovingPoint( virtualPoint, mappedMovingPoint, mappedMovingPixelValue );

    // computed explicitly as ground truth
    if ( b )
      {
      metric->ComputeMovingImageGradientAtPoint( mappedMovingPoint, mappedMovingImageGradient );

      vnl_vector_ref<double> p2 = mappedMovingImageGradient.GetVnlVector();
      vnl_vector_ref<double> p1 = mappedMovingImageGradientGroundtruth.GetVnlVector();

      double norm1 = p1.two_norm();
      double norm2 = p2.two_norm();
      if ( norm1 > 0 && norm2 > 0 )
        {
        double correlation = dot_product( p2, p1 ) / ( norm1 * norm2 );
        sumc += correlation;
        }

      if( verbose )
        {
        std::cout << "use gradient filter: " << metric->GetUseMovingImageGradientFilter() << std::endl;
        std::cout << "rotation: " << rotation << std::endl << "virtualIndex: " << virtualIndex << std::endl
            << "virtualPoint: " << virtualPoint << std::endl
            << "mappedMovingPoint: " << mappedMovingPoint << std::endl
            << "mappedMovingGradient: " << mappedMovingImageGradient << std::endl
            << "mappedMovingImageGradientGroundtruth: " << mappedMovingImageGradientGroundtruth << std::endl;
        }
      } // if (b)
    }
  return sumc / static_cast<double>(2.0); //correlation;
}

//////////////////////////////////////////////////////
int itkMetricImageGradientTest( int argc, char *argv[] )
{
  typedef unsigned int DimensionSizeType;
  DimensionSizeType imageSize = 60;
  unsigned int dimensionality = 3;
  double minimumAverage = itk::NumericTraits<double>::max();
  double rotationDegrees = static_cast<double>( 0.0 ); // (3.0);
  double maxDegrees = static_cast<double>( 359.0 );
  double degreeStep = static_cast<double>( 15.0 ); //(3.0);

  std::string outputPath( "" );
  if ( argc >= 2 )
    {
    std::string path( argv[1] );
    outputPath = path;
    }
  std::string commandName( argv[0] );
  outputPath += commandName;
  std::cout << outputPath << std::endl;

  for(dimensionality = 2; dimensionality <= 3; dimensionality++)
    {
    std::cout << "testing dimension: " << dimensionality << std::endl;
    minimumAverage = itk::NumericTraits<double>::max();
    for ( rotationDegrees = static_cast<double>( 0.0 ); rotationDegrees < maxDegrees; rotationDegrees += degreeStep )
      {

      std::cerr << std::setw( 3 );
      double average = minimumAverage;

      if ( dimensionality == 2 )
        {
        // Transform
        typedef itk::Image<double, 2> ImageType;

        typedef itk::AffineTransform<double, 2> TransformType;

        TransformType::Pointer transform = TransformType::New();
        transform->SetIdentity();

        transform->Rotate2D( itk::Math::pi * rotationDegrees / 180 );
        ImageType::PointType center;
        center.Fill( ( imageSize - 1 ) / 2.0 );
        transform->SetCenter( center );

        typedef itk::Image<double, 2> ImageType;

        typedef itk::VanillaImageToImageMetricv4<ImageType, ImageType> MetricType;

        average = itkMetricImageGradientTestRunTest<2, MetricType::MovingTransformType>( imageSize,
            MetricType::MovingTransformType::Pointer( transform ), rotationDegrees, false,
            outputPath );
        }

      if ( dimensionality == 3 )
        {
        // Transform
        typedef itk::AffineTransform<double, 3> TransformType;
        TransformType::Pointer transform = TransformType::New();
        transform->SetIdentity();
        double angleRad = itk::Math::pi * rotationDegrees / 180;
        //    transform->SetRotation( angleRad, angleRad, angleRad );
        TransformType::OutputVectorType axis1;
        axis1[0] = 1;
        axis1[1] = 0;
        axis1[2] = 0;
        TransformType::OutputVectorType axis2;
        axis2[0] = 0;
        axis2[1] = 1;
        axis2[2] = 0;
        transform->Rotate3D( axis1, angleRad );
        transform->Scale( 1.2 );
        transform->Shear( 0, 1, 0.05 );
        TransformType::ParametersType center( 3 );
        center.Fill( ( imageSize - 1 ) / 2.0 );
        transform->SetFixedParameters( center );

        typedef itk::Image<double, 3> ImageType;

        typedef itk::VanillaImageToImageMetricv4<ImageType, ImageType> MetricType;

        average = itkMetricImageGradientTestRunTest<3, MetricType::MovingTransformType>( imageSize,
            MetricType::MovingTransformType::Pointer( transform ), rotationDegrees, false,
            outputPath );
        }

      if ( average < minimumAverage )
        {
        minimumAverage = average;
        }
      std::cout << average << ", " << rotationDegrees << std::endl;
      }

    std::cout << "minimumAverage: " << minimumAverage << std::endl;
    double threshold = static_cast<double>( 0.96 );
    if ( minimumAverage < threshold )
      {
      std::cerr << "Minimum average of all runs is below threshold of " << threshold << std::endl;
      return EXIT_FAILURE;
      }

    }

  return EXIT_SUCCESS;
}
