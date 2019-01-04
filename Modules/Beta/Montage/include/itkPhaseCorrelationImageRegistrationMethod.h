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
#ifndef itkPhaseCorrelationImageRegistrationMethod_h
#define itkPhaseCorrelationImageRegistrationMethod_h

#include "itkConstantPadImageFilter.h"
#include "itkDataObjectDecorator.h"
#include "itkFrequencyHalfHermitianFFTLayoutImageRegionIteratorWithIndex.h"
#include "itkHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkImage.h"
#include "itkMirrorPadImageFilter.h"
#include "itkProcessObject.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkTranslationTransform.h"
#include "itkUnaryFrequencyDomainFilter.h"
#include <cmath>
#include <complex>

#include "itkPhaseCorrelationOperator.h"
#include "itkPhaseCorrelationOptimizer.h"

namespace itk
{
/** \class PhaseCorrelationImageRegistrationMethod
 *  \brief Base class for phase-correlation-based image registration.
 *
 *  Phase Correlation Method (PCM) estimates shift between the Fixed image and
 *  Moving image. See <em>C. D. Kuglin and D. C. Hines, The phase correlation
 *  image alignment method, in Proc. Int. Conf. on Cybernetics and Society,
 *  pp. 163-165, IEEE, Sep. 1975</em> for method description.
 *
 *  The method consists of 4 (5) steps:
 *    0. Resampling and padding the images to the same spacing and size.
 *    1. Compute FFT of the two images.
 *    2. Compute the ratio of the two spectrums.
      3. Apply Butterworth band-pass filter in frequency domain
 *    4. Compute the inverse FFT of the cross-power spectrum.
 *    5. Find the maximum peak in cross-power spectrum and estimate the shift.
 *
 *  Resampling (step 0) is not included in the method itself - it is a prerequisite of PCM.
 *  It is required that the input itk::Image's have the same Spacing and
 *  Direction. If that is not the case, resample one of the images with the
 *  ResampleImageFilter prior to applying this method.
 *
 *  This class will zero-pad the images so they have the same real size
 *  (in all dimensions) and are multiples of FFT's supported prime factors.
 *
 *  Step 1. is performed by this class too using FFT filters supplied by
 *  itk::RealToHalfHermitianForwardFFTImageFilter::New() factory.
 *
 *  Step 2. is performed by generic PhaseCorrelationOperator supplied at
 *  run-time.  PhaseCorrelationOperator can be derived to implement some special
 *  filtering during this phase.
 *
 *  As some special techniques (e.g. to compute subpixel shifts) require complex
 *  correlation surface, while the others compute the shift from real
 *  correlation surface.
 *
 *  Step 3. is there to ease registration in case of significant
 *  low and/or high frequency artifcats, such as uneven lighting or high noise.
 *
 *  Step 4. is carried by this class only when necessary.
 *  The IFFT filter is created using
 *  itk::HalfHermitianToRealInverseFFTImageFilter::New() factory.
 *
 *  Step 5. is performed with the run-time supplied PhaseCorrelationOptimizer. It has
 *  to determine the shift from the real or complex correlation surface
 *  and fixed and moving image's origins.
 *
 *
 *  First, plug in the operator, optimizer and the input images. The method
 *  is executed by calling Update() (or updating some downstream filter).
 *
 *  The output shift can be passed downstream in the form of
 *  TranslationTransform or can be obtained as transform parameters vector. The
 *  transform can be directly used to resample the Moving image to match the
 *  Fixed image.
 *
 *  This class allows caching of image FFTs, because image montaging usually
 *  requires a single tile to participate in multiple image registrations.
 *
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 * \ingroup Montage
 */
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT PhaseCorrelationImageRegistrationMethod : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN( PhaseCorrelationImageRegistrationMethod );

  /** Standard class type aliases. */
  using Self = PhaseCorrelationImageRegistrationMethod;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( PhaseCorrelationImageRegistrationMethod, ProcessObject );

  /**  Type of the Fixed image. */
  using FixedImageType = TFixedImage;
  using FixedImagePixelType = typename FixedImageType::PixelType;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;

  /**  Type of the Moving image. */
  using MovingImageType = TMovingImage;
  using MovingImagePixelType = typename MovingImageType::PixelType;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro( ImageDimension, unsigned int, FixedImageType::ImageDimension );

  /** Image and region size type. */
  using SizeType = Size< ImageDimension >;

  /** Pixel type, that will be used by internal filters.
   *  It should be float for integral and float inputs and it should
   *  be double for double inputs */
  using InternalPixelType = typename NumericTraits< FixedImagePixelType >::RealType;

  /** Type of the image, that is passed between the internal components. */
  using RealImageType = Image< InternalPixelType, ImageDimension >;

  /** Type of the image, that is passed between the internal components. */
  using ComplexConjugateImageType =
    Image< std::complex< InternalPixelType >, itkGetStaticConstMacro( ImageDimension ) >;

  /**  Type of the Operator */
  using OperatorType = PhaseCorrelationOperator< InternalPixelType, ImageDimension >;
  using OperatorPointer = typename OperatorType::Pointer;

  /**  Type of the Optimizer */
  using RealOptimizerType = PhaseCorrelationOptimizer< RealImageType >;
  using RealOptimizerPointer = typename RealOptimizerType::Pointer;
  using ComplexOptimizerType = PhaseCorrelationOptimizer< ComplexConjugateImageType >;
  using ComplexOptimizerPointer = typename ComplexOptimizerType::Pointer;

  /**  Type for the transform. */
  using TransformType = TranslationTransform< typename MovingImageType::PointType::ValueType, ImageDimension >;
  using TransformPointer = typename TransformType::Pointer;

  /** Type for the output transform parameters (the shift). */
  using ParametersType = typename TransformType::ParametersType;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  using TransformOutputType = DataObjectDecorator< TransformType >;
  using TransformOutputPointer = typename TransformOutputType::Pointer;
  using TransformOutputConstPointer = typename TransformOutputType::ConstPointer;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Set/Get the Fixed image. */
  void SetFixedImage( const FixedImageType* fixedImage );
  itkGetConstObjectMacro( FixedImage, FixedImageType );

  /** Set/Get the Moving image. */
  void SetMovingImage( const MovingImageType* movingImage );
  itkGetConstObjectMacro( MovingImage, MovingImageType );

  /** Internal FFT filter type. */
  using FFTFilterType = RealToHalfHermitianForwardFFTImageFilter< RealImageType >;

  /** Image's FFT type. */
  using ComplexImageType = typename FFTFilterType::OutputImageType;

  /** Set the fixed image's cached FFT. */
  void SetFixedImageFFT( const ComplexImageType* fixedImageFFT );

  /** Get the fixed image's FFT (useful for caching).
   *  Available after Update() has been called. */
  itkGetConstObjectMacro( FixedImageFFT, ComplexImageType );

  /** Set the moving image's cached FFT. */
  void SetMovingImageFFT( const ComplexImageType* movingImageFFT );

  /** Get the moving image's FFT (useful for caching).
   *  Available after Update() has been called. */
  itkGetConstObjectMacro( MovingImageFFT, ComplexImageType );

  /** Passes ReleaseDataFlag to internal filters. */
  void SetReleaseDataFlag( bool flag ) override;

  /** Passes ReleaseDataBeforeUpdateFlag to internal filters. */
  void SetReleaseDataBeforeUpdateFlag( const bool flag ) override;

  /** Set/Get the Operator. */
  itkSetObjectMacro( Operator, OperatorType );
  itkGetConstObjectMacro( Operator, OperatorType );

  /** Set/Get the Optimizer. */
  virtual void SetOptimizer( RealOptimizerType* );
  virtual void SetOptimizer( ComplexOptimizerType* );
  itkGetConstObjectMacro( RealOptimizer, RealOptimizerType );
  itkGetConstObjectMacro( ComplexOptimizer, ComplexOptimizerType );

  /** Given an image size, returns the smallest size
   *  which factorizes using FFT's prime factors. */
  SizeType RoundUpToFFTSize( SizeType inSize );

  /** Set/Get the PadToSize.
   *  Unset by setting a size of all zeroes.
   *
   *  If PadToSize is set, image sizes are ignored and this size is used.
   *
   *  If used in a montage, a maximum image size can be determined,
   *  RoundUpToFFTSize() called and the resulting size set as PadToSize. */
  itkSetMacro( PadToSize, SizeType );
  itkGetConstMacro( PadToSize, SizeType );

  /** Set/Get obligatory padding.
   * If set, padding of this many pixels is added on both beginning and end
   * sides of each dimension of the image. */
  itkSetMacro( ObligatoryPadding, SizeType );
  itkGetConstMacro( ObligatoryPadding, SizeType );

  /** \class PaddingMethod
   *  \brief Different methods of padding the images to satisfy FFT size requirements.
   *  \ingroup Montage */
  enum class PaddingMethod
  {
    Zero = 0,
    Mirror,
    MirrorWithExponentialDecay,
    Last = MirrorWithExponentialDecay
  };
  itkGetConstMacro( PaddingMethod, PaddingMethod );
  void SetPaddingMethod( const PaddingMethod paddingMethod );

  /** Set/Get the order for Butterworth band-pass filtering
   * of complex correlation surface. Greater than zero. Default is 3. */
  itkSetMacro( ButterworthOrder, unsigned );
  itkGetConstMacro( ButterworthOrder, unsigned );

  /** Set/Get low frequency threshold for Butterworth band-pass.
   * Expressed in Hertz. Valid range (0.0, 1.0).
   * If equal to 0.0 it means low pass filtering is disabled.*/
  virtual void SetButterworthLowFrequency( double f_Hz )
  {
    double f2 = f_Hz * f_Hz; // square of frequency
    // we save per-pixel computation by recording the square
    if ( this->m_LowFrequency2 != f2 )
      {
      this->m_LowFrequency2 = f2;
      this->Modified();
      }
  }
  virtual double GetButterworthLowFrequency()
  {
    return std::sqrt( m_LowFrequency2 );
  }

  /** Set/Get high frequency threshold for Butterworth band-pass.
   * Expressed in Hertz. Valid range (0.0, 1.0).
   * If equal to 0.0 it means high pass filtering is disabled.*/
  virtual void SetButterworthHighFrequency( double f_Hz )
  {
    double f2 = f_Hz * f_Hz; //square of frequency
    // we save per-pixel computation by recording the square
    if ( this->m_HighFrequency2 != f2 )
      {
      this->m_HighFrequency2 = f2;
      this->Modified();
      }
  }
  virtual double GetButterworthHighFrequency()
  {
    return std::sqrt( m_HighFrequency2 );
  }

  /** Get the correlation surface.
   *
   *  Use method appropriate to the type (real/complex) of optimizer. If the
   *  complex optimizer is used, the real correlation surface is not available
   *  or is not up-to-date.
   */
  virtual RealImageType* GetRealCorrelationSurface()
  {
    itkDebugMacro( "returning RealCorrelationSurface address " << this->m_IFFT->GetOutput() );
    if ( m_IFFT.IsNotNull() )
      {
      return m_IFFT->GetOutput();
      }
    else
      {
      return 0;
      }
  }
  virtual ComplexConjugateImageType* GetComplexCorrelationSurface()
  {
    itkDebugMacro( "returning ComplexCorrelationSurface address " << this->m_Operator->GetOutput() );
    if ( m_Operator.IsNotNull() )
      {
      return m_Operator->GetOutput();
      }
    else
      {
      return 0;
      }
  }

  /** Get the computed transformation parameters. */
  itkGetConstReferenceMacro( TransformParameters, ParametersType );

  /** Returns the transform resulting from the registration process  */
  const TransformOutputType* GetOutput() const;

  /** Returns the phase correlation image from the registration process  */
  const RealImageType* GetPhaseCorrelationImage() const;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkStaticConstMacro( MovingImageDimension, unsigned int, FixedImageType::ImageDimension );
  /** Start concept checking */
  itkConceptMacro( SameDimensionCheck, (Concept::SameDimension< ImageDimension, MovingImageDimension >));
#endif

protected:
  PhaseCorrelationImageRegistrationMethod();
  virtual ~PhaseCorrelationImageRegistrationMethod(){};
  void PrintSelf( std::ostream& os, Indent indent ) const override;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  DataObjectPointer MakeOutput( DataObjectPointerArraySizeType idx ) override;

  /** Initialize by setting the interconnects between the components. */
  virtual void Initialize();

  /** Determine the correct padding for the fixed image and moving image. */
  void DeterminePadding();

  /** Method that initiates the optimization process. */
  void StartOptimization();

  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  void GenerateData() override;

  /** Method invoked by the pipeline to determine the output information. */
  void GenerateOutputInformation() override;

  /** Provides derived classes with the ability to set this private var */
  itkSetMacro( TransformParameters, ParametersType );


  /** Types for internal componets. */
  using FixedPadderImageFilter = PadImageFilter< FixedImageType, RealImageType >;
  using MovingPadderImageFilter = PadImageFilter< MovingImageType, RealImageType >;
  using FixedConstantPadderType = ConstantPadImageFilter< FixedImageType, RealImageType >;
  using MovingConstantPadderType = ConstantPadImageFilter< MovingImageType, RealImageType >;
  using FixedMirrorPadderType = MirrorPadImageFilter< FixedImageType, RealImageType >;
  using MovingMirrorPadderType = MirrorPadImageFilter< MovingImageType, RealImageType >;
  using IFFTFilterType = HalfHermitianToRealInverseFFTImageFilter< ComplexImageType, RealImageType >;
  using BandBassFilterType = UnaryFrequencyDomainFilter< ComplexImageType,
    FrequencyHalfHermitianFFTLayoutImageRegionIteratorWithIndex< ComplexImageType > >;
  using FrequencyFunctorType = std::function< typename BandBassFilterType::ValueFunctionType >;

  const FrequencyFunctorType m_IdentityFunctor = []( typename BandBassFilterType::FrequencyIteratorType& ){};
  FrequencyFunctorType m_BandPassFunctor;
  FrequencyFunctorType m_HighPassFunctor;
  FrequencyFunctorType m_LowPassFunctor;

private:
  OperatorPointer         m_Operator;
  RealOptimizerPointer    m_RealOptimizer;
  ComplexOptimizerPointer m_ComplexOptimizer;

  MovingImageConstPointer m_MovingImage;
  FixedImageConstPointer  m_FixedImage;

  typename ComplexImageType::Pointer m_FixedImageFFT;
  typename ComplexImageType::Pointer m_MovingImageFFT;

  ParametersType m_TransformParameters;
  SizeType       m_PadToSize;
  SizeType       m_ObligatoryPadding;
  PaddingMethod  m_PaddingMethod;

  typename FixedPadderImageFilter::Pointer   m_FixedPadder;
  typename MovingPadderImageFilter::Pointer  m_MovingPadder;
  typename FixedConstantPadderType::Pointer  m_FixedConstantPadder;
  typename MovingConstantPadderType::Pointer m_MovingConstantPadder;
  typename FixedMirrorPadderType::Pointer    m_FixedMirrorPadder;
  typename MovingMirrorPadderType::Pointer   m_MovingMirrorPadder;
  typename FixedMirrorPadderType::Pointer    m_FixedMirrorWEDPadder;
  typename MovingMirrorPadderType::Pointer   m_MovingMirrorWEDPadder;
  typename BandBassFilterType::Pointer       m_BandPassFilter;

  unsigned m_ButterworthOrder;
  double   m_LowFrequency2; //square of low frequency threshold
  double   m_HighFrequency2; // square of high frequency threshold

  typename FFTFilterType::Pointer  m_FixedFFT;
  typename FFTFilterType::Pointer  m_MovingFFT;
  typename IFFTFilterType::Pointer m_IFFT;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPhaseCorrelationImageRegistrationMethod.hxx"
#endif

#endif
