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
#ifndef itkN4BiasFieldCorrectionImageFilter_hxx
#define itkN4BiasFieldCorrectionImageFilter_hxx

#include "itkN4BiasFieldCorrectionImageFilter.h"

#include "itkAddImageFilter.h"
#include "itkBSplineControlPointImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImportImageFilter.h"
#include "itkIterationReporter.h"
#include "itkSubtractImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"

CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/vnl_complex_traits.h"
#include "complex"
CLANG_PRAGMA_POP

namespace itk {

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>
::N4BiasFieldCorrectionImageFilter() :
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  m_MaskLabel( NumericTraits< MaskPixelType >::OneValue() ),
  m_UseMaskLabel( true ),
#endif
  m_NumberOfHistogramBins( 200 ),
  m_WienerFilterNoise( 0.01 ),
  m_BiasFieldFullWidthAtHalfMaximum( 0.15 ),
  m_ElapsedIterations( 0 ),
  m_ConvergenceThreshold( 0.001 ),
  m_CurrentConvergenceMeasurement( NumericTraits<RealType>::ZeroValue() ),
  m_CurrentLevel( 0 ),
  m_SplineOrder( 3 )
{
  this->SetNumberOfRequiredInputs( 1 );

  this->m_LogBiasFieldControlPointLattice = ITK_NULLPTR;

  this->m_NumberOfFittingLevels.Fill( 1 );
  this->m_NumberOfControlPoints.Fill( 4 );

  this->m_MaximumNumberOfIterations.SetSize( 1 );
  this->m_MaximumNumberOfIterations.Fill( 50 );
}

template<typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>
::GenerateData()
{
  this->AllocateOutputs();

  const InputImageType * inputImage = this->GetInput();
  typedef typename InputImageType::RegionType RegionType;
  const RegionType inputRegion = inputImage->GetBufferedRegion();

  // Calculate the log of the input image.
  RealImagePointer logInputImage = RealImageType::New();
  logInputImage->CopyInformation( inputImage );
  logInputImage->SetRegions( inputRegion );
  logInputImage->Allocate( false );

  ImageRegionConstIterator<InputImageType> inpItr( inputImage, inputRegion );
  ImageRegionIterator<RealImageType> outItr( logInputImage, inputRegion );

  inpItr.GoToBegin();
  outItr.GoToBegin();

  while( !inpItr.IsAtEnd() )
    {
    outItr.Set( static_cast< RealType >( inpItr.Get() ) );
    ++inpItr;
    ++outItr;
    }

  const MaskImageType * maskImage = this->GetMaskImage();
  const RealImageType * confidenceImage = this->GetConfidenceImage();
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  const MaskPixelType maskLabel = this->GetMaskLabel();
  const bool useMaskLabel = this->GetUseMaskLabel();
#endif

  ImageRegionIteratorWithIndex<RealImageType> It( logInputImage, inputRegion );

  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    if( ( !maskImage ||
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          ( useMaskLabel && maskImage->GetPixel( It.GetIndex() ) == maskLabel ) || ( !useMaskLabel &&
#endif
            maskImage->GetPixel( It.GetIndex() ) != NumericTraits< MaskPixelType >::ZeroValue() )
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          )
#endif
        && ( !confidenceImage ||
             confidenceImage->GetPixel( It.GetIndex() ) > 0.0 ) )
      {
      if( It.Get() > NumericTraits<typename InputImageType::PixelType>::ZeroValue() )
        {
        It.Set( std::log( static_cast< RealType >( It.Get() ) ) );
        }
      }
    }

  // Duplicate logInputImage since we reuse the original at each iteration.

  typedef ImageDuplicator<RealImageType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( logInputImage );
  duplicator->Update();

  RealImagePointer logUncorrectedImage = duplicator->GetModifiableOutput();

  // Provide an initial log bias field of zeros

  RealImagePointer logBiasField = RealImageType::New();
  logBiasField->CopyInformation( inputImage );
  logBiasField->SetRegions( inputImage->GetLargestPossibleRegion() );
  logBiasField->Allocate( true ); // initialize buffer to zero

  // Iterate until convergence or iterative exhaustion.
  unsigned int maximumNumberOfLevels = 1;
  for( unsigned int d = 0; d < this->m_NumberOfFittingLevels.Size(); d++ )
    {
    if( this->m_NumberOfFittingLevels[d] > maximumNumberOfLevels )
      {
      maximumNumberOfLevels = this->m_NumberOfFittingLevels[d];
      }
    }
  if( this->m_MaximumNumberOfIterations.Size() != maximumNumberOfLevels )
    {
    itkExceptionMacro(
      "Number of iteration levels is not equal to the max number of levels." );
    }

  for( this->m_CurrentLevel = 0; this->m_CurrentLevel < maximumNumberOfLevels;
       this->m_CurrentLevel++ )
    {
    IterationReporter reporter( this, 0, 1 );

    this->m_ElapsedIterations = 0;
    this->m_CurrentConvergenceMeasurement = NumericTraits<RealType>::max();
    while( this->m_ElapsedIterations++ <
           this->m_MaximumNumberOfIterations[this->m_CurrentLevel] &&
           this->m_CurrentConvergenceMeasurement > this->m_ConvergenceThreshold )
      {

      // Sharpen the current estimate of the uncorrected image.

      RealImagePointer logSharpenedImage = this->SharpenImage( logUncorrectedImage );

      typedef SubtractImageFilter<RealImageType, RealImageType, RealImageType>
      SubtracterType;
      typename SubtracterType::Pointer subtracter1 = SubtracterType::New();
      subtracter1->SetInput1( logUncorrectedImage );
      subtracter1->SetInput2( logSharpenedImage );

      RealImagePointer residualBiasField = subtracter1->GetOutput();
      residualBiasField->Update();

      // Smooth the residual bias field estimate and add the resulting
      // control point grid to get the new total bias field estimate.

      RealImagePointer newLogBiasField = this->UpdateBiasFieldEstimate( residualBiasField );

      this->m_CurrentConvergenceMeasurement =
        this->CalculateConvergenceMeasurement( logBiasField, newLogBiasField );
      logBiasField = newLogBiasField;

      typename SubtracterType::Pointer subtracter2 = SubtracterType::New();
      subtracter2->SetInput1( logInputImage );
      subtracter2->SetInput2( logBiasField );

      logUncorrectedImage = subtracter2->GetOutput();
      logUncorrectedImage->Update();

      reporter.CompletedStep();
      }

    typedef BSplineControlPointImageFilter<BiasFieldControlPointLatticeType, ScalarImageType>
      BSplineReconstructerType;
    typename BSplineReconstructerType::Pointer reconstructer = BSplineReconstructerType::New();
    reconstructer->SetInput( this->m_LogBiasFieldControlPointLattice );
    reconstructer->SetOrigin( logBiasField->GetOrigin() );
    reconstructer->SetSpacing( logBiasField->GetSpacing() );
    reconstructer->SetDirection( logBiasField->GetDirection() );
    reconstructer->SetSize( logBiasField->GetLargestPossibleRegion().GetSize() );
    reconstructer->SetSplineOrder( this->m_SplineOrder );
    reconstructer->Update();

    typename BSplineReconstructerType::ArrayType numberOfLevels;
    numberOfLevels.Fill( 1 );
    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      if( this->m_NumberOfFittingLevels[d] + 1 >= this->m_CurrentLevel &&
          this->m_CurrentLevel != maximumNumberOfLevels-1 )
        {
        numberOfLevels[d] = 2;
        }
      }
    this->m_LogBiasFieldControlPointLattice = reconstructer->
      RefineControlPointLattice( numberOfLevels );
    }

  typedef ExpImageFilter<RealImageType, RealImageType> ExpImageFilterType;
  typename ExpImageFilterType::Pointer expFilter = ExpImageFilterType::New();
  expFilter->SetInput( logBiasField );
  expFilter->Update();

  // Divide the input image by the bias field to get the final image.

  typedef DivideImageFilter<InputImageType, RealImageType, OutputImageType>
  DividerType;
  typename DividerType::Pointer divider = DividerType::New();
  divider->SetInput1( inputImage );
  divider->SetInput2( expFilter->GetOutput() );
  divider->GraftOutput( this->GetOutput() );
  divider->Update();

  this->GraftOutput( divider->GetOutput() );
}

template<typename TInputImage, typename TMaskImage, typename TOutputImage>
typename
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealImagePointer
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>
::SharpenImage( const RealImageType *unsharpenedImage ) const
{
  const MaskImageType * maskImage = this->GetMaskImage();
  const RealImageType * confidenceImage = this->GetConfidenceImage();
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  const MaskPixelType maskLabel = this->GetMaskLabel();
  const bool useMaskLabel = this->GetUseMaskLabel();
#endif

  // Build the histogram for the uncorrected image.  Store copy
  // in a vnl_vector to utilize vnl FFT routines.  Note that variables
  // in real space are denoted by a single uppercase letter whereas their
  // frequency counterparts are indicated by a trailing lowercase 'f'.

  RealType binMaximum = NumericTraits<RealType>::NonpositiveMin();
  RealType binMinimum = NumericTraits<RealType>::max();

  ImageRegionConstIterator<RealImageType> ItU(
    unsharpenedImage, unsharpenedImage->GetLargestPossibleRegion() );

  for( ItU.GoToBegin(); !ItU.IsAtEnd(); ++ItU )
    {
    if( ( !maskImage ||
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          ( useMaskLabel && maskImage->GetPixel( ItU.GetIndex() ) == maskLabel ) || ( !useMaskLabel &&
#endif
            maskImage->GetPixel( ItU.GetIndex() ) != NumericTraits< MaskPixelType >::ZeroValue() )
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          )
#endif
        && ( !confidenceImage ||
             confidenceImage->GetPixel( ItU.GetIndex() ) > 0.0 ) )
      {
      RealType pixel = ItU.Get();
      if( pixel > binMaximum )
        {
        binMaximum = pixel;
        }
      else if( pixel < binMinimum )
        {
        binMinimum = pixel;
        }
      }
    }
  RealType histogramSlope = ( binMaximum - binMinimum ) /
    static_cast<RealType>( this->m_NumberOfHistogramBins - 1 );

  // Create the intensity profile (within the masked region, if applicable)
  // using a triangular parzen windowing scheme.

  vnl_vector<RealType> H( this->m_NumberOfHistogramBins, 0.0 );

  for( ItU.GoToBegin(); !ItU.IsAtEnd(); ++ItU )
    {
    if( ( !maskImage ||
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          ( useMaskLabel && maskImage->GetPixel( ItU.GetIndex() ) == maskLabel ) || ( !useMaskLabel &&
#endif
            maskImage->GetPixel( ItU.GetIndex() ) != NumericTraits< MaskPixelType >::ZeroValue() )
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          )
#endif
        && ( !confidenceImage ||
             confidenceImage->GetPixel( ItU.GetIndex() ) > 0.0 ) )
      {
      RealType pixel = ItU.Get();

      RealType cidx = ( static_cast<RealType>( pixel ) - binMinimum ) /
        histogramSlope;
      unsigned int idx = itk::Math::floor( cidx );
      RealType     offset = cidx - static_cast<RealType>( idx );

      if( offset == 0.0 )
        {
        H[idx] += 1.0;
        }
      else if( idx < this->m_NumberOfHistogramBins - 1 )
        {
        H[idx] += 1.0 - offset;
        H[idx+1] += offset;
        }
      }
    }

  // Determine information about the intensity histogram and zero-pad
  // histogram to a power of 2.

  RealType exponent =
    std::ceil( std::log( static_cast<RealType>( this->m_NumberOfHistogramBins ) ) /
              std::log( 2.0 ) ) + 1;
  unsigned int paddedHistogramSize = static_cast<unsigned int>(
    std::pow( static_cast<RealType>( 2.0 ), exponent ) + 0.5 );
  unsigned int histogramOffset = static_cast<unsigned int>( 0.5 *
    ( paddedHistogramSize - this->m_NumberOfHistogramBins ) );

  typedef double                           FFTComputationType;
  typedef std::complex<FFTComputationType> FFTComplexType;

  vnl_vector< FFTComplexType > V( paddedHistogramSize,
                                         FFTComplexType( 0.0, 0.0 ) );

  for( unsigned int n = 0; n < this->m_NumberOfHistogramBins; n++ )
    {
    V[n+histogramOffset] = H[n];
    }

  // Instantiate the 1-d vnl fft routine.

  vnl_fft_1d<FFTComputationType> fft( paddedHistogramSize );

  vnl_vector< FFTComplexType > Vf( V );

  fft.fwd_transform( Vf );

  // Create the Gaussian filter.

  RealType scaledFWHM = this->m_BiasFieldFullWidthAtHalfMaximum / histogramSlope;
  RealType expFactor = 4.0 * std::log( 2.0 ) / itk::Math::sqr( scaledFWHM );
  RealType scaleFactor = 2.0 * std::sqrt( std::log( 2.0 )
                                         / itk::Math::pi ) / scaledFWHM;

  vnl_vector< FFTComplexType > F( paddedHistogramSize,
                                         FFTComplexType( 0.0, 0.0 ) );

  F[0] = FFTComplexType( scaleFactor, 0.0 );
  unsigned int halfSize = static_cast<unsigned int>(
      0.5 * paddedHistogramSize );
  for( unsigned int n = 1; n <= halfSize; n++ )
    {
    F[n] = F[paddedHistogramSize - n] = FFTComplexType( scaleFactor *
      std::exp( -itk::Math::sqr( static_cast<RealType>( n ) ) * expFactor ), 0.0 );
    }
  if( paddedHistogramSize % 2 == 0 )
    {
    F[halfSize] = FFTComplexType( scaleFactor * std::exp( 0.25 *
      -itk::Math::sqr( static_cast<RealType>( paddedHistogramSize ) ) *
      expFactor ), 0.0 );
    }

  vnl_vector< FFTComplexType > Ff( F );

  fft.fwd_transform( Ff );

  // Create the Wiener deconvolution filter.

  vnl_vector< FFTComplexType > Gf( paddedHistogramSize );

  const FFTComputationType wienerNoiseValue=static_cast<FFTComputationType>(this->m_WienerFilterNoise);
  for( unsigned int n = 0; n < paddedHistogramSize; n++ )
    {
    FFTComplexType c =
      vnl_complex_traits< FFTComplexType >::conjugate( Ff[n] );
    Gf[n] = c / ( c * Ff[n] + wienerNoiseValue );
    }

  vnl_vector< FFTComplexType > Uf( paddedHistogramSize );

  for( unsigned int n = 0; n < paddedHistogramSize; n++ )
    {
    Uf[n] = Vf[n] * Gf[n].real();
    }

  vnl_vector< FFTComplexType > U( Uf );

  fft.bwd_transform( U );
  for( unsigned int n = 0; n < paddedHistogramSize; n++ )
    {
    U[n] = FFTComplexType( std::max( U[n].real(), static_cast<FFTComputationType>( 0.0 ) ), 0.0 );
    }

  // Compute mapping E(u|v).

  vnl_vector< FFTComplexType > numerator( paddedHistogramSize );

  for( unsigned int n = 0; n < paddedHistogramSize; n++ )
    {
    numerator[n] = FFTComplexType(
        ( binMinimum + ( static_cast<RealType>( n ) - histogramOffset )
          * histogramSlope ) * U[n].real(), 0.0 );
    }
  fft.fwd_transform( numerator );
  for( unsigned int n = 0; n < paddedHistogramSize; n++ )
    {
    numerator[n] *= Ff[n];
    }
  fft.bwd_transform( numerator );

  vnl_vector< FFTComplexType > denominator( U );

  fft.fwd_transform( denominator );
  for( unsigned int n = 0; n < paddedHistogramSize; n++ )
    {
    denominator[n] *= Ff[n];
    }
  fft.bwd_transform( denominator );

  vnl_vector<RealType> E( paddedHistogramSize );

  for( unsigned int n = 0; n < paddedHistogramSize; n++ )
    {
    if( denominator[n].real() != 0.0 )
      {
      E[n] = numerator[n].real() / denominator[n].real();
      }
    else
      {
      E[n] = 0.0;
      }
    }

  // Remove the zero-padding from the mapping.

  E = E.extract( this->m_NumberOfHistogramBins, histogramOffset );

  const InputImageType * inputImage = this->GetInput();

  // Sharpen the image with the new mapping, E(u|v)
  RealImagePointer sharpenedImage = RealImageType::New();
  sharpenedImage->CopyInformation( inputImage );
  sharpenedImage->SetRegions( inputImage->GetLargestPossibleRegion() );
  sharpenedImage->Allocate( true ); // initialize buffer to zero

  ImageRegionIterator<RealImageType> ItC(
    sharpenedImage, sharpenedImage->GetLargestPossibleRegion() );

  for( ItU.GoToBegin(), ItC.GoToBegin(); !ItU.IsAtEnd(); ++ItU, ++ItC )
    {
    if( ( !maskImage ||
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          ( useMaskLabel && maskImage->GetPixel( ItU.GetIndex() ) == maskLabel ) || ( !useMaskLabel &&
#endif
            maskImage->GetPixel( ItU.GetIndex() ) != NumericTraits< MaskPixelType >::ZeroValue() )
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          )
#endif
        && ( !confidenceImage ||
             confidenceImage->GetPixel( ItU.GetIndex() ) > 0.0 ) )
      {
      RealType     cidx = ( ItU.Get() - binMinimum ) / histogramSlope;
      unsigned int idx = itk::Math::floor( cidx );

      RealType correctedPixel = 0;
      if( idx < E.size() - 1 )
        {
        correctedPixel = E[idx] + ( E[idx + 1] - E[idx] )
          * ( cidx - static_cast<RealType>( idx ) );
        }
      else
        {
        correctedPixel = E[E.size() - 1];
        }
      ItC.Set( correctedPixel );
      }
    }

  return sharpenedImage;
}

template<typename TInputImage, typename TMaskImage, typename TOutputImage>
typename
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealImagePointer
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>
::UpdateBiasFieldEstimate( RealImageType* fieldEstimate )
{
  // Temporarily set the direction cosine to identity since the B-spline
  // approximation algorithm works in parametric space and not physical
  // space.
  typename ScalarImageType::DirectionType identity;
  identity.SetIdentity();

  const typename ScalarImageType::RegionType & bufferedRegion = fieldEstimate->GetBufferedRegion();
  const SizeValueType numberOfPixels = bufferedRegion.GetNumberOfPixels();
  const bool filterHandlesMemory = false;

  typedef ImportImageFilter<RealType, ImageDimension> ImporterType;
  typename ImporterType::Pointer importer = ImporterType::New();
  importer->SetImportPointer( fieldEstimate->GetBufferPointer(), numberOfPixels, filterHandlesMemory );
  importer->SetRegion( fieldEstimate->GetBufferedRegion() );
  importer->SetOrigin( fieldEstimate->GetOrigin() );
  importer->SetSpacing( fieldEstimate->GetSpacing() );
  importer->SetDirection( identity );
  importer->Update();

  const typename ImporterType::OutputImageType * parametricFieldEstimate = importer->GetOutput();

  PointSetPointer fieldPoints = PointSetType::New();
  fieldPoints->Initialize();

  typename BSplineFilterType::WeightsContainerType::Pointer weights =
    BSplineFilterType::WeightsContainerType::New();
  weights->Initialize();

  const MaskImageType * maskImage = this->GetMaskImage();
  const RealImageType * confidenceImage = this->GetConfidenceImage();
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  const MaskPixelType maskLabel = this->GetMaskLabel();
  const bool useMaskLabel = this->GetUseMaskLabel();
#endif

  ImageRegionConstIteratorWithIndex<RealImageType>
    It( parametricFieldEstimate, parametricFieldEstimate->GetRequestedRegion() );

  unsigned int index = 0;
  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    if( ( !maskImage ||
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          ( useMaskLabel && maskImage->GetPixel( It.GetIndex() ) == maskLabel ) || ( !useMaskLabel &&
#endif
            maskImage->GetPixel( It.GetIndex() ) != NumericTraits< MaskPixelType >::ZeroValue() )
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          )
#endif
        && ( !confidenceImage ||
             confidenceImage->GetPixel( It.GetIndex() ) > 0.0 ) )
      {
      PointType point;
      parametricFieldEstimate->TransformIndexToPhysicalPoint( It.GetIndex(), point );

      ScalarType scalar;
      scalar[0] = It.Get();

      fieldPoints->SetPointData( index, scalar );
      fieldPoints->SetPoint( index, point );

      RealType confidenceWeight = 1.0;
      if( confidenceImage )
        {
        confidenceWeight = confidenceImage->GetPixel( It.GetIndex() );
        }
      weights->InsertElement( index, confidenceWeight );
      index++;
      }
    }

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();

  typename BSplineFilterType::ArrayType numberOfControlPoints;
  typename BSplineFilterType::ArrayType numberOfFittingLevels;
  numberOfFittingLevels.Fill( 1 );
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    if( !this->m_LogBiasFieldControlPointLattice )
      {
      numberOfControlPoints[d] = this->m_NumberOfControlPoints[d];
      }
    else
      {
      numberOfControlPoints[d] = this->m_LogBiasFieldControlPointLattice->
        GetLargestPossibleRegion().GetSize()[d];
      }
    }

  typename ScalarImageType::PointType parametricOrigin =
    fieldEstimate->GetOrigin();
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    parametricOrigin[d] += (
        fieldEstimate->GetSpacing()[d] *
        fieldEstimate->GetLargestPossibleRegion().GetIndex()[d] );
    }
  bspliner->SetOrigin( parametricOrigin );
  bspliner->SetSpacing( fieldEstimate->GetSpacing() );
  bspliner->SetSize( fieldEstimate->GetLargestPossibleRegion().GetSize() );
  bspliner->SetDirection( fieldEstimate->GetDirection() );
  bspliner->SetGenerateOutputImage( false );
  bspliner->SetNumberOfLevels( numberOfFittingLevels );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner->SetInput( fieldPoints );
  bspliner->SetPointWeights( weights );
  bspliner->Update();

  typename BiasFieldControlPointLatticeType::Pointer phiLattice = bspliner->GetPhiLattice();

  // Add the bias field control points to the current estimate.

  if( !this->m_LogBiasFieldControlPointLattice )
    {
    this->m_LogBiasFieldControlPointLattice = phiLattice;
    }
  else
    {
    // Ensure that the two lattices occupy the same physical space.  Not
    // necessary for performance since the parameters of the reconstructed
    // bias field are specified later in this function in the reconstructer.
    phiLattice->CopyInformation( this->m_LogBiasFieldControlPointLattice );

    typedef AddImageFilter<BiasFieldControlPointLatticeType,
                           BiasFieldControlPointLatticeType,
                           BiasFieldControlPointLatticeType>
      AdderType;
    typename AdderType::Pointer adder = AdderType::New();
    adder->SetInput1( this->m_LogBiasFieldControlPointLattice );
    adder->SetInput2( phiLattice );
    adder->Update();

    this->m_LogBiasFieldControlPointLattice = adder->GetOutput();
    }

  RealImagePointer smoothField = this->ReconstructBiasField( this->m_LogBiasFieldControlPointLattice );

  return smoothField;
}

template<typename TInputImage, typename TMaskImage, typename TOutputImage>
typename
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealImagePointer
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>
::ReconstructBiasField( BiasFieldControlPointLatticeType* controlPointLattice )
{
  const InputImageType * inputImage = this->GetInput();

  typedef BSplineControlPointImageFilter
    <BiasFieldControlPointLatticeType, ScalarImageType> BSplineReconstructerType;
  typename BSplineReconstructerType::Pointer reconstructer =
    BSplineReconstructerType::New();
  reconstructer->SetInput( controlPointLattice );
  reconstructer->SetOrigin( inputImage->GetOrigin() );
  reconstructer->SetSpacing( inputImage->GetSpacing() );
  reconstructer->SetDirection( inputImage->GetDirection() );
  reconstructer->SetSplineOrder( this->m_SplineOrder );
  reconstructer->SetSize( inputImage->GetLargestPossibleRegion().GetSize() );

  typename ScalarImageType::Pointer biasFieldBsplineImage = reconstructer->GetOutput();
  biasFieldBsplineImage->Update();

  typedef VectorIndexSelectionCastImageFilter<ScalarImageType, RealImageType>
    SelectorType;
  typename SelectorType::Pointer selector = SelectorType::New();
  selector->SetInput( biasFieldBsplineImage );
  selector->SetIndex( 0 );

  RealImagePointer biasField = selector->GetOutput();
  biasField->Update();

  biasField->DisconnectPipeline();
  biasField->SetRegions( inputImage->GetRequestedRegion() );

  return biasField;
}

template<typename TInputImage, typename TMaskImage, typename TOutputImage>
typename
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealType
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>
::CalculateConvergenceMeasurement( const RealImageType *fieldEstimate1,
                                   const RealImageType *fieldEstimate2 ) const
{
  typedef SubtractImageFilter<RealImageType, RealImageType, RealImageType>
    SubtracterType;
  typename SubtracterType::Pointer subtracter = SubtracterType::New();
  subtracter->SetInput1( fieldEstimate1 );
  subtracter->SetInput2( fieldEstimate2 );
  subtracter->Update();

  // Calculate statistics over the mask region

  RealType mu = 0.0;
  RealType sigma = 0.0;
  RealType N = 0.0;

  const MaskImageType * maskImage = this->GetMaskImage();
  const RealImageType * confidenceImage = this->GetConfidenceImage();
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  const MaskPixelType maskLabel = this->GetMaskLabel();
  const bool useMaskLabel = this->GetUseMaskLabel();
#endif

  ImageRegionConstIteratorWithIndex<RealImageType> It(
    subtracter->GetOutput(),
    subtracter->GetOutput()->GetLargestPossibleRegion() );

  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    if( ( !maskImage ||
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          ( useMaskLabel && maskImage->GetPixel( It.GetIndex() ) == maskLabel ) || ( !useMaskLabel &&
#endif
            maskImage->GetPixel( It.GetIndex() ) != NumericTraits< MaskPixelType >::ZeroValue() )
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
          )
#endif
        && ( !confidenceImage ||
             confidenceImage->GetPixel( It.GetIndex() ) > 0.0 ) )
      {
      RealType pixel = std::exp( It.Get() );
      N += 1.0;

      if( N > 1.0 )
        {
        sigma = sigma + itk::Math::sqr( pixel - mu ) * ( N - 1.0 ) / N;
        }
      mu = mu * ( 1.0 - 1.0 / N ) + pixel / N;
      }
    }
  sigma = std::sqrt( sigma / ( N - 1.0 ) );

  return ( sigma / mu );
}

template<typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  os << indent << "Mask label: " << static_cast< typename NumericTraits< MaskPixelType >::PrintType >( this->m_MaskLabel ) << std::endl;
#endif
  os << indent << "Number of histogram bins: "
     << this->m_NumberOfHistogramBins << std::endl;
  os << indent << "Wiener filter noise: "
     << this->m_WienerFilterNoise << std::endl;
  os << indent << "Bias field FWHM: "
     << this->m_BiasFieldFullWidthAtHalfMaximum << std::endl;
  os << indent << "Maximum number of iterations: "
     << this->m_MaximumNumberOfIterations << std::endl;
  os << indent << "Convergence threshold: "
     << this->m_ConvergenceThreshold << std::endl;
  os << indent << "Spline order: " << this->m_SplineOrder << std::endl;
  os << indent << "Number of fitting levels: "
     << this->m_NumberOfFittingLevels << std::endl;
  os << indent << "Number of control points: "
     << this->m_NumberOfControlPoints << std::endl;
  os << indent << "CurrentConvergenceMeasurement: "
     << this->m_CurrentConvergenceMeasurement << std::endl;
  os << indent << "CurrentLevel: " << this->m_CurrentLevel << std::endl;
  os << indent << "ElapsedIterations: "
     << this->m_ElapsedIterations << std::endl;
  itkPrintSelfObjectMacro( LogBiasFieldControlPointLattice );
}

} // end namespace itk

#endif
