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
#ifndef itkRecursiveMultiResolutionPyramidImageFilter_hxx
#define itkRecursiveMultiResolutionPyramidImageFilter_hxx

#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkCastImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkMacro.h"
#include "itkResampleImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkIdentityTransform.h"

#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
RecursiveMultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::RecursiveMultiResolutionPyramidImageFilter()
{
  this->Superclass::m_UseShrinkImageFilter = true;
}

/**
 * GenerateData
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveMultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  if ( !this->IsScheduleDownwardDivisible( this->GetSchedule() ) )
    {
    // use the Superclass implementation
    this->Superclass::GenerateData();
    return;
    }

  // Get the input and output pointers
  InputImageConstPointer inputPtr = this->GetInput();

  // Create caster, smoother and resampleShrink filters
  typedef CastImageFilter< TInputImage, TOutputImage >              CasterType;
  typedef CastImageFilter< TOutputImage, TOutputImage >             CopierType;
  typedef DiscreteGaussianImageFilter< TOutputImage, TOutputImage > SmootherType;

  typedef ImageToImageFilter< TOutputImage, TOutputImage >  ImageToImageType;
  typedef ResampleImageFilter< TOutputImage, TOutputImage > ResampleShrinkerType;
  typedef ShrinkImageFilter< TOutputImage, TOutputImage >   ShrinkerType;

  typename CasterType::Pointer caster   = CasterType::New();
  typename CopierType::Pointer copier   = CopierType::New();
  typename SmootherType::Pointer smoother = SmootherType::New();

  typename ImageToImageType::Pointer shrinkerFilter;
  //
  // only one of these pointers is going to be valid, depending on the
  // value of UseShrinkImageFilter flag
  typename ResampleShrinkerType::Pointer resampleShrinker;
  typename ShrinkerType::Pointer shrinker;

  if ( this->GetUseShrinkImageFilter() )
    {
    shrinker = ShrinkerType::New();
    shrinkerFilter = shrinker.GetPointer();
    }
  else
    {
    resampleShrinker = ResampleShrinkerType::New();
    typedef itk::LinearInterpolateImageFunction< OutputImageType, double >
    LinearInterpolatorType;
    typename LinearInterpolatorType::Pointer interpolator =
      LinearInterpolatorType::New();
    typedef itk::IdentityTransform< double, OutputImageType::ImageDimension >
    IdentityTransformType;
    typename IdentityTransformType::Pointer identityTransform =
      IdentityTransformType::New();
    resampleShrinker->SetInterpolator(interpolator);
    resampleShrinker->SetDefaultPixelValue(0);
    resampleShrinker->SetTransform(identityTransform);
    shrinkerFilter = resampleShrinker.GetPointer();
    }

  int          ilevel;
  unsigned int idim;
  unsigned int factors[ImageDimension];
  double       variance[ImageDimension];

  bool               allOnes;
  OutputImagePointer outputPtr;
  OutputImagePointer swapPtr;
  typename TOutputImage::RegionType LPRegion;

  smoother->SetUseImageSpacing(false);
  smoother->SetMaximumError( this->GetMaximumError() );
  shrinkerFilter->SetInput( smoother->GetOutput() );

  // recursively compute outputs starting from the last one
  for ( ilevel = this->GetNumberOfLevels() - 1; ilevel > -1; ilevel-- )
    {
    this->UpdateProgress( 1.0 - static_cast< float >( 1 + ilevel )
                          / static_cast< float >( this->GetNumberOfLevels() ) );

    // Allocate memory for each output
    outputPtr = this->GetOutput(ilevel);
    outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
    outputPtr->Allocate();

    // cached a copy of the largest possible region
    LPRegion = outputPtr->GetLargestPossibleRegion();

    // Check shrink factors and compute variances
    allOnes = true;
    for ( idim = 0; idim < ImageDimension; idim++ )
      {
      if ( ilevel == static_cast< int >( this->GetNumberOfLevels() ) - 1 )
        {
        factors[idim] = this->GetSchedule()[ilevel][idim];
        }
      else
        {
        factors[idim] = this->GetSchedule()[ilevel][idim]
                        / this->GetSchedule()[ilevel + 1][idim];
        }
      variance[idim] = itk::Math::sqr( 0.5
                                     * static_cast< float >( factors[idim] ) );
      if ( factors[idim] != 1 )
        {
        allOnes = false;
        }
      else
        {
        variance[idim] = 0.0;
        }
      }

    if ( allOnes && ilevel == static_cast< int >( this->GetNumberOfLevels() ) - 1 )
      {
      // just copy the input over
      caster->SetInput(inputPtr);
      caster->GraftOutput(outputPtr);
      // ensure only the requested region is updated
      caster->UpdateOutputInformation();
      caster->GetOutput()->SetRequestedRegion( outputPtr->GetRequestedRegion() );
      caster->GetOutput()->PropagateRequestedRegion();
      caster->GetOutput()->UpdateOutputData();

      swapPtr = caster->GetOutput();
      }
    else if ( allOnes )
      {
      // just copy the data over
      copier->SetInput(swapPtr);
      copier->GraftOutput(outputPtr);
      // ensure only the requested region is updated
      copier->GetOutput()->UpdateOutputInformation();
      copier->GetOutput()->SetRequestedRegion( outputPtr->GetRequestedRegion() );
      copier->GetOutput()->PropagateRequestedRegion();
      copier->GetOutput()->UpdateOutputData();

      swapPtr = copier->GetOutput();
      }
    else
      {
      if ( ilevel == static_cast< int >( this->GetNumberOfLevels() ) - 1 )
        {
        // use caster -> smoother -> shrinker piepline
        caster->SetInput(inputPtr);
        smoother->SetInput( caster->GetOutput() );
        }
      else
        {
        // use smoother -> shrinker pipeline
        smoother->SetInput(swapPtr);
        }

      smoother->SetVariance(variance);

      //      shrinker->SetShrinkFactors( factors );
      //      shrinker->GraftOutput( outputPtr );
      if ( !this->GetUseShrinkImageFilter() )
        {
        resampleShrinker->SetOutputParametersFromImage(outputPtr);
        }
      else
        {
        shrinker->SetShrinkFactors(factors);
        }
      shrinkerFilter->GraftOutput(outputPtr);
      shrinkerFilter->Modified();
      // ensure only the requested region is updated
      shrinkerFilter->GetOutput()->UpdateOutputInformation();
      shrinkerFilter->GetOutput()->SetRequestedRegion( outputPtr->GetRequestedRegion() );
      shrinkerFilter->GetOutput()->PropagateRequestedRegion();
      shrinkerFilter->GetOutput()->UpdateOutputData();

      swapPtr = shrinkerFilter->GetOutput();
      }

    // graft pipeline output back onto this filter's output
    swapPtr->SetLargestPossibleRegion(LPRegion);
    this->GraftNthOutput(ilevel, swapPtr);

    // disconnect from pipeline to stop cycle
    swapPtr->DisconnectPipeline();
    }
}

/**
 * PrintSelf method
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveMultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/*
 * GenerateOutputRequestedRegion
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveMultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::GenerateOutputRequestedRegion(DataObject *ptr)
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion(ptr);

  TOutputImage *refOutputPtr = itkDynamicCastInDebugMode< TOutputImage * >( ptr );
  if ( !refOutputPtr )
    {
    itkExceptionMacro(<< "Could not cast ptr to TOutputImage*.");
    }

  // find the index for this output
  unsigned int refLevel;
  refLevel = static_cast<unsigned int>( refOutputPtr->GetSourceOutputIndex() );

  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef GaussianOperator< OutputPixelType, ImageDimension > OperatorType;

  OperatorType *oper = new OperatorType;
  oper->SetMaximumError( this->GetMaximumError() );

  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::RegionType RegionType;

  int          ilevel, idim;
  unsigned int factors[ImageDimension];

  typename TInputImage::SizeType radius;

  RegionType requestedRegion;
  SizeType   requestedSize;
  IndexType  requestedIndex;

  // compute requested regions for lower levels
  for ( ilevel = refLevel + 1; ilevel < static_cast< int >( this->GetNumberOfLevels() );
        ilevel++ )
    {
    requestedRegion = this->GetOutput(ilevel - 1)->GetRequestedRegion();
    requestedSize = requestedRegion.GetSize();
    requestedIndex = requestedRegion.GetIndex();

    for ( idim = 0; idim < static_cast< int >( ImageDimension ); idim++ )
      {
      factors[idim] = this->GetSchedule()[ilevel - 1][idim] / this->GetSchedule()[ilevel][idim];

      // take into account shrink component
      requestedSize[idim] *= static_cast< SizeValueType >( factors[idim] );
      requestedIndex[idim] *= static_cast< IndexValueType >( factors[idim] );

      // take into account smoothing component
      if ( factors[idim] > 1 )
        {
        oper->SetDirection(idim);
        oper->SetVariance( itk::Math::sqr( 0.5
                                         * static_cast< float >( factors[idim] ) ) );
        oper->CreateDirectional();
        radius[idim] = oper->GetRadius()[idim];
        }
      else
        {
        radius[idim] = 0;
        }
      }

    requestedRegion.SetSize(requestedSize);
    requestedRegion.SetIndex(requestedIndex);
    requestedRegion.PadByRadius(radius);
    requestedRegion.Crop( this->GetOutput(ilevel)->
                          GetLargestPossibleRegion() );

    this->GetOutput(ilevel)->SetRequestedRegion(requestedRegion);
    }

  // compute requested regions for higher levels
  for ( ilevel = refLevel - 1; ilevel > -1; ilevel-- )
    {
    requestedRegion = this->GetOutput(ilevel + 1)->GetRequestedRegion();
    requestedSize = requestedRegion.GetSize();
    requestedIndex = requestedRegion.GetIndex();

    for ( idim = 0; idim < static_cast< int >( ImageDimension ); idim++ )
      {
      factors[idim] = this->GetSchedule()[ilevel][idim] / this->GetSchedule()[ilevel + 1][idim];

      // take into account smoothing component
      if ( factors[idim] > 1 )
        {
        oper->SetDirection(idim);
        oper->SetVariance( itk::Math::sqr( 0.5
                                         * static_cast< float >( factors[idim] ) ) );
        oper->CreateDirectional();
        radius[idim] = oper->GetRadius()[idim];
        }
      else
        {
        radius[idim] = 0;
        }

      requestedSize[idim] -= static_cast< SizeValueType >(
        2 * radius[idim] );
      requestedIndex[idim] += radius[idim];

      // take into account shrink component
      requestedSize[idim] = static_cast< SizeValueType >( std::floor(
                                                            static_cast< double >( requestedSize[idim] )
                                                            / static_cast< double >( factors[idim] ) ) );
      if ( requestedSize[idim] < 1 )
        {
        requestedSize[idim] = 1;
        }
      requestedIndex[idim] = static_cast< IndexValueType >( std::ceil(
                                                              static_cast< double >( requestedIndex[idim] )
                                                              / static_cast< double >( factors[idim] ) ) );
      }

    requestedRegion.SetSize(requestedSize);
    requestedRegion.SetIndex(requestedIndex);
    requestedRegion.Crop( this->GetOutput(ilevel)->
                          GetLargestPossibleRegion() );

    this->GetOutput(ilevel)->SetRequestedRegion(requestedRegion);
    }

  // clean up
  delete oper;
}

/**
 * GenerateInputRequestedRegion
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveMultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< InputImageType * >( this->GetInput() );
  if ( !inputPtr )
    {
    itkExceptionMacro(<< "Input has not been set.");
    }

  // compute baseIndex and baseSize
  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::RegionType RegionType;

  unsigned int refLevel = this->GetNumberOfLevels() - 1;
  SizeType     baseSize = this->GetOutput(refLevel)->GetRequestedRegion().GetSize();
  IndexType    baseIndex = this->GetOutput(refLevel)->GetRequestedRegion().GetIndex();
  RegionType   baseRegion;

  unsigned int idim;
  for ( idim = 0; idim < ImageDimension; idim++ )
    {
    unsigned int factor = this->GetSchedule()[refLevel][idim];
    baseIndex[idim] *= static_cast< IndexValueType >( factor );
    baseSize[idim] *= static_cast< SizeValueType >( factor );
    }
  baseRegion.SetIndex(baseIndex);
  baseRegion.SetSize(baseSize);

  // compute requirements for the smoothing part
  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef GaussianOperator< OutputPixelType, ImageDimension > OperatorType;

  OperatorType *oper = new OperatorType;

  typename TInputImage::SizeType radius;

  RegionType inputRequestedRegion = baseRegion;
  refLevel = 0;

  for ( idim = 0; idim < TInputImage::ImageDimension; idim++ )
    {
    oper->SetDirection(idim);
    oper->SetVariance( itk::Math::sqr( 0.5 * static_cast< float >(
                                       this->GetSchedule()[refLevel][idim] ) ) );
    oper->SetMaximumError( this->GetMaximumError() );
    oper->CreateDirectional();
    radius[idim] = oper->GetRadius()[idim];
    if ( this->GetSchedule()[refLevel][idim] <= 1 )
      {
      radius[idim] = 0;
      }
    }
  delete oper;

  inputRequestedRegion.PadByRadius(radius);

  // make sure the requested region is within the largest possible
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );

  // set the input requested region
  inputPtr->SetRequestedRegion(inputRequestedRegion);
}
} // namespace itk

#endif
