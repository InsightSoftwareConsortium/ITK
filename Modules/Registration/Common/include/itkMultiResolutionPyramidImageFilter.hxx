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
#ifndef itkMultiResolutionPyramidImageFilter_hxx
#define itkMultiResolutionPyramidImageFilter_hxx

#include "itkMultiResolutionPyramidImageFilter.h"
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
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::MultiResolutionPyramidImageFilter()
{
  m_NumberOfLevels = 0;
  this->SetNumberOfLevels(2);
  m_MaximumError = 0.1;
  m_UseShrinkImageFilter = false;
}

/**
 * Set the number of computation levels
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::SetNumberOfLevels(
  unsigned int num)
{
  if ( m_NumberOfLevels == num )
    {
    return;
    }

  this->Modified();

  // clamp value to be at least one
  m_NumberOfLevels = num;
  if ( m_NumberOfLevels < 1 ) { m_NumberOfLevels = 1; }

  // resize the schedules
  ScheduleType temp(m_NumberOfLevels, ImageDimension);
  temp.Fill(0);
  m_Schedule = temp;

  // determine initial shrink factor
  unsigned int startfactor = 1;
  startfactor = startfactor << ( m_NumberOfLevels - 1 );

  // set the starting shrink factors
  this->SetStartingShrinkFactors(startfactor);

  // set the required number of outputs
  this->SetNumberOfRequiredOutputs(m_NumberOfLevels);

  unsigned int numOutputs = static_cast< unsigned int >( this->GetNumberOfIndexedOutputs() );
  unsigned int idx;
  if ( numOutputs < m_NumberOfLevels )
    {
    // add extra outputs
    for ( idx = numOutputs; idx < m_NumberOfLevels; idx++ )
      {
      typename DataObject::Pointer output =
        this->MakeOutput(idx);
      this->SetNthOutput( idx, output.GetPointer() );
      }
    }
  else if ( numOutputs > m_NumberOfLevels )
    {
    // remove extra outputs
    for ( idx = m_NumberOfLevels; idx < numOutputs; idx++ )
      {
      this->RemoveOutput(idx);
      }
    }
}

/*
 * Set the starting shrink factors
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::SetStartingShrinkFactors(
  unsigned int factor)
{
  unsigned int array[ImageDimension];

  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    array[dim] = factor;
    }

  this->SetStartingShrinkFactors(array);
}

/**
 * Set the starting shrink factors
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::SetStartingShrinkFactors(
  unsigned int *factors)
{
  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    m_Schedule[0][dim] = factors[dim];
    if ( m_Schedule[0][dim] == 0 )
      {
      m_Schedule[0][dim] = 1;
      }
    }

  for ( unsigned int level = 1; level < m_NumberOfLevels; ++level )
    {
    for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
      {
      m_Schedule[level][dim] = m_Schedule[level - 1][dim] / 2;
      if ( m_Schedule[level][dim] == 0 )
        {
        m_Schedule[level][dim] = 1;
        }
      }
    }

  this->Modified();
}

/*
 * Get the starting shrink factors
 */
template< typename TInputImage, typename TOutputImage >
const unsigned int *
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::GetStartingShrinkFactors() const
{
  return ( m_Schedule.data_block() );
}

/*
 * Set the multi-resolution schedule
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::SetSchedule(
  const ScheduleType & schedule)
{
  if ( schedule.rows() != m_NumberOfLevels
       || schedule.columns() != ImageDimension )
    {
    itkDebugMacro(<< "Schedule has wrong dimensions");
    return;
    }

  if ( schedule == m_Schedule )
    {
    return;
    }

  this->Modified();
  unsigned int level, dim;
  for ( level = 0; level < m_NumberOfLevels; level++ )
    {
    for ( dim = 0; dim < ImageDimension; dim++ )
      {
      m_Schedule[level][dim] = schedule[level][dim];

      // set schedule to max( 1, min(schedule[level],
      //  schedule[level-1] );
      if ( level > 0 )
        {
        m_Schedule[level][dim] = std::min(
          m_Schedule[level][dim], m_Schedule[level - 1][dim]);
        }

      if ( m_Schedule[level][dim] < 1 )
        {
        m_Schedule[level][dim] = 1;
        }
      }
    }
}

/*
 * Is the schedule downward divisible ?
 */
template< typename TInputImage, typename TOutputImage >
bool
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::IsScheduleDownwardDivisible(const ScheduleType & schedule)
{
  unsigned int ilevel, idim;

  for ( ilevel = 0; ilevel < schedule.rows() - 1; ilevel++ )
    {
    for ( idim = 0; idim < schedule.columns(); idim++ )
      {
      if ( schedule[ilevel][idim] == 0 )
        {
        return false;
        }
      if ( ( schedule[ilevel][idim] % schedule[ilevel + 1][idim] ) > 0 )
        {
        return false;
        }
      }
    }

  return true;
}

/*
 * GenerateData for non downward divisible schedules
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Get the input and output pointers
  InputImageConstPointer inputPtr = this->GetInput();

  // Create caster, smoother and resampleShrinker filters
  typedef CastImageFilter< TInputImage, TOutputImage >              CasterType;
  typedef DiscreteGaussianImageFilter< TOutputImage, TOutputImage > SmootherType;

  typedef ImageToImageFilter< TOutputImage, TOutputImage >  ImageToImageType;
  typedef ResampleImageFilter< TOutputImage, TOutputImage > ResampleShrinkerType;
  typedef ShrinkImageFilter< TOutputImage, TOutputImage >   ShrinkerType;

  typename CasterType::Pointer caster = CasterType::New();
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
    resampleShrinker->SetInterpolator(interpolator);
    resampleShrinker->SetDefaultPixelValue(0);
    shrinkerFilter = resampleShrinker.GetPointer();
    }
  // Setup the filters
  caster->SetInput(inputPtr);

  smoother->SetUseImageSpacing(false);
  smoother->SetInput( caster->GetOutput() );
  smoother->SetMaximumError(m_MaximumError);

  shrinkerFilter->SetInput( smoother->GetOutput() );

  unsigned int ilevel, idim;
  unsigned int factors[ImageDimension];
  double       variance[ImageDimension];

  for ( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
    {
    this->UpdateProgress( static_cast< float >( ilevel )
                          / static_cast< float >( m_NumberOfLevels ) );

    // Allocate memory for each output
    OutputImagePointer outputPtr = this->GetOutput(ilevel);
    outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
    outputPtr->Allocate();

    // compute shrink factors and variances
    for ( idim = 0; idim < ImageDimension; idim++ )
      {
      factors[idim] = m_Schedule[ilevel][idim];
      variance[idim] = itk::Math::sqr( 0.5
                                     * static_cast< float >( factors[idim] ) );
      }

    if ( !this->GetUseShrinkImageFilter() )
      {
      typedef itk::IdentityTransform< double, OutputImageType::ImageDimension >
      IdentityTransformType;
      typename IdentityTransformType::Pointer identityTransform =
        IdentityTransformType::New();
      resampleShrinker->SetOutputParametersFromImage(outputPtr);
      resampleShrinker->SetTransform(identityTransform);
      }
    else
      {
      shrinker->SetShrinkFactors(factors);
      }
    // use mini-pipeline to compute output
    smoother->SetVariance(variance);

    shrinkerFilter->GraftOutput(outputPtr);

    // force to always update in case shrink factors are the same
    shrinkerFilter->Modified();
    shrinkerFilter->UpdateLargestPossibleRegion();
    this->GraftNthOutput( ilevel, shrinkerFilter->GetOutput() );
    }
}

/**
 * PrintSelf method
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MaximumError: " << m_MaximumError << std::endl;
  os << indent << "No. levels: " << m_NumberOfLevels << std::endl;
  os << indent << "Schedule: " << std::endl;
  os << m_Schedule << std::endl;
  os << "Use ShrinkImageFilter= " << m_UseShrinkImageFilter << std::endl;
}

/*
 * GenerateOutputInformation
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer inputPtr = this->GetInput();

  if ( !inputPtr  )
    {
    itkExceptionMacro(<< "Input has not been set");
    }

  const typename InputImageType::PointType &
  inputOrigin = inputPtr->GetOrigin();
  const typename InputImageType::SpacingType &
  inputSpacing = inputPtr->GetSpacing();
  const typename InputImageType::DirectionType &
  inputDirection = inputPtr->GetDirection();
  const typename InputImageType::SizeType & inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType & inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typedef typename OutputImageType::SizeType  SizeType;
  typedef typename OutputImageType::IndexType IndexType;

  OutputImagePointer outputPtr;
  typename OutputImageType::PointType outputOrigin;
  typename OutputImageType::SpacingType outputSpacing;
  SizeType  outputSize;
  IndexType outputStartIndex;

  // we need to compute the output spacing, the output image size,
  // and the output image start index
  for ( unsigned int ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
    {
    outputPtr = this->GetOutput(ilevel);
    if ( !outputPtr ) { continue; }

    for ( unsigned int idim = 0; idim < OutputImageType::ImageDimension; idim++ )
      {
      const double shrinkFactor = static_cast< double >( m_Schedule[ilevel][idim] );
      outputSpacing[idim] = inputSpacing[idim] * shrinkFactor;

      outputSize[idim] = static_cast< SizeValueType >(
        std::floor(static_cast< double >( inputSize[idim] ) / shrinkFactor) );
      if ( outputSize[idim] < 1 ) { outputSize[idim] = 1; }

      outputStartIndex[idim] = static_cast< IndexValueType >(
        std::ceil(static_cast< double >( inputStartIndex[idim] ) / shrinkFactor) );
      }
    //Now compute the new shifted origin for the updated levels;
    const typename OutputImageType::PointType::VectorType outputOriginOffset =
      ( inputDirection * ( outputSpacing - inputSpacing ) ) * 0.5;
    for ( unsigned int idim = 0; idim < OutputImageType::ImageDimension; idim++ )
      {
      outputOrigin[idim] = inputOrigin[idim] + outputOriginOffset[idim];
      }

    typename OutputImageType::RegionType outputLargestPossibleRegion;
    outputLargestPossibleRegion.SetSize(outputSize);
    outputLargestPossibleRegion.SetIndex(outputStartIndex);

    outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
    outputPtr->SetOrigin (outputOrigin);
    outputPtr->SetSpacing(outputSpacing);
    outputPtr->SetDirection(inputDirection);  //Output Direction should be same
                                              // as input.
    }
}

/*
 * GenerateOutputRequestedRegion
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
::GenerateOutputRequestedRegion(DataObject *refOutput)
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion(refOutput);

  // find the index for this output
  unsigned int refLevel = static_cast<unsigned int>( refOutput->GetSourceOutputIndex() );

  // compute baseIndex and baseSize
  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::RegionType RegionType;

  TOutputImage *ptr = itkDynamicCastInDebugMode< TOutputImage * >( refOutput );
  if ( !ptr )
    {
    itkExceptionMacro(<< "Could not cast refOutput to TOutputImage*.");
    }

  unsigned int ilevel, idim;

  if ( ptr->GetRequestedRegion() == ptr->GetLargestPossibleRegion() )
    {
    // set the requested regions for the other outputs to their
    // requested region

    for ( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
      {
      if ( ilevel == refLevel ) { continue; }
      if ( !this->GetOutput(ilevel) ) { continue; }
      this->GetOutput(ilevel)->SetRequestedRegionToLargestPossibleRegion();
      }
    }
  else
    {
    // compute requested regions for the other outputs based on
    // the requested region of the reference output
    IndexType  outputIndex;
    SizeType   outputSize;
    RegionType outputRegion;
    IndexType  baseIndex = ptr->GetRequestedRegion().GetIndex();
    SizeType   baseSize  = ptr->GetRequestedRegion().GetSize();

    for ( idim = 0; idim < TOutputImage::ImageDimension; idim++ )
      {
      unsigned int factor = m_Schedule[refLevel][idim];
      baseIndex[idim] *= static_cast< IndexValueType >( factor );
      baseSize[idim] *= static_cast< SizeValueType >( factor );
      }

    for ( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
      {
      if ( ilevel == refLevel ) { continue; }
      if ( !this->GetOutput(ilevel) ) { continue; }

      for ( idim = 0; idim < TOutputImage::ImageDimension; idim++ )
        {
        double factor = static_cast< double >( m_Schedule[ilevel][idim] );

        outputSize[idim] = static_cast< SizeValueType >(
          std::floor(static_cast< double >( baseSize[idim] ) / factor) );
        if ( outputSize[idim] < 1 ) { outputSize[idim] = 1; }

        outputIndex[idim] = static_cast< IndexValueType >(
          std::ceil(static_cast< double >( baseIndex[idim] ) / factor) );
        }

      outputRegion.SetIndex(outputIndex);
      outputRegion.SetSize(outputSize);

      // make sure the region is within the largest possible region
      outputRegion.Crop( this->GetOutput(ilevel)->
                         GetLargestPossibleRegion() );
      // set the requested region
      this->GetOutput(ilevel)->SetRequestedRegion(outputRegion);
      }
    }
}

/**
 * GenerateInputRequestedRegion
 */
template< typename TInputImage, typename TOutputImage >
void
MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
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

  unsigned int refLevel = m_NumberOfLevels - 1;
  SizeType     baseSize = this->GetOutput(refLevel)->GetRequestedRegion().GetSize();
  IndexType    baseIndex = this->GetOutput(refLevel)->GetRequestedRegion().GetIndex();
  RegionType   baseRegion;

  unsigned int idim;
  for ( idim = 0; idim < ImageDimension; idim++ )
    {
    unsigned int factor = m_Schedule[refLevel][idim];
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
                                       m_Schedule[refLevel][idim] ) ) );
    oper->SetMaximumError(m_MaximumError);
    oper->CreateDirectional();
    radius[idim] = oper->GetRadius()[idim];
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
