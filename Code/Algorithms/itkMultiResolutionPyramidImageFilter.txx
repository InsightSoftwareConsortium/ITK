/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionPyramidImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMultiResolutionPyramidImageFilter_txx
#define _itkMultiResolutionPyramidImageFilter_txx

#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkCastImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_math.h"

namespace itk
{


/*
 * Constructor
 */
template <class TInputImage, class TOutputImage>
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::MultiResolutionPyramidImageFilter()
{
  m_NumberOfLevels = 0;
  this->SetNumberOfLevels( 2 );
  m_MaximumError = 0.1;
}


/*
 * Set the number of computation levels
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::SetNumberOfLevels(
  unsigned int num )
{
  if( m_NumberOfLevels == num )
    { 
    return; 
    }
  
  this->Modified();

  // clamp value to be at least one
  m_NumberOfLevels = num;
  if( m_NumberOfLevels < 1 ) m_NumberOfLevels = 1;

  // resize the schedules
  ScheduleType temp( m_NumberOfLevels, ImageDimension );
  temp.Fill( 0 );
  m_Schedule = temp;

  // determine initial shrink factor
  unsigned int startfactor = 1;
  startfactor = startfactor << ( m_NumberOfLevels - 1 );

  // set the starting shrink factors
  this->SetStartingShrinkFactors( startfactor );

  // set the required number of outputs
  this->SetNumberOfRequiredOutputs( m_NumberOfLevels );

  unsigned int numOutputs = static_cast<unsigned int>( this->GetNumberOfOutputs() );
  unsigned int idx;
  if( numOutputs < m_NumberOfLevels )
    {
    // add extra outputs
    for( idx = numOutputs; idx < m_NumberOfLevels; idx++ )
      {
      typename DataObject::Pointer output =
        this->MakeOutput( idx );
      this->SetNthOutput( idx, output.GetPointer() );
      }

    }
  else if( numOutputs > m_NumberOfLevels )
    {
    // remove extra outputs
    for( idx = m_NumberOfLevels; idx < numOutputs; idx++ )
      {
      typename DataObject::Pointer output = 
        this->GetOutputs()[idx];
      this->RemoveOutput( output );
      }
    }

}


/*
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::SetStartingShrinkFactors(
  unsigned int factor )
{

  unsigned int array[ImageDimension];
  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    array[dim] = factor;
    }

  this->SetStartingShrinkFactors( array );

}


/*
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::SetStartingShrinkFactors(
  unsigned int * factors )
{

  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    m_Schedule[0][dim] = factors[dim];
    if( m_Schedule[0][dim] == 0 ) 
      {
      m_Schedule[0][dim] = 1;
      }
    }

  for( unsigned int level = 1; level < m_NumberOfLevels; ++level )
    {
    for( unsigned int dim = 0; dim < ImageDimension; ++dim )
      {
      m_Schedule[level][dim] = m_Schedule[level-1][dim] / 2;
      if( m_Schedule[level][dim] == 0 ) 
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
template <class TInputImage, class TOutputImage>
const unsigned int *
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GetStartingShrinkFactors() const
{
  return ( m_Schedule.data_block() );
}


/*
 * Set the multi-resolution schedule
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::SetSchedule(
  const ScheduleType& schedule )
{

  if( schedule.rows() != m_NumberOfLevels ||
      schedule.columns() != ImageDimension )
    {
    itkDebugMacro(<< "Schedule has wrong dimensions" );
    return;
    }

  if( schedule == m_Schedule )
    {
    return;
    }

  this->Modified();
  unsigned int level, dim;
  for( level = 0; level < m_NumberOfLevels; level++ )
    {
    for( dim = 0; dim < ImageDimension; dim++ )
      {

      m_Schedule[level][dim] = schedule[level][dim];
 
      // set schedule to max( 1, min(schedule[level],
      //  schedule[level-1] ); 
      if( level > 0 )
        {
        m_Schedule[level][dim] = vnl_math_min(
          m_Schedule[level][dim], m_Schedule[level-1][dim] );
        }
   
      if( m_Schedule[level][dim] < 1 )
        {
        m_Schedule[level][dim] = 1;
        }

      }
    }
}


/*
 * Is the schedule downward divisible ?
 */
template <class TInputImage, class TOutputImage>
bool
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::IsScheduleDownwardDivisible( const ScheduleType& schedule )
{

  unsigned int ilevel, idim;
  for( ilevel = 0; ilevel < schedule.rows() - 1; ilevel++ )
    {
    for( idim = 0; idim < schedule.columns(); idim++ )
      {
      if( schedule[ilevel][idim] == 0 )
        {
        return false;
        }
      if( ( schedule[ilevel][idim] % schedule[ilevel+1][idim] ) > 0 )
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
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GenerateData()
{

  // Get the input and output pointers
  InputImageConstPointer  inputPtr = this->GetInput();

  // Create caster, smoother and shrinker filters
  typedef CastImageFilter<TInputImage, TOutputImage> CasterType;
  typedef DiscreteGaussianImageFilter<TOutputImage, TOutputImage> SmootherType;
  typedef ShrinkImageFilter<TOutputImage,TOutputImage> ShrinkerType;

  typename CasterType::Pointer caster = CasterType::New();
  typename SmootherType::Pointer smoother = SmootherType::New();
  typename ShrinkerType::Pointer shrinker = ShrinkerType::New();

  // Setup the filters
  caster->SetInput( inputPtr );

  smoother->SetInput( caster->GetOutput() );
  smoother->SetMaximumError( m_MaximumError );

  shrinker->SetInput( smoother->GetOutput() );

  unsigned int ilevel, idim;
  unsigned int factors[ImageDimension];
  double       variance[ImageDimension];

  for( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
    {

    this->UpdateProgress( static_cast<float>( ilevel ) /
                          static_cast<float>( m_NumberOfLevels ) );

    // Allocate memory for each output
    OutputImagePointer outputPtr = this->GetOutput( ilevel );
    outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
    outputPtr->Allocate();

    // compute shrink factors and variances
    for( idim = 0; idim < ImageDimension; idim++ )
      {
      factors[idim] = m_Schedule[ilevel][idim];
      variance[idim] = vnl_math_sqr( 0.5 * 
                                     static_cast<float>( factors[idim] ) );
      }

    // use mini-pipeline to compute output
    smoother->SetVariance( variance );
    shrinker->SetShrinkFactors( factors );

    shrinker->GraftOutput( outputPtr );

    // force to always update in case shrink factors are the same
    shrinker->Modified();
    shrinker->Update();
/*
    // ensure only the requested region is updated
    shrinker->GetOutput()->UpdateOutputInformation();
    shrinker->GetOutput()->SetRequestedRegion(outputPtr->GetRequestedRegion());
    shrinker->GetOutput()->PropagateRequestedRegion();
    shrinker->GetOutput()->UpdateOutputData();
*/
   
    this->GraftNthOutput( ilevel, shrinker->GetOutput() );

    }


}


/*
 * PrintSelf method
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "No. levels: " << m_NumberOfLevels << std::endl;
  os << indent << "Schedule: " << std::endl;
  os << m_Schedule << std::endl;

}


/* 
 * GenerateOutputInformation
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GenerateOutputInformation()
{

  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer inputPtr = this->GetInput();

  if ( !inputPtr  )
    {
    itkExceptionMacro( << "Input has not been set" );
    }

  const double * inputSpacing = inputPtr->GetSpacing();
  const typename InputImageType::SizeType& inputSize = 
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType& inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typedef typename OutputImageType::SizeType SizeType;
  typedef typename SizeType::SizeValueType   SizeValueType;
  typedef typename OutputImageType::IndexType IndexType;
  typedef typename IndexType::IndexValueType  IndexValueType;

  OutputImagePointer outputPtr;
  double outputSpacing[OutputImageType::ImageDimension];
  SizeType    outputSize;
  IndexType   outputStartIndex;
  double shrinkFactor;
  
  // we need to compute the output spacing, the output image size,
  // and the output image start index
  unsigned int ilevel, idim;
  for( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
    {

    OutputImagePointer outputPtr = this->GetOutput( ilevel );
    if( !outputPtr ) { continue; }

    for( idim = 0; idim < OutputImageType::ImageDimension; idim++ )
      {

      shrinkFactor = static_cast<double>( m_Schedule[ilevel][idim] );

      outputSpacing[idim] = inputSpacing[idim] * shrinkFactor;

      outputSize[idim] = static_cast<SizeValueType>(
        floor( static_cast<double>(inputSize[idim]) / shrinkFactor ) );
      if( outputSize[idim] < 1 ) { outputSize[idim] = 1; }

      outputStartIndex[idim] = static_cast<IndexValueType>(
        ceil( static_cast<double>(inputStartIndex[idim]) / shrinkFactor ) );

      }
  
    typename OutputImageType::RegionType outputLargestPossibleRegion;
    outputLargestPossibleRegion.SetSize( outputSize );
    outputLargestPossibleRegion.SetIndex( outputStartIndex );

    outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
    outputPtr->SetSpacing( outputSpacing );

    }

}


/* 
 * GenerateOutputRequestedRegion
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GenerateOutputRequestedRegion(DataObject * refOutput )
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion( refOutput );

  // find the index for this output
  unsigned int refLevel = refOutput->GetSourceOutputIndex();

  // compute baseIndex and baseSize
  typedef typename OutputImageType::SizeType    SizeType;
  typedef typename SizeType::SizeValueType      SizeValueType;
  typedef typename OutputImageType::IndexType   IndexType;
  typedef typename IndexType::IndexValueType    IndexValueType;
  typedef typename OutputImageType::RegionType  RegionType;

  TOutputImage * ptr = static_cast<TOutputImage*>( refOutput );
  if( !ptr )
    {
    itkExceptionMacro( << "Could not cast refOutput to TOutputImage*." );
    }

  unsigned int ilevel, idim;

  if ( ptr->GetRequestedRegion() == ptr->GetLargestPossibleRegion() )
    {

    // set the requested regions for the other outputs to their 
    // requested region

    for( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
      {
      if( ilevel == refLevel ) { continue; }
      if( !this->GetOutput(ilevel) ) { continue; }
    
      this->GetOutput(ilevel)->SetRequestedRegionToLargestPossibleRegion();      
      }

    }
  else
    {

    // compute requested regions for the other outputs based on
    // the requested region of the reference output

    IndexType outputIndex;
    SizeType  outputSize;
    RegionType outputRegion;

    IndexType  baseIndex = ptr->GetRequestedRegion().GetIndex();
    SizeType   baseSize  = ptr->GetRequestedRegion().GetSize();


    for( idim = 0; idim < TOutputImage::ImageDimension; idim++ )
      {
      unsigned int factor = m_Schedule[refLevel][idim];
      baseIndex[idim] *= static_cast<IndexValueType>( factor );
      baseSize[idim]  *= static_cast<SizeValueType>( factor );
      }

    for( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
      {
      if( ilevel == refLevel ) { continue; }
      if( !this->GetOutput(ilevel) ) { continue; }

      for( idim = 0; idim < TOutputImage::ImageDimension; idim++ )
        {

        double factor = static_cast<double>( m_Schedule[ilevel][idim] );

        outputSize[idim] = static_cast<SizeValueType>(
          floor( static_cast<double>(baseSize[idim]) / factor ) );
        if( outputSize[idim] < 1 ) { outputSize[idim] = 1; } 

        outputIndex[idim] = static_cast<IndexValueType>(
          ceil( static_cast<double>(baseIndex[idim]) / factor ) );      

        }

      outputRegion.SetIndex( outputIndex );
      outputRegion.SetSize( outputSize );

      // make sure the region is within the largest possible region
      outputRegion.Crop( this->GetOutput( ilevel )->
                         GetLargestPossibleRegion() );
      // set the requested region
      this->GetOutput( ilevel )->SetRequestedRegion( outputRegion );
      }

    }


}


/* 
 * GenerateInputRequestedRegion
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
    const_cast< InputImageType * >( this->GetInput() );
  if ( !inputPtr )
    {
    itkExceptionMacro( << "Input has not been set." );
    }

  // compute baseIndex and baseSize
  typedef typename OutputImageType::SizeType    SizeType;
  typedef typename SizeType::SizeValueType      SizeValueType;
  typedef typename OutputImageType::IndexType   IndexType;
  typedef typename IndexType::IndexValueType    IndexValueType;
  typedef typename OutputImageType::RegionType  RegionType;

  unsigned int refLevel = m_NumberOfLevels - 1;
  SizeType baseSize = this->GetOutput(refLevel)->GetRequestedRegion().GetSize();
  IndexType baseIndex = this->GetOutput(refLevel)->GetRequestedRegion().GetIndex();
  RegionType baseRegion;

  unsigned int idim;
  for( idim = 0; idim < ImageDimension; idim++ )
    {
    unsigned int factor = m_Schedule[refLevel][idim];
    baseIndex[idim] *= static_cast<IndexValueType>( factor );
    baseSize[idim]  *= static_cast<SizeValueType>( factor );    
    }
  baseRegion.SetIndex( baseIndex );
  baseRegion.SetSize( baseSize );

  // compute requirements for the smoothing part
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef GaussianOperator<OutputPixelType,ImageDimension> OperatorType;

  OperatorType *oper = new OperatorType;
  unsigned long radius[ImageDimension];
  RegionType inputRequestedRegion = baseRegion;
  refLevel = 0;

  for( idim = 0; idim < TInputImage::ImageDimension; idim++ )
    {
    oper->SetDirection(idim);
    oper->SetVariance( vnl_math_sqr( 0.5 * static_cast<float>(
                                       m_Schedule[refLevel][idim] ) ) );
    oper->SetMaximumError( m_MaximumError );
    oper->CreateDirectional();
    radius[idim] = oper->GetRadius()[idim];
    }
  delete oper;

  inputRequestedRegion.PadByRadius( radius );

  // make sure the requested region is within the largest possible
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );
  
  // set the input requested region
  inputPtr->SetRequestedRegion( inputRequestedRegion );

}


} // namespace itk

#endif
