/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionImagePyramid.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkMultiResolutionImagePyramid_txx
#define _itkMultiResolutionImagePyramid_txx

#include "itkMultiResolutionImagePyramid.h"
#include "itkCastImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_math.h"

namespace itk
{


/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::MultiResolutionImagePyramid()
{
  this->SetNumberOfLevels( 2 );
  m_CurrentLevel = 0;
  m_MaximumError = 0.1;
}


/**
 * Set the current level
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::SetCurrentLevel(
unsigned int level )
{

  if( level == m_CurrentLevel ) return;

  this->Modified();

  // clamp current level to be between 0 and NumberOfLevels - 1
  if( level > this->GetNumberOfLevels() - 1 )
    {
    m_CurrentLevel = this->GetNumberOfLevels() - 1;
    }
  else
    {
    m_CurrentLevel = level;
    }

}


/**
 * Set the number of computation levels
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::SetNumberOfLevels(
unsigned int num )
{
  // clamp value to be at least one
  m_NumberOfLevels = num;
  if( m_NumberOfLevels < 1 ) m_NumberOfLevels = 1;

  // resize the schedules
  m_Schedule.resize( m_NumberOfLevels, ImageDimension );

  // determine initial shrink factor
  unsigned int startfactor = 1;
  startfactor = startfactor << ( m_NumberOfLevels - 1 );

  // set the starting shrink factors
  this->SetStartingShrinkFactors( startfactor );

  this->Modified();

}


/**
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
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


/**
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
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


/**
 * Set the multi-resolution schedule
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
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
      if( m_Schedule[level][dim] == 0 )
        {
        m_Schedule[level][dim] = 1;
        }
      }
    }
}


/**
 * GenerateData
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::GenerateData()
{

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Allocate output memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Typedefs for the mini-pipeline
  typedef CastImageFilter<TInputImage, TOutputImage> CasterType;
  typedef ShrinkImageFilter<TOutputImage, TOutputImage> ShrinkerType;
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef GaussianOperator<OutputPixelType,ImageDimension> OperatorType;
  typedef NeighborhoodOperatorImageFilter<TOutputImage,TOutputImage>
    SmootherType;

  Vector<unsigned int, ImageDimension> shrinkFactors;
  shrinkFactors = m_Schedule[m_CurrentLevel];

  Vector<unsigned int,ImageDimension> tempFactors;
  double variance;

  typedef typename TOutputImage::RegionType RegionType;
  RegionType reqRegion = inputPtr->GetRequestedRegion();
  typename TOutputImage::SizeType reqSize = reqRegion.GetSize();
  typename TOutputImage::IndexType reqIndex = reqRegion.GetIndex(); 
  RegionType outputRegion = outputPtr->GetRequestedRegion();

  typename SmootherType::Pointer smoother = SmootherType::New();
  typename ShrinkerType::Pointer shrinker = ShrinkerType::New();
  typename CasterType::Pointer caster = CasterType::New();
  OperatorType *oper = new OperatorType;

  OutputImagePointer tempImagePtr;  // Pointer to the temporary solution  
  bool firstFilter = true;
 
  for( int j = 0; j < ImageDimension; j++ )
    {

    this->UpdateProgress( (float)j / (float)ImageDimension );

    // keep track of what we really need to be updated
    reqSize[j] = outputRegion.GetSize()[j];
    reqIndex[j] = outputRegion.GetIndex()[j];
    reqRegion.SetSize( reqSize );
    reqRegion.SetIndex( reqIndex );

    if( shrinkFactors[j] == 1 )
      {
      continue;
      }

    tempFactors.Fill( 1 );
    tempFactors[j] = shrinkFactors[j];
    variance = vnl_math_sqr( 0.5 * (double) shrinkFactors[j] );

    oper->SetDirection( j );
    oper->SetVariance( variance );
    oper->SetMaximumError( m_MaximumError ); // need not be too accurate
    oper->CreateDirectional();

    smoother->SetOperator( *oper );

    shrinker->SetShrinkFactors( tempFactors.Begin() );

    /**
     *
     * Create mini-pipline: [caster]-> smoother-> shrinker
     *
     * If this is the first dimension processed,
     * a caster is required to convert the input image 
     * to the output image type.
     *
     * Otherwise, the temporary solution is used as
     * input to the smoother.
     */
    if( firstFilter )
      {
      caster->SetInput( inputPtr );
      smoother->SetInput( caster->GetOutput() );
      firstFilter = false;
      }
    else
      {
      smoother->SetInput( tempImagePtr );
      }

    shrinker->SetInput( smoother->GetOutput() );

    // make sure we only update what we really need
    shrinker->UpdateOutputInformation();
    shrinker->GetOutput()->SetRequestedRegion( reqRegion );
    shrinker->GetOutput()->PropagateRequestedRegion();

    if( j < ImageDimension - 1 )
      {
      
      shrinker->GetOutput()->UpdateOutputData();

      tempImagePtr = shrinker->GetOutput();
      tempImagePtr->DisconnectPipeline();

      }
    else
      {

      shrinker->GraftOutput( outputPtr );
      shrinker->GetOutput()->UpdateOutputData();
      this->GraftOutput( shrinker->GetOutput() );

      }

    }

    delete oper;

   if( firstFilter )
    {
    // all shrink factors were ones - just cast the input
    // image to the right output type
    caster->SetInput( inputPtr );
    caster->GraftOutput( outputPtr );
    caster->Update();
    this->GraftOutput( caster->GetOutput() );

    }

}


/**
 * PrintSelf method
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "No. levels: " << m_NumberOfLevels << std::endl;
  os << indent << "Schedule: " << std::endl;
  os << m_Schedule << std::endl;

}


/** 
 * GenerateOutputInformation
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::GenerateOutputInformation()
{

  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImagePointer inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  
  // we need to compute the output spacing, the output image size,
  // and the output image start index
  Vector<unsigned int,ImageDimension> shrinkFactors;
  shrinkFactors = m_Schedule[m_CurrentLevel];

  int i;
  const double * inputSpacing = inputPtr->GetSpacing();
  const typename InputImageType::SizeType& inputSize = 
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType& inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  float outputSpacing[OutputImageType::ImageDimension];
  typename OutputImageType::SizeType    outputSize;
  typename OutputImageType::IndexType   outputStartIndex;

  for( i = 0; i < OutputImageType::ImageDimension; i++ )
    {
    outputSpacing[i] = inputSpacing[i] * (float) shrinkFactors[i];
    outputSize[i] = (unsigned long)
      floor( (float) inputSize[i] / (float) shrinkFactors[i] );
    if( outputSize[i] < 1 ) outputSize[i] = 1;

    outputStartIndex[i] = (long)
      ceil( (float) inputStartIndex[i] / (float) shrinkFactors[i] );
    }
  
  typename OutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
  outputPtr->SetSpacing( outputSpacing );
}


/** 
 * GenerateInputRequestedRegion
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  
  // we need to compute the input requested region
  Vector<unsigned int,ImageDimension> shrinkFactors;
  shrinkFactors = m_Schedule[m_CurrentLevel];

  int i;
  const typename TOutputImage::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();
  
  typename TInputImage::SizeType  inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;
  
  // compute requirements for the shrinkage part
  for (i = 0; i < TInputImage::ImageDimension; i++)
    {
    inputRequestedRegionSize[i]
      = outputRequestedRegionSize[i] * shrinkFactors[i];
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] * (long)shrinkFactors[i];
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize( inputRequestedRegionSize );
  inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );

  // compute requirements for the smoothing part
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef GaussianOperator<OutputPixelType,ImageDimension> OperatorType;

  unsigned long radius[ImageDimension];
  for( i = 0; i < TInputImage::ImageDimension; i++ )
    {
    OperatorType *oper = new OperatorType;
    oper->SetDirection(i);
    oper->SetVariance( vnl_math_sqr( 0.5 * (double) shrinkFactors[i] ) );
    oper->SetMaximumError( m_MaximumError );
    oper->CreateDirectional();
    radius[i] = oper->GetRadius()[i];
    delete oper;
    }

  inputRequestedRegion.PadByRadius( radius );

  // make sure the requested region is within the largest possible
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );
  
  // set the input requested region
  inputPtr->SetRequestedRegion( inputRequestedRegion );

}


} // namespace itk

#endif
