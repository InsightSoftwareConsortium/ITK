/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatchCardinalityImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMatchCardinalityImageToImageMetric_txx
#define _itkMatchCardinalityImageToImageMetric_txx

#include "itkMatchCardinalityImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/*
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::MatchCardinalityImageToImageMetric()
{
  itkDebugMacro("Constructor");

  this->SetComputeGradient(false); // don't use the default gradients
  m_MeasureMatches = true;  // default to measure percentage of pixel matches

  m_Threader = MultiThreader::New();
  m_NumberOfThreads = m_Threader->GetNumberOfThreads();
}

/*
 * Get the match Measure
 */
template <class TFixedImage, class TMovingImage> 
typename MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const TransformParametersType & parameters ) const
{
  return const_cast<Self*>(this)->GetNonconstValue(parameters);
}

/*
 * Get the match Measure (non const version. spawns threads).
 */
template <class TFixedImage, class TMovingImage> 
typename MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::GetNonconstValue( const TransformParametersType & parameters ) 
{
  itkDebugMacro("GetValue( " << parameters << " ) ");

  FixedImageConstPointer fixedImage = this->m_FixedImage;
  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  // Initialize some variables before spawning threads
  //
  //
  MeasureType measure = NumericTraits< MeasureType >::Zero;
  this->m_NumberOfPixelsCounted = 0;

  m_ThreadMatches.clear();
  m_ThreadCounts.clear();
  m_ThreadMatches.resize( this->GetNumberOfThreads() );
  m_ThreadCounts.resize( this->GetNumberOfThreads() );

  typename std::vector<MeasureType>::iterator mIt;
  std::vector<unsigned long>::iterator cIt;
  for (mIt = m_ThreadMatches.begin(), cIt = m_ThreadCounts.begin();
       mIt != m_ThreadMatches.end(); ++mIt, ++cIt)
    {
    *mIt = NumericTraits< MeasureType >::Zero;
    *cIt = 0;
    }

  // store the parameters in the transform so all threads can access them
  this->SetTransformParameters( parameters );


  // Set up the multithreaded processing
  //
  //
  ThreadStruct str;
  str.Metric = this;
  
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str);
  
  // multithread the execution
  //
  //
  this->GetMultiThreader()->SingleMethodExecute();
  
  // Collect the contribution to the metric for each thread
  //
  //
  for (mIt = m_ThreadMatches.begin(), cIt = m_ThreadCounts.begin();
       mIt != m_ThreadMatches.end(); ++mIt, ++cIt)
    {
    measure += *mIt;
    this->m_NumberOfPixelsCounted += *cIt;
    }


  if( !this->m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<<"All the points mapped to outside of the moving image");
    }
  else
    {
    measure /= this->m_NumberOfPixelsCounted;
    }

  return measure;
}

template <class TFixedImage, class TMovingImage> 
void
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::ThreadedGetValue( const FixedImageRegionType &regionForThread,
                    int threadId ) 
{
  FixedImageConstPointer fixedImage = this->GetFixedImage();
  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;
  typename FixedImageType::IndexType index;
  FixedIteratorType ti( fixedImage, regionForThread );

  MeasureType threadMeasure = NumericTraits< MeasureType >::Zero;
  unsigned long threadNumberOfPixelsCounted = 0;

  while(!ti.IsAtEnd())
    {
    index = ti.GetIndex();
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    if( this->GetFixedImageMask() && !this->GetFixedImageMask()->IsInside( inputPoint ) )
      {
      ++ti;
      continue;
      }

    typename Superclass::OutputPointType
      transformedPoint = this->GetTransform()->TransformPoint( inputPoint );

    if( this->GetMovingImageMask() && !this->GetMovingImageMask()->IsInside( transformedPoint ) )
      {
      ++ti;
      continue;
      }

    if( this->GetInterpolator()->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue= this->GetInterpolator()->Evaluate( transformedPoint );
      const RealType fixedValue = ti.Get();
      RealType diff;
      
      threadNumberOfPixelsCounted++;
      
      if (m_MeasureMatches)
        {
        diff = (movingValue == fixedValue); // count matches
        }
      else
        {
        diff = (movingValue != fixedValue); // count mismatches
        }
      threadMeasure += diff;
      }

    ++ti;
    }

  m_ThreadMatches[threadId] = threadMeasure;
  m_ThreadCounts[threadId] = threadNumberOfPixelsCounted;
}

//----------------------------------------------------------------------------
template <class TFixedImage, class TMovingImage> 
int 
MatchCardinalityImageToImageMetric<TFixedImage, TMovingImage>
::SplitFixedRegion(int i, int num, FixedImageRegionType& splitRegion)
{
  // Get the output pointer
  const typename FixedImageRegionType::SizeType& fixedRegionSize 
    = this->GetFixedImageRegion().GetSize();

  int splitAxis;
  typename FixedImageRegionType::IndexType splitIndex;
  typename FixedImageRegionType::SizeType splitSize;

  // Initialize the splitRegion to the output requested region
  splitRegion = this->GetFixedImageRegion();
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  // split on the outermost dimension available
  splitAxis = this->GetFixedImage()->GetImageDimension() - 1;
  while (fixedRegionSize[splitAxis] == 1)
    {
    --splitAxis;
    if (splitAxis < 0)
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
      }
    }

  // determine the actual number of pieces that will be generated
  typename FixedImageRegionType::SizeType::SizeValueType range = fixedRegionSize[splitAxis];
  int valuesPerThread = (int)::ceil(range/(double)num);
  int maxThreadIdUsed = (int)::ceil(range/(double)valuesPerThread) - 1;

  // Split the region
  if (i < maxThreadIdUsed)
    {
    splitIndex[splitAxis] += i*valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
    }
  if (i == maxThreadIdUsed)
    {
    splitIndex[splitAxis] += i*valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i*valuesPerThread;
    }
  
  // set the split region ivars
  splitRegion.SetIndex( splitIndex );
  splitRegion.SetSize( splitSize );

  itkDebugMacro("  Split Piece: " << splitRegion );

  return maxThreadIdUsed + 1;
}

// Callback routine used by the threading library. This routine just calls
// the ThreadedGenerateData method after setting the correct region for this
// thread. 
template <class TFixedImage, class TMovingImage> 
ITK_THREAD_RETURN_TYPE 
MatchCardinalityImageToImageMetric<TFixedImage, TMovingImage>
::ThreaderCallback( void *arg )
{
  ThreadStruct *str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;

  str = (ThreadStruct *)(((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // execute the actual method with appropriate computation region
  // first find out how many pieces extent can be split into.
  FixedImageRegionType splitRegion;
  total = str->Metric->SplitFixedRegion(threadId, threadCount, splitRegion);

  if (threadId < total)
    {
    str->Metric->ThreadedGetValue(splitRegion, threadId);
    }
  // else
  //   {
  //   otherwise don't use this thread. Sometimes the threads dont
  //   break up very well and it is just as efficient to leave a 
  //   few threads idle.
  //   }
  
  return ITK_THREAD_RETURN_VALUE;
}

/*
 * PrintSelf
 */
template <class TFixedImage, class TMovingImage> 
void
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "MeasureMatches: " << (m_MeasureMatches ? "On" : "Off")  << std::endl;
  os << indent << "NumberOfThreads: " << m_NumberOfThreads  << std::endl;
}

} // end namespace itk


#endif
