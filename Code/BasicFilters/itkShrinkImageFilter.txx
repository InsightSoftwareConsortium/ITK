/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShrinkImageFilter_txx
#define __itkShrinkImageFilter_txx

#include "itkShrinkImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkContinuousIndex.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ShrinkImageFilter<TInputImage,TOutputImage>
::ShrinkImageFilter()
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ShrinkFactors[j] = 1;
    }
}
  

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Shrink Factor: ";
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    os << m_ShrinkFactors[j] << " ";
    } 
  os << std::endl;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::SetShrinkFactors(unsigned int factors[])
{
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    if( factors[j] != m_ShrinkFactors[j] ) break;
    }
  if( j < ImageDimension )
    {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
      {
      m_ShrinkFactors[j] = factors[j];
      if( m_ShrinkFactors[j] < 1 ) 
        {
        m_ShrinkFactors[j] = 1;
        }
      }
    }
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::SetShrinkFactors(unsigned int factor)
{
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    if( factor != m_ShrinkFactors[j] ) break;
    }
  if( j < ImageDimension )
    {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
      {
      m_ShrinkFactors[j] = factor;
      if( m_ShrinkFactors[j] < 1 ) 
        {
        m_ShrinkFactors[j] = 1;
        }
      }
    }
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImageConstPointer  inputPtr = this->GetInput();
  OutputImagePointer      outputPtr = this->GetOutput();
  
  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef ImageRegionIteratorWithIndex<TOutputImage> OutputIterator;
  OutputIterator outIt(outputPtr, outputRegionForThread);
  
  // Define a few indices that will be used to transform from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType   outputIndex;
  typename TInputImage::IndexType    inputIndex;
  typename TOutputImage::OffsetType  offsetIndex;
  
  typename TOutputImage::PointType outputPoint;

  // convert the time for convenient multiplication
  typename TOutputImage::SizeType  factorSize;
  for (unsigned int i=0; i < TInputImage::ImageDimension; i++)
    {
    factorSize[i] = m_ShrinkFactors[i];
    }

  // use this index to compute the offset
  outputIndex = outputPtr->GetLargestPossibleRegion().GetIndex();
  inputIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  // We wish to perform the following mapping of outputIndex to
  // inputIndex on all point in our region
  outputPtr->TransformIndexToPhysicalPoint(outputIndex, outputPoint);
  inputPtr->TransformPhysicalPointToIndex(outputPoint, inputIndex);

  // Given that the size is scaled by a constant factor eq:
  // inputIndex = outputIndex * factorSize 
  // is equivalent up to a fixed offset which we now compute
  offsetIndex = inputIndex - outputIndex*factorSize;

    
  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  while ( !outIt.IsAtEnd() ) 
    {
    // determine the index and physical location of the output pixel
    outputIndex = outIt.GetIndex();

    // an optimized version  has a segfault bug
    //inputIndex = outputIndex * factorSize + offsetIndex;

    // determine the input pixel index associated with this output pixel 
    outputPtr->TransformIndexToPhysicalPoint(outputIndex, outputPoint);
    inputPtr->TransformPhysicalPointToIndex(outputPoint, inputIndex);

    // copy the input pixel to the output
    outIt.Set( inputPtr->GetPixel(inputIndex) );
    ++outIt;

    progress.CompletedPixel();
    }
}

/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = const_cast<TInputImage *> (this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  
  // Compute the input requested region (size and start index)
  // Use the image transformations to insure an input requested region
  // that will provide the proper range
  unsigned int i;
  const typename TOutputImage::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();

  typename TOutputImage::IndexType outputIndex0, outputIndex1;
  typename TInputImage::IndexType  inputIndex0,  inputIndex1;
  typename TInputImage::SizeType   inputSize;
  typename TOutputImage::PointType outputPoint;

  outputIndex0 = outputRequestedRegionStartIndex;
  for (i = 0; i < TInputImage::ImageDimension; i++)
    {
    outputIndex1[i] = outputRequestedRegionStartIndex[i] +
      outputRequestedRegionSize[i] - 1;
    }
  // Find the bounds of the input region by transforming the bounds of
  // the output region
  outputPtr->TransformIndexToPhysicalPoint(outputIndex0, outputPoint);
  inputPtr->TransformPhysicalPointToIndex(outputPoint, inputIndex0);
  outputPtr->TransformIndexToPhysicalPoint(outputIndex1, outputPoint);
  inputPtr->TransformPhysicalPointToIndex(outputPoint, inputIndex1);

  for (i = 0; i < TInputImage::ImageDimension; i++)
    {
    inputSize[i] = inputIndex1[i] - inputIndex0[i] + 1;
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetIndex( inputIndex0 );
  inputRequestedRegion.SetSize( inputSize );
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );

  inputPtr->SetRequestedRegion( inputRequestedRegion );
}

/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();
  
  // get pointers to the input and output
  InputImageConstPointer  inputPtr  = this->GetInput();
  OutputImagePointer      outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  
  // we need to compute the output spacing, the output image size, and the
  // output image start index
  unsigned int i;
  const typename TInputImage::SpacingType&
    inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType&  inputStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TOutputImage::SpacingType  outputSpacing;
  typename TOutputImage::SizeType     outputSize;
  typename TOutputImage::IndexType    outputStartIndex;
  
  for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
    
    outputSpacing[i] = inputSpacing[i] * (float) m_ShrinkFactors[i];
    outputSize[i] = (unsigned long)
      vcl_floor((float) inputSize[i] / (float) m_ShrinkFactors[i]);
    if( outputSize[i] < 1 )
      {
      outputSize[i] = 1;
      }
    
    outputStartIndex[i] = (long)
      vcl_ceil((float) inputStartIndex[i] / (float) m_ShrinkFactors[i] );
    }
  
  outputPtr->SetSpacing( outputSpacing );

  // The physical center's of the input and output should be the same
  ContinuousIndex<double, TOutputImage::ImageDimension> inputCenterIndex;
  ContinuousIndex<double, TOutputImage::ImageDimension> outputCenterIndex;
  for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
    inputCenterIndex[i] = inputStartIndex[i] + (inputSize[i] - 1) / 2.0;
    outputCenterIndex[i] = outputStartIndex[i] + (outputSize[i] - 1) / 2.0;
    }

  typename TOutputImage::PointType inputCenterPoint;
  typename TOutputImage::PointType outputCenterPoint;
  inputPtr->TransformContinuousIndexToPhysicalPoint(inputCenterIndex, inputCenterPoint);
  outputPtr->TransformContinuousIndexToPhysicalPoint(outputCenterIndex, outputCenterPoint);
  
  typename TOutputImage::PointType outputOrigin = outputPtr->GetOrigin();
  outputOrigin = outputOrigin + (inputCenterPoint - outputCenterPoint);
  outputPtr->SetOrigin(outputOrigin);

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
}

} // end namespace itk

#endif
