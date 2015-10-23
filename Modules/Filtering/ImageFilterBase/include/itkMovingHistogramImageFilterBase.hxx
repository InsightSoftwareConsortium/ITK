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
#ifndef itkMovingHistogramImageFilterBase_hxx
#define itkMovingHistogramImageFilterBase_hxx

#include "itkMovingHistogramImageFilterBase.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkNumericTraits.h"
#include "itkImageRegionIterator.h"
#include "itkImageLinearConstIteratorWithIndex.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
MovingHistogramImageFilterBase< TInputImage, TOutputImage, TKernel >
::MovingHistogramImageFilterBase()
{
  m_PixelsPerTranslation = 0;
  // call again SetKernel in that class, to have the offsets computed for
  // the default kernel. An AnalyzeKernel() method as in the binary
  // morphology class may be a better way to go.
  this->SetKernel( this->GetKernel() );
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MovingHistogramImageFilterBase< TInputImage, TOutputImage, TKernel >
::SetKernel(const KernelType & kernel)
{
  // first, build the list of offsets of added and removed pixels when the
  // structuring element move of 1 pixel on 1 axis; do it for the 2 directions
  // on each axes.

  // transform the structuring element in an image for an easier
  // access to the data
  typedef Image< bool, TInputImage::ImageDimension > BoolImageType;
  typename BoolImageType::Pointer tmpSEImage = BoolImageType::New();
  tmpSEImage->SetRegions( kernel.GetSize() );
  tmpSEImage->Allocate();
  RegionType                                    tmpSEImageRegion = tmpSEImage->GetRequestedRegion();
  ImageRegionIteratorWithIndex< BoolImageType > kernelImageIt;
  kernelImageIt = ImageRegionIteratorWithIndex< BoolImageType >(tmpSEImage, tmpSEImageRegion);
  kernelImageIt.GoToBegin();
  KernelIteratorType kernel_it = kernel.Begin();
  OffsetListType     kernelOffsets;

  // create a center index to compute the offset
  IndexType centerIndex;
  for ( unsigned axis = 0; axis < ImageDimension; axis++ )
    {
    centerIndex[axis] = kernel.GetSize()[axis] / 2;
    }

  SizeValueType count = 0;
  while ( !kernelImageIt.IsAtEnd() )
    {
    // TODO: be sure that removing that comparison doesn't break backward
    // compatibility
    // only to fix msvc warning C4804: '>' : unsafe use of type 'bool' in
    // operation
    // kernelImageIt.Set( *kernel_it > 0 );
    //
    kernelImageIt.Set(*kernel_it);
    // if( *kernel_it > 0 )
    if ( *kernel_it )
      {
      kernelImageIt.Set(true);
      kernelOffsets.push_front(kernelImageIt.GetIndex() - centerIndex);
      count++;
      }
    else
      {
      kernelImageIt.Set(false);
      }
    ++kernelImageIt;
    ++kernel_it;
    }

  // verify that the kernel contain at least one point
  if ( count == 0 )
    {
    itkExceptionMacro(<< "The kernel must contain at least one point.");
    }

  // no attribute should be modified before here to avoid setting the filter in
  // a bad status
  // store the kernel !!
  Superclass::SetKernel(kernel);

  // clear the already stored values
  m_AddedOffsets.clear();
  m_RemovedOffsets.clear();
  //m_Axes

  // store the kernel offset list
  m_KernelOffsets = kernelOffsets;

  FixedArray< SizeValueType, ImageDimension > axisCount;
  axisCount.Fill(0);

  for ( unsigned axis = 0; axis < ImageDimension; axis++ )
    {
    OffsetType refOffset;
    refOffset.Fill(0);
    for ( int direction = -1; direction <= 1; direction += 2 )
      {
      refOffset[axis] = direction;
      for ( kernelImageIt.GoToBegin(); !kernelImageIt.IsAtEnd(); ++kernelImageIt )
        {
        IndexType idx = kernelImageIt.GetIndex();

        if ( kernelImageIt.Get() )
          {
          // search for added pixel during a translation
          IndexType nextIdx = idx + refOffset;
          if ( tmpSEImageRegion.IsInside(nextIdx) )
            {
            if ( !tmpSEImage->GetPixel(nextIdx) )
              {
              m_AddedOffsets[refOffset].push_front(nextIdx - centerIndex);
              axisCount[axis]++;
              }
            }
          else
            {
            m_AddedOffsets[refOffset].push_front(nextIdx - centerIndex);
            axisCount[axis]++;
            }
          // search for removed pixel during a translation
          IndexType prevIdx = idx - refOffset;
          if ( tmpSEImageRegion.IsInside(prevIdx) )
            {
            if ( !tmpSEImage->GetPixel(prevIdx) )
              {
              m_RemovedOffsets[refOffset].push_front(idx - centerIndex);
              axisCount[axis]++;
              }
            }
          else
            {
            m_RemovedOffsets[refOffset].push_front(idx - centerIndex);
            axisCount[axis]++;
            }
          }
        }
      }
    }

  // search for the best axis
  typedef typename std::set< DirectionCost > MapCountType;
  MapCountType invertedCount;
  unsigned int i;
  for ( i = 0; i < ImageDimension; i++ )
    {
    invertedCount.insert( DirectionCost(i, axisCount[i]) );
    }

  i = 0;
  for ( typename MapCountType::iterator it = invertedCount.begin(); it != invertedCount.end(); it++, i++ )
    {
    m_Axes[i] = it->m_Dimension;
    }

  m_PixelsPerTranslation = axisCount[m_Axes[ImageDimension - 1]] / 2;  //
                                                                       // divided
                                                                       // by 2
                                                                       // because
                                                                       // there
                                                                       // is 2
                                                                       // directions
                                                                       // on the
                                                                       // axis
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MovingHistogramImageFilterBase< TInputImage, TOutputImage, TKernel >
::GetDirAndOffset(const IndexType LineStart,
                  const IndexType PrevLineStart,
                  OffsetType & LineOffset,
                  OffsetType & Changes,
                  int & LineDirection)
{
  // when moving between lines in the same plane there should be only
  // 1 non zero (positive) entry in LineOffset.
  // When moving between planes there will be some negative ones too.
  LineOffset = Changes = LineStart - PrevLineStart;
  for ( unsigned int y = 0; y < ImageDimension; y++ )
    {
    if ( LineOffset[y] > 0 )
      {
      LineOffset[y] = 1;  // should be 1 anyway
      LineDirection = y;
      }
    else
      {
      LineOffset[y] = 0;
      }
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MovingHistogramImageFilterBase< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PixelsPerTranslation: " << m_PixelsPerTranslation << std::endl;
}
} // end namespace itk
#endif
