/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorUtilities.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorUtilities_txx
#define __itkAnchorUtilities_txx

#include "itkAnchorUtilities.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk {

/**
* \class AnchorUtilities 
 * \brief functionality in common for anchor openings/closings and
 * erosions/dilation
 *
**/
#if 0
template <class TImage, class TBres, class TLine>
int FillLineBuffer(typename TImage::ConstPointer input,
                   const typename TImage::IndexType StartIndex,
                   const TLine line,  // unit vector
                   const float tol,
                   const typename TBres::OffsetArray LineOffsets,
                   const typename TImage::RegionType AllImage, 
                   typename TImage::PixelType * inbuffer,
                   unsigned &start,
                   unsigned &end)
{

//   if (AllImage.IsInside(StartIndex))
//     {
//     start = 0;
//     }
//   else
  
#if 0
  // we need to figure out where to start
  // this is an inefficient way we'll use for testing
  for (start=0;start < LineOffsets.size();++start)
    {
    if (AllImage.IsInside(StartIndex + LineOffsets[start])) break;
    }
#else
  int status = ComputeStartEnd<TImage, TBres, TLine>(StartIndex, line, tol, LineOffsets, AllImage,
                                                     start, end);
  if (!status) return(status);
#endif
#if 1
  unsigned size = end - start + 1;
  // compat
  for (unsigned i = 0; i < size;i++)
    {
    inbuffer[i+1] = input->GetPixel(StartIndex + LineOffsets[start + i]);
    }
#else
  typedef ImageRegionConstIteratorWithIndex<TImage> ItType;
  ItType it(input, AllImage);
  it.SetIndex(StartIndex);
  for (unsigned i = 0; i < lastPos;i++)
    {
    inbuffer[i]= it.Get();
    typename TImage::IndexType I = StartIndex + LineOffsets[i];
    typename TImage::OffsetType Off = I - it.GetIndex();
    it += Off;
    }
#endif
  return(1);
}
#endif

template <class TImage, class TBres, class TAnchor, class TLine>
void DoAnchorFace(const TImage * input,
            TImage * output,
            typename TImage::PixelType border,
            TLine line,
            TAnchor &AnchorLine,
            const typename TBres::OffsetArray LineOffsets,
            typename TImage::PixelType * inbuffer,
            typename TImage::PixelType * outbuffer,
            const typename TImage::RegionType AllImage, 
            const typename TImage::RegionType face)
{
  // iterate over the face
  typedef ImageRegionConstIteratorWithIndex<TImage> ItType;
  ItType it(input, face);
  it.GoToBegin();
  TLine NormLine = line;
  NormLine.Normalize();
  // set a generous tolerance
  float tol = 1.0/LineOffsets.size();
  while (!it.IsAtEnd()) 
    {
    typename TImage::IndexType Ind = it.GetIndex();
    unsigned start, end, len;
    if (FillLineBuffer<TImage, TBres, TLine>(input, Ind, NormLine, tol, LineOffsets, 
                                             AllImage, inbuffer, start, end))
      {
      len = end - start + 1;
      // compat
      inbuffer[0]=border;
      inbuffer[len+1]=border;

#if 1
      AnchorLine.DoLine(outbuffer, inbuffer, len + 2);  // compat
      CopyLineToImage<TImage, TBres>(output, Ind, LineOffsets, outbuffer, start, end);
#else
      // test the decomposition
      CopyLineToImage<TImage, TBres>(output, Ind, LineOffsets, inbuffer, start, end);
#endif
      }
    ++it;
    }

}

} // namespace itk

#endif
