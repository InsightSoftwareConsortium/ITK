/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVanHerkGilWermanUtilities.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVanHerkGilWermanUtilities_txx
#define __itkVanHerkGilWermanUtilities_txx

#include "itkVanHerkGilWermanUtilities.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
/**
 * \class VanHerkGilWermanUtilities
 * \brief functionality in common for VanHerkGilWerman openings/closings and
 * erosions/dilation
 *
 */

#if 0

// this version doesn't allow the user to set border values

template< class TImage, class TBres, class TLine, class TFunction >
int FillLineBuffer(typename TImage::ConstPointer input,
                   const typename TImage::IndexType StartIndex,
                   const TLine line,  // unit vector
                   const float tol,
                   const typename TBres::OffsetArray LineOffsets,
                   const typename TImage::RegionType AllImage,
                   const unsigned int KernLen,
                   typename TImage::PixelType *pixbuffer,
                   typename TImage::PixelType *fExtBuffer,
                   unsigned & start,
                   unsigned & end)
{
  int status = ComputeStartEnd< TImage, TBres, TLine >(StartIndex, line, tol, LineOffsets, AllImage,
                                                       start, end);

  if ( !status ) { return ( status ); }

  unsigned  size = end - start + 1;
  unsigned  blocks = size / KernLen;
  unsigned  i = 0;
  TFunction m_TF;
  //  std::cout << "Line length = " << size << " KernSize = " << KernLen << "
  // Blocks = " << blocks << std::endl;
  for ( unsigned j = 0; j < blocks; j++ )
    {
//    std::cout << "f1: i = " << i << std::endl;
    typename TImage::PixelType Ext = input->GetPixel(StartIndex + LineOffsets[start + i]);
    pixbuffer[i] = Ext;
    fExtBuffer[i] = Ext;
    ++i;
    for ( unsigned k = 1; k < KernLen; k++ )
      {
      typename TImage::PixelType V = input->GetPixel(StartIndex + LineOffsets[start + i]);
      pixbuffer[i] = V;
      fExtBuffer[i] = m_TF(V, fExtBuffer[i - 1]);
      ++i;
      }
    }
  // finish the rest
  if ( i != size - 1 )
    {
    typename TImage::PixelType V = input->GetPixel(StartIndex + LineOffsets[start + i]);
    pixbuffer[i] = V;
    fExtBuffer[i] = V;
    i++;
    }
  while ( i < size )
    {
    typename TImage::PixelType V = input->GetPixel(StartIndex + LineOffsets[start + i]);
    pixbuffer[i] = V;
    fExtBuffer[i] = m_TF(V, fExtBuffer[i - 1]);
    ++i;
    }
  return ( 1 );
}

template< class TImage, class TBres, class TFunction, class TLine >
void DoFace(typename TImage::ConstPointer input,
            typename TImage::Pointer output,
            TLine line,
            const typename TBres::OffsetArray LineOffsets,
            const unsigned int KernLen,
            typename TImage::PixelType *pixbuffer,
            typename TImage::PixelType *fExtBuffer,
            typename TImage::PixelType *rExtBuffer,
            const typename TImage::RegionType AllImage,
            const typename TImage::RegionType face)
{
  // iterate over the face
  typedef ImageRegionConstIteratorWithIndex< TImage > ItType;
  ItType it(input, face);
  it.GoToBegin();
  TLine NormLine = line;
  NormLine.Normalize();
  // set a generous tolerance
  float     tol = 1.0 / LineOffsets.size();
  TFunction m_TF;

  while ( !it.IsAtEnd() )
    {
    typename TImage::IndexType Ind = it.GetIndex();
    unsigned start, end, len;
    if ( FillLineBuffer< TImage, TBres, TLine, TFunction >(input, Ind, NormLine, tol, LineOffsets,
                                                           AllImage, KernLen, pixbuffer, fExtBuffer,
                                                           start, end) )
      {
      len = end - start + 1;
      // compute the reverse running extreme -- not that we aren't
      // using unsigned types because it is messy using them in
      // reverse loops
      int size = (int)( end - start + 1 );
      int blocks = size / (int)KernLen;
      int i = size - 1;

      if ( ( i > ( blocks * (int)KernLen - 1 ) ) )
        {
        rExtBuffer[i] = pixbuffer[i];
        // std::cout << "r1: i = " << i << " " << (int) rExtBuffer[i] <<
        // std::endl;
        --i;
        while ( i >= (int)( blocks * KernLen ) )
          {
          // std::cout << "r2: i = " << i << std::endl;

          typename TImage::PixelType V = pixbuffer[i];
          rExtBuffer[i] = m_TF(V, rExtBuffer[i + 1]);
//           if (m_TF(V, rExtBuffer[i+1]))
//             {
//             rExtBuffer[i] = V;
//             }
//           else
//             {
//             rExtBuffer[i] = rExtBuffer[i+1];
//             }
//          std::cout << "r2: i = " << i << " " << (int) rExtBuffer[i] <<
// std::endl;
          --i;
          }
        }
      for ( unsigned j = 0; j < blocks; j++ )
        {
//        std::cout << "r3: i = " << i << std::endl;
        typename TImage::PixelType Ext = pixbuffer[i];
        rExtBuffer[i] = Ext;
        --i;
        for ( unsigned k = 1; k < KernLen; k++ )
          {
//          std::cout << "r4: i = " << i << std::endl;
          typename TImage::PixelType V = pixbuffer[i];
          rExtBuffer[i] = m_TF(V, rExtBuffer[i + 1]);
//           if (m_TF(V, rExtBuffer[i+1]))
//             {
//             rExtBuffer[i] = V;
//             }
//           else
//             {
//             rExtBuffer[i] = rExtBuffer[i+1];
//             }
          --i;
          }
        }
      // now compute result
      if ( size <= KernLen / 2 )
        {
        for ( unsigned j = 0; j < size; j++ )
          {
          pixbuffer[j] = fExtBuffer[size - 1];
          }
        }
      else if ( size <= KernLen )
        {
        for ( unsigned j = 0; j < size - KernLen / 2; j++ )
          {
          pixbuffer[j] = fExtBuffer[j + KernLen / 2];
          }
        for ( unsigned j =  size - KernLen / 2; j <= KernLen / 2; j++ )
          {
          pixbuffer[j] = fExtBuffer[size - 1];
          }
        for ( unsigned j =  KernLen / 2 + 1; j < size; j++ )
          {
          pixbuffer[j] = rExtBuffer[j - KernLen / 2];
          }
        }
      else
        {
        // line beginning
        for ( unsigned j = 0; j < KernLen / 2; j++ )
          {
          pixbuffer[j] = fExtBuffer[j + KernLen / 2];
          }
        for ( unsigned j = KernLen / 2, k = KernLen / 2 + KernLen / 2, l = KernLen / 2 - KernLen / 2;
              j < size - KernLen / 2; j++, k++, l++ )
          {
          typename TImage::PixelType V1 = fExtBuffer[k];
          typename TImage::PixelType V2 = rExtBuffer[l];
          pixbuffer[j] = m_TF(V1, V2);
//           if (m_TF(V1, V2))
//             {
//             pixbuffer[j] = V1;
//             }
//           else
//             {
//             pixbuffer[j] = V2;
//             }
          }
        // line end -- involves reseting the end of the reverse
        // extreme array
        for ( unsigned j = size - 2; ( j > 0 ) && ( j >= ( size - KernLen - 1 ) ); j-- )
          {
//          std::cout << "j1 = " << j << " ";
          rExtBuffer[j] = m_TF(rExtBuffer[j + 1], rExtBuffer[j]);
//           if (m_TF(rExtBuffer[j+1], rExtBuffer[j]))
//             {
//             rExtBuffer[j] = rExtBuffer[j+1];
//             }
//            std::cout << (int) rExtBuffer[j] << " ";
          }
//        std::cout << std::endl;
        for ( unsigned j = size - KernLen / 2; j < size; j++ )
          {
//          std::cout << "j2 = " << j - KernLen/2 << std::endl;
          pixbuffer[j] = rExtBuffer[j - KernLen / 2];
          }
        }
      CopyLineToImage< TImage, TBres >(output, Ind, LineOffsets, pixbuffer, start, end);
      }
    ++it;
    }
}

#else
// this version does allow the user to set border values. It follows
// the same structure as the anchor approach. I've stopped attempting
// to have the line copy and forward extreme buffer filled in the same
// loop - it is too messy.

template< class PixelType, class TFunction >
void FillForwardExt(PixelType *pixbuffer, PixelType *fExtBuffer,
                    const unsigned int KernLen, unsigned len)
{
  unsigned  size = len;
  unsigned  blocks = size / KernLen;
  unsigned  i = 0;
  TFunction m_TF;

  for ( unsigned j = 0; j < blocks; j++ )
    {
    assert(i >= 0);
    assert(i < len);
    PixelType Ext = pixbuffer[i];
    fExtBuffer[i] = Ext;
    ++i;
    for ( unsigned k = 1; k < KernLen; k++ )
      {
      assert( ( i - 1 ) >= 0 );
      assert(i < len);
      PixelType V = pixbuffer[i];
      fExtBuffer[i] = m_TF(V, fExtBuffer[i - 1]);
      ++i;
      }
    }
  // finish the rest
//  if ((i != size - 1) && (i < size))
  if ( i < size )
    {
    assert(i >= 0);
    assert(i < len);
    PixelType V = pixbuffer[i];
    fExtBuffer[i] = V;
    i++;
    }
  while ( i < size )
    {
    assert( ( i - 1 ) >= 0 );
    assert(i < len);
    PixelType V = pixbuffer[i];
    fExtBuffer[i] = m_TF(V, fExtBuffer[i - 1]);
    ++i;
    }
}

template< class PixelType, class TFunction >
void FillReverseExt(PixelType *pixbuffer, PixelType *rExtBuffer,
                    const unsigned int KernLen, unsigned len)
{
  long      size = (long)( len );
  long      blocks = size / (int)KernLen;
  long      i = size - 1;
  TFunction m_TF;

  if ( ( i > ( blocks * (int)KernLen - 1 ) ) )
    {
    assert(i >= 0);
    assert(i < (long)len);
    rExtBuffer[i] = pixbuffer[i];
    --i;
    while ( i >= (int)( blocks * KernLen ) )
      {
      assert(i >= 0);
      assert( ( i + 1 ) < (long)len );
      PixelType V = pixbuffer[i];
      rExtBuffer[i] = m_TF(V, rExtBuffer[i + 1]);
      --i;
      }
    }
  for ( unsigned j = 0; j < (unsigned)blocks; j++ )
    {
    assert(i >= 0);
    assert(i < (long)len);
    PixelType Ext = pixbuffer[i];
    rExtBuffer[i] = Ext;
    --i;
    for ( unsigned k = 1; k < KernLen; k++ )
      {
      assert(i >= 0);
      assert( ( i + 1 ) < (long)len );
      PixelType V = pixbuffer[i];
      rExtBuffer[i] = m_TF(V, rExtBuffer[i + 1]);
      --i;
      }
    }
}

template< class TImage, class TBres, class TFunction, class TLine >
void DoFace(typename TImage::ConstPointer input,
            typename TImage::Pointer output,
            typename TImage::PixelType border,
            TLine line,
            const typename TBres::OffsetArray LineOffsets,
            const unsigned int KernLen,
            typename TImage::PixelType *pixbuffer,
            typename TImage::PixelType *fExtBuffer,
            typename TImage::PixelType *rExtBuffer,
            const typename TImage::RegionType AllImage,
            const typename TImage::RegionType face)
{
  // iterate over the face

  // we can't use an iterator with a region outside the image. All we need here
  // is to
  // iterate over all the indexes of the face, without accessing the content of
  // the image.
  // I can't find any cleaner way, so we use a dumb image, not even allocated,
  // to iterate
  // over all the indexes inside the region.
  //
  // typedef ImageRegionConstIteratorWithIndex<TImage> ItType;
  // ItType it(input, face);

  typename TImage::Pointer dumbImg = TImage::New();
  dumbImg->SetRegions(face);

  TLine NormLine = line;
  NormLine.Normalize();
  // set a generous tolerance
  float     tol = 1.0 / LineOffsets.size();
  TFunction m_TF;
  for ( unsigned int it = 0; it < face.GetNumberOfPixels(); it++ )
    {
    typename TImage::IndexType Ind = dumbImg->ComputeIndex(it);
    unsigned start, end, len;
    if ( FillLineBuffer< TImage, TBres, TLine >(input, Ind, NormLine, tol, LineOffsets,
                                                AllImage, pixbuffer, start, end) )
      {
      len = end - start + 1;
      // compat
      pixbuffer[0] = border;
      pixbuffer[len + 1] = border;
      FillForwardExt< ITK_TYPENAME TImage::PixelType, TFunction >(pixbuffer, fExtBuffer, KernLen, len + 2);
      FillReverseExt< ITK_TYPENAME TImage::PixelType, TFunction >(pixbuffer, rExtBuffer, KernLen, len + 2);
      // now compute result
      unsigned int size = len + 2;
      if ( size <= KernLen / 2 )
        {
        for ( unsigned j = 0; j < size; j++ )
          {
          assert(j >= 0);
          assert(j < size);
          pixbuffer[j] = fExtBuffer[size - 1];
          }
        }
      else if ( size <= KernLen )
        {
        for ( unsigned j = 0; j < size - KernLen / 2; j++ )
          {
          pixbuffer[j] = fExtBuffer[j + KernLen / 2];
          }
        for ( unsigned j =  size - KernLen / 2; j <= KernLen / 2; j++ )
          {
          pixbuffer[j] = fExtBuffer[size - 1];
          }
        for ( unsigned j =  KernLen / 2 + 1; j < size; j++ )
          {
          pixbuffer[j] = rExtBuffer[j - KernLen / 2];
          }
        }
      else
        {
        // line beginning
        for ( unsigned j = 0; j < KernLen / 2; j++ )
          {
          assert(j >= 0);
          assert( ( j + KernLen / 2 ) < size );
          pixbuffer[j] = fExtBuffer[j + KernLen / 2];
          }
        for ( unsigned j = KernLen / 2, k = KernLen / 2 + KernLen / 2, l = KernLen / 2 - KernLen / 2;
              j < size - KernLen / 2; j++, k++, l++ )
          {
          assert(k >= 0);
          assert(k < size);
          assert(l >= 0);
          assert(l < size);
          assert(j >= 0);
          assert(j < size);

          typename TImage::PixelType V1 = fExtBuffer[k];
          typename TImage::PixelType V2 = rExtBuffer[l];
          pixbuffer[j] = m_TF(V1, V2);
          }
        // line end -- involves reseting the end of the reverse
        // extreme array
        for ( unsigned j = size - 2; ( j > 0 ) && ( j >= ( size - KernLen - 1 ) ); j-- )
          {
          assert(j >= 0);
          assert( ( j + 1 ) < size );
          rExtBuffer[j] = m_TF(rExtBuffer[j + 1], rExtBuffer[j]);
          }
        for ( unsigned j = size - KernLen / 2; j < size; j++ )
          {
          assert( ( j - KernLen / 2 ) >= 0 );
          assert( ( j - KernLen / 2 ) < size );
          assert( ( j ) < size );
          pixbuffer[j] = rExtBuffer[j - KernLen / 2];
          }
        }
      CopyLineToImage< TImage, TBres >(output, Ind, LineOffsets, pixbuffer, start, end);
      }
    }
}

#endif
} // namespace itk

#endif
