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
#ifndef itkVanHerkGilWermanUtilities_hxx
#define itkVanHerkGilWermanUtilities_hxx

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

// this version does allow the user to set border values. It follows
// the same structure as the anchor approach. I've stopped attempting
// to have the line copy and forward extreme buffer filled in the same
// loop - it is too messy.

template< typename PixelType, typename TFunction >
void FillForwardExt(std::vector<PixelType> & pixbuffer, std::vector<PixelType> & fExtBuffer,
                    const unsigned int KernLen, unsigned len)
{
  unsigned  size = len;
  unsigned  blocks = size / KernLen;
  unsigned  i = 0;
  TFunction m_TF;

  for ( unsigned j = 0; j < blocks; j++ )
    {
    PixelType Ext = pixbuffer[i];
    fExtBuffer[i] = Ext;
    ++i;
    for ( unsigned k = 1; k < KernLen; k++ )
      {
      PixelType V = pixbuffer[i];
      fExtBuffer[i] = m_TF(V, fExtBuffer[i - 1]);
      ++i;
      }
    }
  // finish the rest
  if ( i < size )
    {
    PixelType V = pixbuffer[i];
    fExtBuffer[i] = V;
    i++;
    }
  while ( i < size )
    {
    PixelType V = pixbuffer[i];
    fExtBuffer[i] = m_TF(V, fExtBuffer[i - 1]);
    ++i;
    }
}

template< typename PixelType, typename TFunction >
void FillReverseExt(std::vector<PixelType> & pixbuffer, std::vector<PixelType> & rExtBuffer,
                    const unsigned int KernLen, unsigned len)
{
  IndexValueType      size = (IndexValueType)( len );
  IndexValueType      blocks = size / (int)KernLen;
  IndexValueType      i = size - 1;
  TFunction m_TF;

  if ( ( i > ( blocks * (int)KernLen - 1 ) ) )
    {
    rExtBuffer[i] = pixbuffer[i];
    --i;
    while ( i >= (int)( blocks * KernLen ) )
      {
      PixelType V = pixbuffer[i];
      rExtBuffer[i] = m_TF(V, rExtBuffer[i + 1]);
      --i;
      }
    }
  for ( unsigned j = 0; j < (unsigned)blocks; j++ )
    {
    PixelType Ext = pixbuffer[i];
    rExtBuffer[i] = Ext;
    --i;
    for ( unsigned k = 1; k < KernLen; k++ )
      {
      PixelType V = pixbuffer[i];
      rExtBuffer[i] = m_TF(V, rExtBuffer[i + 1]);
      --i;
      }
    }
}

template< typename TImage, typename TBres, typename TFunction, typename TLine >
void DoFace(typename TImage::ConstPointer input,
            typename TImage::Pointer output,
            typename TImage::PixelType border,
            TLine line,
            const typename TBres::OffsetArray LineOffsets,
            const unsigned int KernLen,
            std::vector<typename TImage::PixelType> & pixbuffer,
            std::vector<typename TImage::PixelType> & fExtBuffer,
            std::vector<typename TImage::PixelType> & rExtBuffer,
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
    unsigned start, end;
    if ( FillLineBuffer< TImage, TBres, TLine >(input, Ind, NormLine, tol, LineOffsets,
                                                AllImage, pixbuffer, start, end) )
      {
      const unsigned len = end - start + 1;
      // compat
      pixbuffer[0] = border;
      pixbuffer[len + 1] = border;
      FillForwardExt< typename TImage::PixelType, TFunction >(pixbuffer, fExtBuffer, KernLen, len + 2);
      FillReverseExt< typename TImage::PixelType, TFunction >(pixbuffer, rExtBuffer, KernLen, len + 2);
      // now compute result
      unsigned int size = len + 2;
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
          }
        // line end -- involves reseting the end of the reverse
        // extreme array
        for ( unsigned j = size - 2; ( j > 0 ) && ( j >= ( size - KernLen - 1 ) ); j-- )
          {
          rExtBuffer[j] = m_TF(rExtBuffer[j + 1], rExtBuffer[j]);
          }
        for ( unsigned j = size - KernLen / 2; j < size; j++ )
          {
          pixbuffer[j] = rExtBuffer[j - KernLen / 2];
          }
        }
      CopyLineToImage< TImage, TBres >(output, Ind, LineOffsets, pixbuffer, start, end);
      }
    }
}

} // namespace itk

#endif
