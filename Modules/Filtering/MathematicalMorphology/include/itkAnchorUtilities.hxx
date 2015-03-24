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
#ifndef itkAnchorUtilities_hxx
#define itkAnchorUtilities_hxx

#include "itkAnchorUtilities.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
/**
* AnchorUtilities
* functionality in common for anchor openings/closings and
* erosions/dilation
*
**/
template< typename TImage, typename TBres, typename TAnchor, typename TLine >
void DoAnchorFace(const TImage *input,
                  TImage *output,
                  typename TImage::PixelType border,
                  TLine line,
                  TAnchor & AnchorLine,
                  const typename TBres::OffsetArray LineOffsets,
                  std::vector<typename TImage::PixelType> & inbuffer,
                  std::vector<typename TImage::PixelType> & outbuffer,
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
  float tol = 1.0 / LineOffsets.size();
  for ( unsigned int it = 0; it < face.GetNumberOfPixels(); it++ )
    {
    typename TImage::IndexType Ind = dumbImg->ComputeIndex(it);
    unsigned start, end;
    if ( FillLineBuffer< TImage, TBres, TLine >(input, Ind, NormLine, tol, LineOffsets,
                                                AllImage, inbuffer, start, end) )
      {
      const unsigned len = end - start + 1;
      // compat
      inbuffer[0] = border;
      inbuffer[len + 1] = border;

      AnchorLine.DoLine(outbuffer, inbuffer, len + 2);  // compat
      CopyLineToImage< TImage, TBres >(output, Ind, LineOffsets, outbuffer, start, end);
      }
    }
}
} // namespace itk

#endif
