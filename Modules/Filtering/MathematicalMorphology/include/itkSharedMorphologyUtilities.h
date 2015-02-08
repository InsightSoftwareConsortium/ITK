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
#ifndef itkSharedMorphologyUtilities_h
#define itkSharedMorphologyUtilities_h

#include <list>
#include <vector>

namespace itk
{
template< typename TRegion, typename TLine >
bool NeedToDoFace(const TRegion AllImage,
                  const TRegion face,
                  const TLine line);

template< typename TImage, typename TBres, typename TLine >
int ComputeStartEnd(const typename TImage::IndexType StartIndex,
                    const TLine line,
                    const float tol,
                    const typename TBres::OffsetArray LineOffsets,
                    const typename TImage::RegionType AllImage,
                    unsigned & start,
                    unsigned & end);

template< typename TImage, typename TBres, typename TLine >
int FillLineBuffer(typename TImage::ConstPointer input,
                   const typename TImage::IndexType StartIndex,
                   const TLine line,
                   const float tol,
                   const typename TBres::OffsetArray LineOffsets,
                   const typename TImage::RegionType AllImage,
                   std::vector<typename TImage::PixelType> & inbuffer,
                   unsigned int &start,
                   unsigned int &end);

template< typename TImage, typename TBres >
void CopyLineToImage(const typename TImage::Pointer output,
                     const typename TImage::IndexType StartIndex,
                     const typename TBres::OffsetArray LineOffsets,
                     std::vector<typename TImage::PixelType> & outbuffer,
                     const unsigned start,
                     const unsigned end);

// This returns a face with a normal between +/- 45 degrees of the
// line. The face is enlarged so that AllImage is entirely filled by
// lines starting from every pixel in the face. This means that some
// of the region will not touch the image. This approach is necessary
// because we want to be able to sweep the lines in a fashion that
// does not have overlap between them.
template< typename TInputImage, typename TLine >
typename TInputImage::RegionType
MakeEnlargedFace(const TInputImage *input,
                 const typename TInputImage::RegionType AllImage,
                 const TLine line);

// figure out the correction factor for length->pixel count based on
// line angle
template< typename TLine >
unsigned int GetLinePixels(const TLine line);

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSharedMorphologyUtilities.hxx"
#endif

#endif
