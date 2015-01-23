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
#ifndef itkVanHerkGilWermanUtilities_h
#define itkVanHerkGilWermanUtilities_h

#include <list>

#include "itkSharedMorphologyUtilities.h"

namespace itk
{
/**
 * \class VanHerkGilWermanUtilities
 * \brief functionality in common for anchor openings/closings and
 * erosions/dilation
 *
 * \ingroup ITKMathematicalMorphology
 */
template< typename PixelType, typename TFunction >
void FillReverseExt(std::vector<PixelType> & pixbuffer,
                    std::vector<PixelType> & rExtBuffer,
                    const unsigned int KernLen, unsigned len);

template< typename PixelType, typename TFunction >
void FillForwardExt(std::vector<PixelType> & pixbuffer,
                    std::vector<PixelType> & fExtBuffer,
                    const unsigned int KernLen, unsigned len);

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
            const typename TImage::RegionType face);
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVanHerkGilWermanUtilities.hxx"
#endif

#endif
