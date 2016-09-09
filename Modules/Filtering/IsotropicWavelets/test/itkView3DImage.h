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
#ifndef itkView3DImage_h
#define itkView3DImage_h
#include <cstddef>
namespace itk
{
template <typename T>
void
View3DImage(const T * img, size_t win_x = 600, size_t win_y = 600);

template <typename TLeft, typename TRight>
void
View3DImages(const TLeft * leftImg, const TRight * rightImg, size_t win_x = 800, size_t win_y = 800);
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkView3DImage.hxx"
#endif
#endif
