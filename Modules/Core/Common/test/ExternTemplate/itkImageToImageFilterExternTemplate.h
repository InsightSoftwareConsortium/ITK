/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkImageToImageFilterExternTemplate_h
#define itkImageToImageFilterExternTemplate_h

#include "itkImage.h"

namespace itk
{
// 2D
// InputImageType equals OutputImageType
extern template class ImageToImageFilter<Image<signed short, 2>, Image<signed short, 2>>;
extern template class ImageToImageFilter<Image<unsigned short, 2>, Image<unsigned short, 2>>;
extern template class ImageToImageFilter<Image<char, 2>, Image<char, 2>>;
extern template class ImageToImageFilter<Image<unsigned char, 2>, Image<unsigned char, 2>>;
extern template class ImageToImageFilter<Image<int, 2>, Image<int, 2>>;
extern template class ImageToImageFilter<Image<unsigned int, 2>, Image<unsigned int, 2>>;
extern template class ImageToImageFilter<Image<long, 2>, Image<long, 2>>;
extern template class ImageToImageFilter<Image<unsigned long, 2>, Image<unsigned long, 2>>;
extern template class ImageToImageFilter<Image<float, 2>, Image<float, 2>>;
extern template class ImageToImageFilter<Image<double, 2>, Image<double, 2>>;

// 3D
// InputImageType equals OutputImageType
extern template class ImageToImageFilter<Image<signed short, 3>, Image<signed short, 3>>;
extern template class ImageToImageFilter<Image<unsigned short, 3>, Image<unsigned short, 3>>;
extern template class ImageToImageFilter<Image<char, 3>, Image<char, 3>>;
extern template class ImageToImageFilter<Image<unsigned char, 3>, Image<unsigned char, 3>>;
extern template class ImageToImageFilter<Image<int, 3>, Image<int, 3>>;
extern template class ImageToImageFilter<Image<unsigned int, 3>, Image<unsigned int, 3>>;
extern template class ImageToImageFilter<Image<long, 3>, Image<long, 3>>;
extern template class ImageToImageFilter<Image<unsigned long, 3>, Image<unsigned long, 3>>;
extern template class ImageToImageFilter<Image<float, 3>, Image<float, 3>>;
extern template class ImageToImageFilter<Image<double, 3>, Image<double, 3>>;
} // end namespace itk

#endif
