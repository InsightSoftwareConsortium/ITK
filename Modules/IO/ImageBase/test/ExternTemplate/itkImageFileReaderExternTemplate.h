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

#ifndef itkImageFileReaderExternTemplate_h
#define itkImageFileReaderExternTemplate_h

namespace itk
{
// 2D
extern template class ImageFileReader<Image<signed short, 2>>;
extern template class ImageFileReader<Image<unsigned short, 2>>;
extern template class ImageFileReader<Image<char, 2>>;
extern template class ImageFileReader<Image<unsigned char, 2>>;
extern template class ImageFileReader<Image<int, 2>>;
extern template class ImageFileReader<Image<unsigned int, 2>>;
extern template class ImageFileReader<Image<long, 2>>;
extern template class ImageFileReader<Image<unsigned long, 2>>;
extern template class ImageFileReader<Image<float, 2>>;
extern template class ImageFileReader<Image<double, 2>>;

// 3D
extern template class ImageFileReader<Image<signed short, 3>>;
extern template class ImageFileReader<Image<unsigned short, 3>>;
extern template class ImageFileReader<Image<char, 3>>;
extern template class ImageFileReader<Image<unsigned char, 3>>;
extern template class ImageFileReader<Image<int, 3>>;
extern template class ImageFileReader<Image<unsigned int, 3>>;
extern template class ImageFileReader<Image<long, 3>>;
extern template class ImageFileReader<Image<unsigned long, 3>>;
extern template class ImageFileReader<Image<float, 3>>;
extern template class ImageFileReader<Image<double, 3>>;
} // end namespace itk
#endif
