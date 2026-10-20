/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageRegionReverseIterator_hxx
#define itkImageRegionReverseIterator_hxx


namespace itk
{
template <typename TImage>
ImageRegionReverseIterator<TImage>::ImageRegionReverseIterator(TImage * ptr, const RegionType & region)
  : ImageRegionReverseConstIterator<TImage>(ptr, region)
{
  // Base class calls the const GetBufferPointer(); call the non-const one for its side effects (e.g. CudaImage).
  static_cast<void>(ptr->GetBufferPointer());
}

template <typename TImage>
ImageRegionReverseIterator<TImage>::ImageRegionReverseIterator(const ImageConstIterator<TImage> & it)
  : Superclass(it)
{
  // Source iterator holds a const image; call the non-const GetBufferPointer() for its side effects (e.g. CudaImage).
  static_cast<void>(const_cast<TImage *>(this->m_Image.GetPointer())->GetBufferPointer());
}

template <typename TImage>
ImageRegionReverseIterator<TImage>::ImageRegionReverseIterator(const ImageRegionReverseConstIterator<TImage> & it)
  : Superclass(it)
{}

template <typename TImage>
ImageRegionReverseIterator<TImage> &
ImageRegionReverseIterator<TImage>::operator=(const ImageRegionReverseConstIterator<TImage> & it)
{
  this->Superclass::operator=(it);
  return *this;
}

} // end namespace itk

#endif
