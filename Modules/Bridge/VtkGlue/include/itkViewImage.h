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
#ifndef itkViewImage_h
#define itkViewImage_h
#include <cstddef>
#include <string>
namespace itk
{
template<typename TImageType >
class ViewImage {
public:
  static void View(const TImageType* img,
      const std::string& win_title = "itkView",
      size_t win_x = 600,
      size_t win_y = 600);
};
}// namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkViewImage.hxx"
#endif
#endif
