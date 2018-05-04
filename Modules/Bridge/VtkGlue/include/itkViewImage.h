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
  /** \class ViewImage
   * Class with static function to visualize an image using just vtk.
   * It accepts 2D and 3D images.
   *
   * Usage example:
   * itk::ViewImage<ImageType>::View( reader->GetOutput())
   *
   * \ingroup ITKVtkGlue
   *
   */
template<typename TImageType >
class ViewImage {
public:

  /**
   * Static function to visualize 2D and 3D images.
   *
   * \param img Input image
   * \param winTitle Title of the window.
   * \param winWidth Width of the visualization window.
   * \param winHeight Height of the visualization window.
   */
  static void View(const TImageType* img,
      const std::string& winTitle = "itkView",
      size_t winWidth = 600,
      size_t winHeight = 600);
};
}// namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkViewImage.hxx"
#endif
#endif
