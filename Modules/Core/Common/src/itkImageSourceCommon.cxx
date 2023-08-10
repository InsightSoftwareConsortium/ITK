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
#include "itkImageRegionSplitterSlowDimension.h"
#include "itkImageSourceCommon.h"
#include "itkSingleton.h"
#include <mutex>

namespace itk
{

struct ImageSourceCommonGlobals
{
  ImageRegionSplitterBase::Pointer m_GlobalDefaultSplitter{ ImageRegionSplitterSlowDimension::New().GetPointer() };
};

itkGetGlobalSimpleMacro(ImageSourceCommon, ImageSourceCommonGlobals, PimplGlobals);
ImageSourceCommonGlobals * ImageSourceCommon::m_PimplGlobals;

const ImageRegionSplitterBase *
ImageSourceCommon::GetGlobalDefaultSplitter()
{
  itkInitGlobalsMacro(PimplGlobals);
  return m_PimplGlobals->m_GlobalDefaultSplitter;
}


} // namespace itk
