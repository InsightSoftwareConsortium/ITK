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
#ifndef itkImageToImageFilterCommon_h
#define itkImageToImageFilterCommon_h

#include "ITKCommonExport.h"

namespace itk
{

/** \class ImageToImageFilterCommon
 * \brief Secondary base class of ImageToImageFilter common between templates
 *
 * This class provides common non-templated code which can be compiled
 * and used by all templated versions of ImageToImageFilter.
 *
 * This class must be inherited privately, and light-weight adapting
 * of methods is required for virtual methods or non-private methods
 * for the ImageToImageFilter interface.
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ImageToImageFilterCommon
{
public:
  static void SetGlobalDefaultCoordinateTolerance(double);
  static double GetGlobalDefaultCoordinateTolerance();
  static void SetGlobalDefaultDirectionTolerance(double);
  static double GetGlobalDefaultDirectionTolerance();
};

} // end namespace itk

#endif
