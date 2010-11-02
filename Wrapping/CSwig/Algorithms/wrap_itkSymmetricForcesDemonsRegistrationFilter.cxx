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
#include "itkImage.h"
#include "itkSymmetricForcesDemonsRegistrationFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkSymmetricForcesDemonsRegistrationFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT3(SymmetricForcesDemonsRegistrationFilter, image::F2, image::F2, image::VF2,
                     itkSymmetricForcesDemonsRegistrationFilterF2F2);
    ITK_WRAP_OBJECT3(SymmetricForcesDemonsRegistrationFilter, image::F3, image::F3, image::VF3,
                     itkSymmetricForcesDemonsRegistrationFilterF3F3);
    ITK_WRAP_OBJECT3(SymmetricForcesDemonsRegistrationFilter, image::US2, image::US2, image::VF2,
                     itkSymmetricForcesDemonsRegistrationFilterUS2US2);
    ITK_WRAP_OBJECT3(SymmetricForcesDemonsRegistrationFilter, image::US3, image::US3, image::VF3,
                     itkSymmetricForcesDemonsRegistrationFilterUS3US3);
  }
}

#endif
