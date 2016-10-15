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
#ifndef itkMetaEvent_h
#define itkMetaEvent_h

#include "itkMacro.h"
#include "metaEvent.h"

namespace itk
{
/** \class MetaEvent
 * \brief Event abstract class
 *
 * The itk::MetaEvent inherits from the
 * MetaEvent that is provided
 * by the MetaIO/src/metaEvent.h class.
 * \ingroup ITKSpatialObjects
 */
#if (METAIO_USE_NAMESPACE)
class MetaEvent : public METAIO_NAMESPACE::MetaEvent
#else
class MetaEvent : public ::MetaEvent
#endif
{
public:
  MetaEvent();
  virtual ~MetaEvent();
};
} // end namespace itk

#endif
