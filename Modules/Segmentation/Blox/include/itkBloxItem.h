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
#ifndef __itkBloxItem_h
#define __itkBloxItem_h

#include "itkWin32Header.h"

namespace itk
{
/**
 * \class BloxItem
 * \brief An entry in the BloxPixel linked list
 *
 * This class is do-nothing virtual class, designed to avoid
 * the necessity of templating BloxPixel over item type.
 * \ingroup ImageObjects
 *
 * \ingroup ITK-Blox
 */

class ITK_EXPORT BloxItem
{
public:
  BloxItem();
  virtual ~BloxItem();
};
} // end namespace itk

#endif
