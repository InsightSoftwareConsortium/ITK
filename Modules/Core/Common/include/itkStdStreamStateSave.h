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
#ifndef itkStdStreamStateSave_h
#define itkStdStreamStateSave_h

#include <iostream>
#include <string>
#include "itkMacro.h"

namespace itk
{
/** \class StdStreamStateSave
 *
 * \brief Save a stream's format state and restore it upon destruction
 *
 * An RAII class to provide an exception safe mechanism to restore the
 * format state of a stream. The class holds a resource (the stream's
 * format state) and resets the resource to a default state upon destruction.
 *
 * Typical usage:
 \code{.cpp}
 itk::StdStreamStateSave coutState(std::cout);
 std::cout.precision(20);
 std::cout.hex();
 ...
 return;
 \endcode
 *
 * \ingroup ITKCommon
 */

class StdStreamStateSave
{
public:
  explicit StdStreamStateSave(std::ios& stream) :
    m_Ios(stream),
    m_State(ITK_NULLPTR)
  {
    m_State.copyfmt(stream);
  }
  ~StdStreamStateSave()
  {
    m_Ios.copyfmt(m_State);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StdStreamStateSave);

  std::ios& m_Ios;
  std::ios  m_State;
};
}

#endif
