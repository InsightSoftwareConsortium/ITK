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
#include "itkLoggerThreadWrapper.h"

namespace itk
{
std::ostream &
operator<<(std::ostream & out, const LoggerThreadWrapperOperationType value)
{
  const char * s = nullptr;
  switch (value)
  {
    case LoggerThreadWrapperOperationType::SET_PRIORITY_LEVEL:
      s = "LoggerThreadWrapperOperationType::SET_PRIORITY_LEVEL";
      break;
    case LoggerThreadWrapperOperationType::SET_LEVEL_FOR_FLUSHING:
      s = "LoggerThreadWrapperOperationType::SET_LEVEL_FOR_FLUSHING";
      break;
    case LoggerThreadWrapperOperationType::ADD_LOG_OUTPUT:
      s = "LoggerThreadWrapperOperationType::ADD_LOG_OUTPUT";
      break;
    case LoggerThreadWrapperOperationType::WRITE:
      s = "LoggerThreadWrapperOperationType::WRITE";
      break;
    default:
      s = "INVALID VALUE FOR LoggerThreadWrapperOperationType";
  }
  return out << s;
}
} // end namespace itk
