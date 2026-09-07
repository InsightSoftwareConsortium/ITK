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
#ifndef itkLinearReduceAlgorithm_hxx
#define itkLinearReduceAlgorithm_hxx

#include "itkLinearReduceAlgorithm.h"

#include <utility>

namespace itk
{

template <typename T>
void
LinearReduceAlgorithm<T>::SetNumberOfWorkUnits(SizeValueType numberOfWorkUnits)
{
  Superclass::SetNumberOfWorkUnits(numberOfWorkUnits);

  const std::lock_guard<std::mutex> lockGuard(this->m_Mutex);
  m_Values.assign(numberOfWorkUnits, std::nullopt);
  m_Result = T{};
  m_Merged = false;
}

template <typename T>
void
LinearReduceAlgorithm<T>::Merge(T && /*localResult*/)
{
  itkExceptionMacro("LinearReduceAlgorithm::Merge(T&&) requires a chunk ID. "
                    "Call Merge(SizeValueType chunkId, T&&) instead.");
}

template <typename T>
void
LinearReduceAlgorithm<T>::Merge(SizeValueType chunkId, T && localResult)
{
  const std::lock_guard<std::mutex> lockGuard(this->m_Mutex);

  if (m_Values.empty())
  {
    itkExceptionMacro("LinearReduceAlgorithm::Merge() called before SetNumberOfWorkUnits().");
  }
  if (chunkId >= this->m_NumberOfWorkUnits)
  {
    itkExceptionMacro("LinearReduceAlgorithm::Merge(): chunkId " << chunkId << " >= NumberOfWorkUnits "
                                                                 << this->m_NumberOfWorkUnits);
  }

  // Just deposit the value; GetResult() performs the actual ordered merge.
  m_Values[chunkId] = std::move(localResult);
}

template <typename T>
const T &
LinearReduceAlgorithm<T>::GetResult() const
{
  const std::lock_guard<std::mutex> lockGuard(this->m_Mutex);

  if (!m_Merged)
  {
    for (auto & value : m_Values)
    {
      if (!value.has_value())
      {
        continue;
      }
      if (m_Merged)
      {
        this->m_MergeFunction(m_Result, *value);
      }
      else
      {
        m_Result = std::move(*value);
        m_Merged = true;
      }
      value.reset();
    }
  }
  return m_Result;
}

template <typename T>
void
LinearReduceAlgorithm<T>::Clear()
{
  const std::lock_guard<std::mutex> lockGuard(this->m_Mutex);
  for (auto & v : m_Values)
  {
    v.reset();
  }
  m_Result = T{};
  m_Merged = false;
  this->Modified();
}

template <typename T>
void
LinearReduceAlgorithm<T>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Merged: " << (m_Merged ? "true" : "false") << std::endl;
}

} // namespace itk

#endif // itkLinearReduceAlgorithm_hxx
