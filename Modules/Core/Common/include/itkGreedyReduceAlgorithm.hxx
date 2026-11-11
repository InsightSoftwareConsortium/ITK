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
#ifndef itkGreedyReduceAlgorithm_hxx
#define itkGreedyReduceAlgorithm_hxx

#include "itkGreedyReduceAlgorithm.h"

#include <utility>

namespace itk
{

template <typename T>
void
GreedyReduceAlgorithm<T>::Merge(T && localResult)
{
  // Greedy swap-and-merge strategy:
  //   Acquire the mutex only long enough to swap ownership of the shared
  //   accumulator.  The actual merge work is done outside the critical section
  //   so other threads can proceed concurrently.
  while (true)
  {
    T tomerge{};
    {
      const std::lock_guard<std::mutex> lockGuard(this->m_Mutex);

      if (!m_HasResult)
      {
        // No accumulated result yet: store the local result and return.
        m_Result = std::move(localResult);
        m_HasResult = true;
        return;
      }

      // Take ownership of the current accumulated result so other threads
      // can deposit their own results immediately after we release the lock.
      std::swap(m_Result, tomerge);
      m_HasResult = false;
    } // release lock

    // Merge the taken result into localResult outside the critical section.
    this->m_MergeFunction(localResult, tomerge);
  }
}

template <typename T>
const T &
GreedyReduceAlgorithm<T>::GetResult() const
{
  return m_Result;
}

template <typename T>
void
GreedyReduceAlgorithm<T>::Clear()
{
  const std::lock_guard<std::mutex> lockGuard(this->m_Mutex);
  m_Result = T{};
  m_HasResult = false;
  Superclass::Clear();
}

template <typename T>
void
GreedyReduceAlgorithm<T>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "HasResult: " << (m_HasResult ? "true" : "false") << std::endl;
}

} // namespace itk

#endif // itkGreedyReduceAlgorithm_hxx
