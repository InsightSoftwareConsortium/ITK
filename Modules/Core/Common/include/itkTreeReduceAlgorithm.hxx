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
#ifndef itkTreeReduceAlgorithm_hxx
#define itkTreeReduceAlgorithm_hxx

#include "itkTreeReduceAlgorithm.h"

#include <utility>

namespace itk
{

// ---------------------------------------------------------------------------
// SetNumberOfWorkUnits
// ---------------------------------------------------------------------------

template <typename T>
void
TreeReduceAlgorithm<T>::SetNumberOfWorkUnits(SizeValueType numberOfWorkUnits)
{
  // Store and propagate the Modified() flag via the base-class macro.
  Superclass::SetNumberOfWorkUnits(numberOfWorkUnits);
  BuildTree();
}

// ---------------------------------------------------------------------------
// BuildTree
// ---------------------------------------------------------------------------

template <typename T>
void
TreeReduceAlgorithm<T>::BuildTree()
{
  const SizeValueType n = this->m_NumberOfWorkUnits;

  if (n == 0)
  {
    m_PaddedSize = 0;
    m_Values.clear();
    m_ChildrenReady.reset();
    m_InitialCounts.clear();
    return;
  }

  // Compute the smallest power of two >= n.
  m_PaddedSize = 1;
  while (m_PaddedSize < n)
  {
    m_PaddedSize <<= 1;
  }

  // Heap array is 1-indexed; total size = 2 * paddedSize (index 0 unused).
  const SizeValueType totalNodes = 2 * m_PaddedSize;

  // Reset all values to empty.
  m_Values.assign(totalNodes, std::nullopt);

  // Allocate atomic counters and zero-initialise.
  m_ChildrenReady = std::make_unique<std::atomic<int>[]>(totalNodes);
  for (SizeValueType i = 0; i < totalNodes; ++i)
  {
    m_ChildrenReady[i].store(0, std::memory_order_relaxed);
  }

  // --- Phantom leaf pre-processing ---
  //
  // For each phantom leaf (chunk k where n <= k < paddedSize) we increment
  // the parent's counter.  If a parent's counter reaches 2 (both children
  // are phantom) the parent is also fully phantom: walk further up the tree.
  //
  // We use a plain vector to accumulate the increments first, then apply
  // them atomically so the loop logic stays simple.
  std::vector<int> preCount(totalNodes, 0);
  for (SizeValueType k = n; k < m_PaddedSize; ++k)
  {
    SizeValueType nodeIdx = m_PaddedSize + k; // phantom leaf index
    while (nodeIdx > 1)
    {
      const SizeValueType parentIdx = nodeIdx / 2;
      ++preCount[parentIdx];
      if (preCount[parentIdx] < 2)
      {
        break; // sibling is real; stop propagation
      }
      // Both children of this parent are phantom: continue propagation up.
      nodeIdx = parentIdx;
    }
  }

  // Apply pre-counts and store for use by Clear().
  m_InitialCounts.assign(totalNodes, 0);
  for (SizeValueType i = 0; i < totalNodes; ++i)
  {
    m_InitialCounts[i] = preCount[i];
    m_ChildrenReady[i].store(preCount[i], std::memory_order_relaxed);
  }
}

// ---------------------------------------------------------------------------
// Merge(T&&) — not supported, always throws
// ---------------------------------------------------------------------------

template <typename T>
void
TreeReduceAlgorithm<T>::Merge(T && /*localResult*/)
{
  itkExceptionMacro("TreeReduceAlgorithm::Merge(T&&) requires a chunk ID. "
                    "Call Merge(SizeValueType chunkId, T&&) instead.");
}

// ---------------------------------------------------------------------------
// Merge(SizeValueType, T&&) — tree algorithm
// ---------------------------------------------------------------------------

template <typename T>
void
TreeReduceAlgorithm<T>::Merge(SizeValueType chunkId, T && localResult)
{
  if (m_PaddedSize == 0)
  {
    itkExceptionMacro("TreeReduceAlgorithm::Merge() called before SetNumberOfWorkUnits().");
  }
  if (chunkId >= this->m_NumberOfWorkUnits)
  {
    itkExceptionMacro("TreeReduceAlgorithm::Merge(): chunkId " << chunkId << " >= NumberOfWorkUnits "
                                                               << this->m_NumberOfWorkUnits);
  }

  // Deposit value at the leaf.
  SizeValueType nodeIdx = m_PaddedSize + chunkId;
  m_Values[nodeIdx] = std::move(localResult);

  // Walk up the tree.  At each internal node:
  //   - Atomically increment the parent's ready counter.
  //   - If we are the first child (counter was 0 before increment): return.
  //     The sibling's thread will complete the merge when it arrives.
  //   - If we are the second child (counter was 1): perform the merge and
  //     continue up to the grandparent.
  while (nodeIdx > 1)
  {
    const SizeValueType parentIdx = nodeIdx / 2;

    const int prev = m_ChildrenReady[parentIdx].fetch_add(1, std::memory_order_acq_rel);
    if (prev < 1)
    {
      // First child to arrive; the sibling will do the merge.
      return;
    }

    // Second child: perform the merge at this parent node.
    // Merge direction: left child is target (lower chunk IDs), right is source.
    const SizeValueType leftIdx = parentIdx * 2;
    const SizeValueType rightIdx = parentIdx * 2 + 1;

    if (m_Values[leftIdx].has_value() && m_Values[rightIdx].has_value())
    {
      // Both children have real values — merge right into left.
      this->m_MergeFunction(*m_Values[leftIdx], *m_Values[rightIdx]);
      m_Values[parentIdx] = std::move(m_Values[leftIdx]);
    }
    else if (m_Values[leftIdx].has_value())
    {
      // Only left has a value (right subtree was phantom).
      m_Values[parentIdx] = std::move(m_Values[leftIdx]);
    }
    else if (m_Values[rightIdx].has_value())
    {
      // Only right has a value (left subtree was phantom).
      m_Values[parentIdx] = std::move(m_Values[rightIdx]);
    }
    // else: both phantom; parent stays nullopt.

    // Release children's memory eagerly.
    m_Values[leftIdx].reset();
    m_Values[rightIdx].reset();

    nodeIdx = parentIdx;
  }
  // Reached the root (nodeIdx == 1); result is in m_Values[1].
}

// ---------------------------------------------------------------------------
// GetResult
// ---------------------------------------------------------------------------

template <typename T>
const T &
TreeReduceAlgorithm<T>::GetResult() const
{
  if (m_PaddedSize > 0 && m_Values[1].has_value())
  {
    return *m_Values[1];
  }
  return m_DefaultResult;
}

// ---------------------------------------------------------------------------
// Clear
// ---------------------------------------------------------------------------

template <typename T>
void
TreeReduceAlgorithm<T>::Clear()
{
  // Reset all values to empty.
  for (auto & v : m_Values)
  {
    v.reset();
  }

  // Restore counters to their initial (post-phantom-analysis) state.
  const SizeValueType totalNodes = static_cast<SizeValueType>(m_InitialCounts.size());
  for (SizeValueType i = 0; i < totalNodes; ++i)
  {
    m_ChildrenReady[i].store(m_InitialCounts[i], std::memory_order_relaxed);
  }

  this->Modified();
}

// ---------------------------------------------------------------------------
// PrintSelf
// ---------------------------------------------------------------------------

template <typename T>
void
TreeReduceAlgorithm<T>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PaddedSize: " << m_PaddedSize << std::endl;
  os << indent << "HasResult: " << (m_PaddedSize > 0 && m_Values[1].has_value() ? "true" : "false") << std::endl;
}

} // namespace itk

#endif // itkTreeReduceAlgorithm_hxx
