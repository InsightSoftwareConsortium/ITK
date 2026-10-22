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
#ifndef itkTreeReduceAlgorithm_h
#define itkTreeReduceAlgorithm_h

#include "itkReduceAlgorithm.h"

#include <atomic>
#include <memory>
#include <optional>
#include <vector>

namespace itk
{
/** \class TreeReduceAlgorithm
 * \brief Deterministic parallel reduce using a binary-tree merge strategy.
 *
 * Merges per-work-unit partial results in a fixed binary-tree order, so the
 * final value is bit-identical across runs regardless of thread scheduling.
 * This makes it suitable for floating-point reductions where reproducibility
 * matters.
 *
 * \par Usage
 * Call SetNumberOfWorkUnits() \em before any Merge() calls; this allocates
 * and initialises the internal heap-like tree.  Each work unit then calls
 * Merge(chunkId, value) exactly once with its 0-based chunk identifier.
 * GetResult() returns the root value once all chunks have merged.
 *
 * \par Tree layout
 * The tree is stored 1-indexed in a flat array of size \c 2*paddedN, where
 * \c paddedN is the smallest power of two ≥ N.  Chunk \c k occupies leaf
 * index \c paddedN+k; the root is at index 1.  Internal nodes are populated
 * only by merge operations; leaf data is moved up the tree eagerly after
 * each chunk arrives.
 *
 * \par Merge direction
 * At every internal node the \em left child (lower chunk-ID subtree) is the
 * target and the \em right child is the source:
 * \code
 *   mergeFunction(leftValue, rightValue);
 * \endcode
 * This order is independent of which thread executes the merge.
 *
 * \par Non-blocking behaviour
 * Merge(chunkId, value) returns as soon as it cannot continue up the tree
 * because the sibling has not yet arrived.  There is no spinning or blocking.
 * GetResult() is undefined until every chunk has called Merge().
 *
 * \par Arbitrary N
 * When N is not a power of two the tree is padded with phantom leaves that
 * carry no value.  Phantom subtrees are pre-processed at build time so that
 * real chunks still walk up the tree correctly without performing spurious
 * merges.
 *
 * \par Memory
 * Each node's value is a \c std::optional<T>; after both children of a node
 * have been merged the children's storage is released (\c reset()), so peak
 * memory is O(N) rather than O(2N).
 *
 * \tparam T The type of the object being reduced.
 *
 * \ingroup ITKCommon
 */
template <typename T>
class ITK_TEMPLATE_EXPORT TreeReduceAlgorithm : public ReduceAlgorithm<T>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TreeReduceAlgorithm);

  /** Standard class type aliases. */
  using Self = TreeReduceAlgorithm;
  using Superclass = ReduceAlgorithm<T>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(TreeReduceAlgorithm);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** The value type being reduced. */
  using typename Superclass::ValueType;

  /** Set the total number of work units.  Rebuilds the internal binary tree.
   * Must be called before the first Merge(). */
  void
  SetNumberOfWorkUnits(SizeValueType numberOfWorkUnits) override;

  /** Not supported — use Merge(SizeValueType chunkId, T &&) instead.
   * Always throws itk::ExceptionObject. */
  void
  Merge(T && localResult) override;

  /** Deposit \p localResult for chunk \p chunkId and walk up the tree,
   * merging completed sibling pairs until either the root is reached or
   * the sibling has not yet arrived.  Thread-safe; non-blocking. */
  void
  Merge(SizeValueType chunkId, T && localResult) override;

  /** Return the fully reduced result at the tree root.
   * Behaviour is defined only after every chunk has called Merge(). */
  const T &
  GetResult() const override;

  /** Reset the tree so this object can be reused with the same N.
   * Does not change the merge function or work-unit count. */
  void
  Clear() override;

protected:
  TreeReduceAlgorithm() = default;
  ~TreeReduceAlgorithm() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Build (or rebuild) the internal heap-like tree based on
   * m_NumberOfWorkUnits.  Pre-processes phantom leaves so that the atomic
   * counters are initialised correctly. */
  void
  BuildTree();

  /** Smallest power of two ≥ m_NumberOfWorkUnits.  Leaves live at indices
   * [m_PaddedSize, 2*m_PaddedSize). */
  SizeValueType m_PaddedSize{ 0 };

  /** 1-indexed heap array.  Index 0 is unused; index 1 is the root;
   * leaves are at [m_PaddedSize, 2*m_PaddedSize). */
  std::vector<std::optional<T>> m_Values{};

  /** Per-node atomic counter: incremented by each child that completes.
   * Reaches 2 when both children are ready; the second thread to arrive
   * performs the merge.  Stored as a unique_ptr because std::atomic is
   * not copyable/movable. */
  std::unique_ptr<std::atomic<int>[]> m_ChildrenReady{};

  /** Initial values of m_ChildrenReady (includes phantom pre-increments).
   * Used by Clear() to reset the counters without re-analysing phantoms. */
  std::vector<int> m_InitialCounts{};

  /** Returned by GetResult() when no chunks have merged yet (tree is empty). */
  T m_DefaultResult{};
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTreeReduceAlgorithm.hxx"
#endif

#endif // itkTreeReduceAlgorithm_h
