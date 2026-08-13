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
#ifndef itkLinearReduceAlgorithm_h
#define itkLinearReduceAlgorithm_h

#include "itkReduceAlgorithm.h"

#include <optional>
#include <vector>

namespace itk
{
/** \class LinearReduceAlgorithm
 * \brief Deterministic parallel reduce using an ordered linear merge strategy.
 *
 * Each work unit deposits its partial result into a chunk-indexed array.
 * The actual merge is deferred until GetResult() is called: it walks the
 * array once, in ascending chunk-ID order, and combines the values under a
 * single lock.  Because the merge order is fixed by chunk ID rather than by
 * thread scheduling, the final result is bit-identical across runs.  This
 * mirrors the strategy of buffering per-work-unit partial results and
 * merging them afterward in ascending work-unit order, as used to make
 * Mattes mutual information derivatives deterministic (ITK pull request
 * #6622).
 *
 * \par Usage
 * Call SetNumberOfWorkUnits() \em before any Merge() calls; this allocates
 * the internal array.  Each work unit then calls Merge(chunkId, value)
 * exactly once with its 0-based chunk identifier, in any order.  Call
 * GetResult() only after every chunk has called Merge().
 *
 * \tparam T The type of the object being reduced.
 *
 * \ingroup ITKCommon
 */
template <typename T>
class ITK_TEMPLATE_EXPORT LinearReduceAlgorithm : public ReduceAlgorithm<T>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LinearReduceAlgorithm);

  /** Standard class type aliases. */
  using Self = LinearReduceAlgorithm;
  using Superclass = ReduceAlgorithm<T>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(LinearReduceAlgorithm);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** The value type being reduced. */
  using typename Superclass::ValueType;

  /** Set the total number of work units.  Allocates the internal array.
   * Must be called before the first Merge(). */
  void
  SetNumberOfWorkUnits(SizeValueType numberOfWorkUnits) override;

  /** Not supported — use Merge(SizeValueType chunkId, T &&) instead.
   * Always throws itk::ExceptionObject. */
  void
  Merge(T && localResult) override;

  /** Deposit \p localResult for chunk \p chunkId.  Thread-safe; merely
   * stores the value, the actual merge happens lazily in GetResult(). */
  void
  Merge(SizeValueType chunkId, T && localResult) override;

  /** Merge every deposited chunk, in ascending chunk-ID order, and return
   * the result.  The merge is performed once, under a lock, and cached;
   * later calls return the cached result.  Behaviour is defined only after
   * every chunk has called Merge(). */
  const T &
  GetResult() const override;

  /** Reset the array so this object can be reused with the same N.
   * Does not change the merge function or work-unit count. */
  void
  Clear() override;

protected:
  LinearReduceAlgorithm() = default;
  ~LinearReduceAlgorithm() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Chunk-indexed array of partial results awaiting the merge in
   * GetResult().  Mutable because GetResult() consumes it lazily. */
  mutable std::vector<std::optional<T>> m_Values{};

  /** Cached result of merging m_Values, computed lazily by GetResult(). */
  mutable T m_Result{};

  /** True once GetResult() has performed the merge. */
  mutable bool m_Merged{ false };
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLinearReduceAlgorithm.hxx"
#endif

#endif // itkLinearReduceAlgorithm_h
