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
#ifndef itkGreedyReduceAlgorithm_h
#define itkGreedyReduceAlgorithm_h

#include "itkReduceAlgorithm.h"

namespace itk
{
/** \class GreedyReduceAlgorithm
 * \brief Thread-safe parallel reduce using a greedy swap-and-merge strategy.
 *
 * Implements the parallel reduction pattern used in several ITK filters
 * (e.g. LabelStatisticsImageFilter, LabelOverlapMeasuresImageFilter,
 * ImageToHistogramFilter).  Each thread calls Merge() with its local
 * partial result; the implementation combines them without holding the
 * global mutex while performing the (potentially expensive) merge step.
 *
 * \par Non-deterministic merge order
 * The order in which partial results are combined depends on thread
 * scheduling and is not deterministic across runs.  Operations whose
 * result is independent of evaluation order (e.g. integer counts,
 * minimum, maximum) produce bit-identical outputs.  Floating-point
 * reductions (e.g. running sums, means) may produce results that differ
 * in the last few ULPs between runs due to non-associativity of
 * floating-point arithmetic.
 *
 * Algorithm (per Merge() call):
 * -# Acquire the mutex.
 * -# If no result is accumulated yet, store the local result and return.
 * -# Otherwise, atomically take ownership of the current accumulated result,
 *    clearing the shared state to allow other threads to proceed immediately.
 * -# Release the mutex.
 * -# Merge the taken result into the local result (off the critical path).
 * -# Repeat from step 1 until the local result is successfully deposited.
 *
 * A merge function must be supplied via SetMergeFunction() before calling
 * Merge(). The function signature is:
 * \code
 *   void mergeFunction(T & target, T & source);
 * \endcode
 * It must merge \c source into \c target.
 *
 * \tparam T The type of the object being reduced.
 *
 * \ingroup ITKCommon
 */
template <typename T>
class ITK_TEMPLATE_EXPORT GreedyReduceAlgorithm : public ReduceAlgorithm<T>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GreedyReduceAlgorithm);

  /** Standard class type aliases. */
  using Self = GreedyReduceAlgorithm;
  using Superclass = ReduceAlgorithm<T>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(GreedyReduceAlgorithm);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** The value type being reduced. */
  using typename Superclass::ValueType;

  /** Bring the chunk-ID overload into scope; Greedy ignores the ID and
   * forwards to Merge(T&&) (see ReduceAlgorithm::Merge(SizeValueType, T&&)). */
  using Superclass::Merge;

  /** Merge a per-thread \p localResult into the accumulated output.
   * This method is thread-safe; multiple threads may call it concurrently. */
  void
  Merge(T && localResult) override;

  /** Return the accumulated result.
   * Should only be called after all concurrent Merge() calls have finished. */
  const T &
  GetResult() const override;

  /** Reset accumulated state so this object can be reused. */
  void
  Clear() override;

protected:
  GreedyReduceAlgorithm() = default;
  ~GreedyReduceAlgorithm() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Accumulated result. Default-constructed value serves as initial state. */
  T m_Result{};

  /** True once the first Merge() has deposited a value. */
  bool m_HasResult{ false };
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGreedyReduceAlgorithm.hxx"
#endif

#endif // itkGreedyReduceAlgorithm_h
