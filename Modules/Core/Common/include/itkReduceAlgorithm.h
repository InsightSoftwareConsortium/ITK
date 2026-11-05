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
#ifndef itkReduceAlgorithm_h
#define itkReduceAlgorithm_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#include <functional>
#include <mutex>

namespace itk
{
/** \class ReduceAlgorithm
 * \brief Abstract base class for thread-safe parallel reduce (merge) algorithms.
 *
 * Defines the interface for combining per-thread local results into a single
 * accumulated output.  Concrete subclasses implement `Merge()` and
 * `GetResult()` and are free to choose any reduction strategy (greedy,
 * tree-based, etc.).
 *
 * The merge function has the signature:
 * \code
 *   void mergeFunction(T & target, T & source);
 * \endcode
 * and must merge \c source \em into \c target.
 *
 * \tparam T The type of the object being reduced.
 *
 * \ingroup ITKCommon
 */
template <typename T>
class ITK_TEMPLATE_EXPORT ReduceAlgorithm : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ReduceAlgorithm);

  /** Standard class type aliases. */
  using Self = ReduceAlgorithm;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(ReduceAlgorithm);

  /** The value type being reduced. */
  using ValueType = T;

  /** Merge function signature: merges \p source into \p target. */
  using MergeFunctionType = std::function<void(T &, T &)>;

  /** Set the function used to merge two objects. */
  virtual void
  SetMergeFunction(MergeFunctionType mergeFunction)
  {
    m_MergeFunction = std::move(mergeFunction);
    this->Modified();
  }

  /** Get the merge function. */
  const MergeFunctionType &
  GetMergeFunction() const
  {
    return m_MergeFunction;
  }

  /** Set the expected number of work units (chunks) that will call Merge().
   * Stored for use by concrete subclasses or future algorithms; not enforced
   * by all implementations. */
  itkSetMacro(NumberOfWorkUnits, SizeValueType);

  /** Get the number of work units. */
  itkGetConstMacro(NumberOfWorkUnits, SizeValueType);

  /** Merge a local result into the accumulated output.
   * Implementations must be thread-safe. */
  virtual void
  Merge(T && localResult) = 0;

  /** Merge a local result identified by \p chunkId (0-based) into the
   * accumulated output.  The default implementation ignores the ID and
   * delegates to Merge(T&&).  Subclasses that need deterministic ordering
   * (e.g. TreeReduceAlgorithm) override this to exploit the chunk ID. */
  virtual void
  Merge(SizeValueType /*chunkId*/, T && localResult)
  {
    this->Merge(std::move(localResult));
  }

  /** Return the accumulated result after all Merge() calls have completed.
   * The returned reference is only stable while no concurrent Merge() or
   * Clear() is in progress. */
  virtual const T &
  GetResult() const = 0;

  /** Reset internal state so the object can be reused for a new reduction. */
  virtual void
  Clear()
  {
    m_NumberOfWorkUnits = 0;
    this->Modified();
  }

protected:
  ReduceAlgorithm() = default;
  ~ReduceAlgorithm() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "NumberOfWorkUnits: " << m_NumberOfWorkUnits << std::endl;
    os << indent << "MergeFunction: " << (m_MergeFunction ? "set" : "not set") << std::endl;
  }

  /** Merge function provided by the caller. */
  MergeFunctionType m_MergeFunction{};

  /** Expected number of work units. */
  SizeValueType m_NumberOfWorkUnits{ 0 };

  /** Mutex protecting the accumulated result. */
  mutable std::mutex m_Mutex{};
};

} // namespace itk

#endif // itkReduceAlgorithm_h
