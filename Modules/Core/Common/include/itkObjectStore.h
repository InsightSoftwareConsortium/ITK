/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkObjectStore_h
#define itkObjectStore_h

#include "itkObjectFactory.h"
#include "itkObject.h"
#include "itkIntTypes.h"
#include <vector>

namespace itk
{
/** \class ObjectStoreEnums
 *
 * \brief enums for ObjectStore
 *
 * \ingroup ITKCommon
 */
class ObjectStoreEnums
{
public:
  /**\class GrowthStrategy
   * \ingroup ITKCommon
   * Type of memory allocation strategy */
  enum class GrowthStrategy : uint8_t
  {
    LINEAR_GROWTH = 0,
    EXPONENTIAL_GROWTH = 1
  };
};
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const ObjectStoreEnums::GrowthStrategy value);

/** \class ObjectStore
 * \brief A specialized memory management object for allocating and destroying
 * contiguous blocks of objects.
 *
 * ObjectStore implements a dynamically sizeable free memory store, from which
 * instantiated objects can be borrowed and returned without always invoking
 * calls to new/delete.  This type of memory management is useful in situations
 * where calls to new/delete may be expensive, such as a multithreaded
 * environment to avoid the heap contention problem.
 *
 * ObjectStore is designed to grow dynamically.  Shrinking is a more difficult
 * problem and is only done if it will not invalidate any pointers that have
 * been "lent" to a calling application.
 *
 * This implementation uses a very simple, list-based scheme to manage
 * pointers that have been borrowed and returned.  Memory overhead incurred is
 * one pointer per object allocated.  Because of this overhead, ObjectStore is
 * not efficient for use with small objects such as native types.
 *
 * Important notes on thread-safety: This object is thread-safe in the same
 * sense that STL defines thread-safety: simultaneous operations on distinct
 * containers are safe.  It is the user's responsibility to apply appropriate
 * mutex locks if the same container is used across multiple threads. One (or
 * more) ObjectStore's can be safely be created for each thread -- and may even
 * be more efficient in terms of memory use than sharing a single ObjectStore
 * across threads. Calls to \em new and \em delete have been placed in
 * critical sections in case a compiler's implementation of new/delete is not
 * thread-safe.
 *
 * \warning  For efficiency reasons, the ObjectStore does not guard against the
 * same pointer being Returned() more than once. Doing this could result in
 * serious problems.
 * \ingroup ITKCommon
 */
template <typename TObjectType>
class ITK_TEMPLATE_EXPORT ObjectStore : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ObjectStore);

  /** Standard type alias. */
  using Self = ObjectStore;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectStore, Object);

  /** Type of the objects in storage. */
  using ObjectType = TObjectType;

  /** Type of list for storing pointers to free memory. */
  using FreeListType = std::vector<ObjectType *>;

  using GrowthStrategyEnum = ObjectStoreEnums::GrowthStrategy;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr GrowthStrategyEnum LINEAR_GROWTH = GrowthStrategyEnum::LINEAR_GROWTH;
  static constexpr GrowthStrategyEnum EXPONENTIAL_GROWTH = GrowthStrategyEnum::EXPONENTIAL_GROWTH;
#endif
  /** Borrow a pointer to an object from the memory store. */
  ObjectType *
  Borrow();

  /** Return a pointer to the memory store for reuse. WARNING: The ObjectStore
   *  assumes a pointer is returned exactly once after each time it has been
   *  borrowed. */
  void
  Return(ObjectType * p);

  /** Returns the size of the container.  This is not the number of objects
   *  available, but the total number of objects allocated. */
  itkGetConstMacro(Size, SizeValueType);

  /** Ensures that there are at least n elements allocated in the storage
   *  container.  Will not shrink the container, but may enlarge the
   *   container. */
  void
  Reserve(SizeValueType n);

  /** Attempts to free memory that is not in use and shrink the size of the
   *  container.  Not guaranteed to do anything. */
  void
  Squeeze();

  /** Frees all memory in the container */
  void
  Clear();

  /** Set/Get the linear growth size */
  itkSetMacro(LinearGrowthSize, SizeValueType);
  itkGetConstMacro(LinearGrowthSize, SizeValueType);

  /** Set/Get the growth strategy. */
  itkSetEnumMacro(GrowthStrategy, GrowthStrategyEnum);
  itkGetConstMacro(GrowthStrategy, GrowthStrategyEnum);

  /** Set growth strategy to exponential */
  void
  SetGrowthStrategyToExponential()
  {
    this->SetGrowthStrategy(GrowthStrategyEnum::EXPONENTIAL_GROWTH);
  }

  /** Set growth strategy to linear */
  void
  SetGrowthStrategyToLinear()
  {
    this->SetGrowthStrategy(GrowthStrategyEnum::LINEAR_GROWTH);
  }

protected:
  ObjectStore();
  ~ObjectStore() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Returns a new size to grow. */
  SizeValueType
  GetGrowthSize();

  struct MemoryBlock
  {
    MemoryBlock()
      : Begin(0)
    {}

    MemoryBlock(SizeValueType n)
      : Size(n)
    {
      Begin = new ObjectType[n];
    }

    ~MemoryBlock() = default; // Purposely does *not* free memory

    void
    Delete()
    {
      delete[] Begin;
    }

    ObjectType *  Begin;
    SizeValueType Size{ 0 };
  };

private:
  GrowthStrategyEnum m_GrowthStrategy;

  SizeValueType m_Size;
  SizeValueType m_LinearGrowthSize;

  /** Pointers to objects available for borrowing. */
  FreeListType m_FreeList;

  /** A list of MemoryBlocks that have been allocated. */
  std::vector<MemoryBlock> m_Store;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkObjectStore.hxx"
#endif

#endif
