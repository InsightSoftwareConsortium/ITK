/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectStore.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkObjectStore_h
#define __itkObjectStore_h

#include "itkObjectFactory.h"
#include "itkObject.h"
#include <vector>

namespace itk {

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
 * across threads. Calls to \em{new} and \em{delete} have been placed in
 * critical sections in case a compiler's implementation of new/delete is not
 * thread-safe.
 * 
 * Warnings:  For efficiency reasons, the ObjectStore does not guard against the
 * same pointer being Returned() more than once.  Doing this could result in
 * serious problems.
 */
template < class TObjectType >
class ITK_EXPORT ObjectStore : public Object
{
public:
   /** Standard typedefs. */
  typedef ObjectStore Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectStore, Object);

  /** Type of the objects in storage. */
  typedef TObjectType ObjectType;

  /** Type of list for storing pointers to free memory.*/
  typedef std::vector<ObjectType *> FreeListType;

  /** Type of memory allocation strategy */
  typedef enum {LINEAR_GROWTH = 0, EXPONENTIAL_GROWTH = 1} GrowthStrategyType;
    
  /** Borrow a pointer to an object from the memory store. */
  ObjectType *Borrow();

  /** Return a pointer to the memory store for reuse. WARNING: The ObjectStore
   *  assumes a pointer is returned exactly once after each time it has been
   *  borrowed. */
  void Return(ObjectType *p);

  /** Returns the size of the container.  This is not the number of objects
   *  available, but the total number of objects allocated. */
  itkGetMacro(Size, ::size_t);

  /** Ensures that there are at least n elements allocated in the storage
   *  container.  Will not shrink the container, but may enlarge the
   *   container. */
  void Reserve(::size_t n);

  /** Attempts to free memory that is not in use and shrink the size of the
   *  container.  Not guaranteed to do anything. */
  void Squeeze();
  
  /** Frees all memory in the container */
  void Clear();

  /** Set/Get the linear growth size */
  itkSetMacro(LinearGrowthSize, ::size_t);
  itkGetMacro(LinearGrowthSize, ::size_t);

  /** Set/Get the growth strategy. */
  itkSetMacro(GrowthStrategy, GrowthStrategyType);
  itkGetMacro(GrowthStrategy, GrowthStrategyType);

  /** Set growth strategy to exponential */
  void SetGrowthStrategyToExponential()
  { this->SetGrowthStrategy(EXPONENTIAL_GROWTH); }

  /** Set growth strategy to linear */
  void SetGrowthStrategyToLinear()
  { this->SetGrowthStrategy(LINEAR_GROWTH); }
  
protected:
  ObjectStore();
  ~ObjectStore();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Returns a new size to grow. */
  ::size_t GetGrowthSize();
  
  struct MemoryBlock
  {
    MemoryBlock(): Size(0), Begin(0) {}

    MemoryBlock(::size_t n) : Size(n)
    { Begin = new ObjectType[n];  }

    ~MemoryBlock()  {  } // Purposely does *not* free memory

    void Delete()
    { if (Begin !=0) delete[] Begin; }
    
    ObjectType *Begin;
    ::size_t Size;
  };
  
private:
  ObjectStore(const Self&);    //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  GrowthStrategyType m_GrowthStrategy;
  
  ::size_t m_Size;
  ::size_t m_LinearGrowthSize;
  
  /** Pointers to objects available for borrowing. */
  FreeListType  m_FreeList;

  /** A list of MemoryBlocks that have been allocated. */
  std::vector<MemoryBlock>   m_Store;

};

  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectStore.txx"
#endif

#endif
