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
#ifndef itkEquivalencyTable_h
#define itkEquivalencyTable_h


#include "itkProcessObject.h"
#include <unordered_map>

namespace itk
{
/** \class EquivalencyTable
 * \brief Hash table to manage integral label equivalencies
 *
 * EquivalencyTable is a hash table for recording equivalencies among
 * unsigned long integer values. EquivalencyTable can store recursive
 * relationships (8=7, 7=6, 6=5, ...) or be "flattened" to eliminate
 * recursion.  The table uses an efficient algorithm for eliminating
 * redundancy and preventing circular dependencies.
 *
 * \par
 * In the context of the watershed segmentation algorithm
 * (itk::WatershedImageFilter), this table is used to store connections
 * identified among image segments and as the input to
 * itk::watershed::Relabeler.
 * \ingroup WatershedSegmentation
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT EquivalencyTable : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(EquivalencyTable);

  /** Standard smart pointer declarations */
  using Self = EquivalencyTable;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(EquivalencyTable, DataObject);

  /** Define the container type for the table. */
  using HashTableType = std::unordered_map<unsigned long, unsigned long, std::hash<unsigned long>>;

  using Iterator = HashTableType::iterator;
  using ConstIterator = HashTableType::const_iterator;
  using ValueType = HashTableType::value_type;

  /** "Flattens" the equivalency table by eliminating all redundant
   * and recursive equivalencies.  I.e. the set { 2=1; 3=2; 4=3 } is
   * converted to {4=1; 3=1; 2=1}.  */
  void
  Flatten();

  /** Insert an equivalency into the table.  A return value of TRUE
   * indicates that the equivalency did not previously exist in the
   * table and was successfully added.  A FALSE return value indicates
   * that the equivalency was not added to the table because a
   * conflict with an existing entry occurred (most likely, the
   * equivalency was already recorded directly or indirectly).  */
  bool
  Add(unsigned long a, unsigned long b);

  /** Insert an equivalency into the table and flatten that
   * equivalency. A return value of TRUE indicates that the
   * equivalency did not previously exist in the table and was
   * successfully added.  A FALSE return value indicates that the
   * equivalency was not added to the table because a conflict with an
   * existing entry occurred (most likely, the equivalency was already
   * recorded directly or indirectly). */
  bool
  AddAndFlatten(unsigned long a, unsigned long b);

  /** Lookup an equivalency in the table.  If no entry is found in the
   * table, the method returns its the value of the argument.  Does
   * not recursively descent through equivalencies.  */
  unsigned long
  Lookup(const unsigned long a) const
  {
    auto result = m_HashMap.find(a);

    if (result == m_HashMap.end())
    {
      return a;
    }
    else
    {
      return (*result).second;
    }
  }

  /** Lookup an equivalency in the table by recursing through all
   * successive equivalencies.  For example, if the follow entries
   * exist in the table {8=7, 7=6, 6=5}, then RecursiveLookup(8)
   * returns 5.  */
  unsigned long
  RecursiveLookup(const unsigned long a) const;

  /** Returns TRUE if the label is found in the table and FALSE is the label is
   * not found in the table.   */
  bool
  IsEntry(const unsigned long a) const
  {
    if (m_HashMap.find(a) == m_HashMap.end())
    {
      return false;
    }
    else
    {
      return true;
    }
  }

  /** Erases the entry with key a.  */
  void
  Erase(const unsigned long a)
  {
    m_HashMap.erase(a);
  }

  /** Erases all the entries in the table.   */
  void
  Clear()
  {
    m_HashMap.clear();
  }

  /** Returns TRUE if the table is empty, FALSE if it is not empty.   */
  bool
  Empty() const
  {
    return m_HashMap.empty();
  }

  /** Returns the number of entries in the table.   */
  HashTableType::size_type
  Size() const
  {
    return m_HashMap.size();
  }

  /** Returns an iterator pointing to the first element of the (unordered)
   *  table.   */
  Iterator
  Begin()
  {
    return m_HashMap.begin();
  }

  /** Returns and iterator pointing to one position past the last
   * element of the (unordered) table.  */
  Iterator
  End()
  {
    return m_HashMap.end();
  }

  /** Convenience method for debugging.   */
  //  void PrintHashTable();

protected:
  EquivalencyTable() = default;
  ~EquivalencyTable() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  HashTableType m_HashMap;
};
} // end namespace itk

#endif
