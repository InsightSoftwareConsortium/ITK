/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedEquivalencyTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkWatershedEquivalencyTable_h
#define __itkWatershedEquivalencyTable_h

#include "itkObjectFactory.h"
#include "itkDataObject.h"
#include "itkProcessObject.h"
#include "itk_hash_map.h"

namespace itk
{
namespace watershed
{

/** \class EquivalencyTable
 * A hash table for recording equivalencies among unsigned long integer
 * values. EquivalencyTable can store recursive relationships (8=7, 7=6, 6=5,
 *  ...) or be ``flattened'' to eliminate recursion.  The table uses an
 * efficient algorithm for eliminating redundancy and preventing circular 
 * dependencies.
 *
 * \par
 * In the context of the watershed segmentation algorithm
 * (itk::WatershedImageFilter), this table is used to store connections
 * identified among image segments and as the input to
 * itk::watershed::Relabeler.
 * \ingroup WatershedSegmentation
 */
class EquivalencyTable : public DataObject
{
public:
  /** Standard smart pointer declarations */
  typedef EquivalencyTable Self;
  typedef DataObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(EquivalencyTable, DataObject);

  /** Define the container type for the table. */
  typedef itk::hash_map<unsigned long, unsigned long,
    itk::hash<unsigned long> > HashTableType;
  typedef HashTableType::iterator Iterator;
  typedef HashTableType::const_iterator ConstIterator;
  typedef HashTableType::value_type ValueType;

  /** ``Flattens'' the equivalency table by eliminating all redundant and
   * recursive equivalencies.  I.e. the set { 2=1; 3=2; 4=3 } is converted
   * to {4=1; 3=1; 2=1}.   */
  void Flatten();

  /** Insert an equivalency into the table.  A return value of TRUE indicates
   * that the equivalency did not previously exist in the table and was
   * successfully added.  A FALSE return value indicates that the equivalency was 
   * not added to the table because a conflict with an existing entry occurred
   * (most likely, the equivalency was already recorded directly or
   * indirectly).  */
  bool Add(unsigned long a, unsigned long b);

  /** Lookup an equivalency in the table.  If no entry is found in the table,
   * the method returns its the value of the argument.  Does not recursively
   * descent through equivalencies.   */
  unsigned long Lookup(const unsigned long a) const
    {
      ConstIterator result = m_HashMap.find(a);
      if ( result == m_HashMap.end() ) return a;
      else return (*result).second;
    }

  /** Lookup an equivalency in the table by recursing through all successive
   * equivalencies.  For example, if the follow entries exist in the table {8=7,
   * 7=6, 6=5}, then RecursiveLookup(8) returns 5.   */
  unsigned long RecursiveLookup(const unsigned a) const;

  /** Returns TRUE if the label is found in the table and FALSE is the label is
   * not found in the table.   */
  bool IsEntry(const unsigned long a) const
    {
      if ( m_HashMap.find(a) == m_HashMap.end() ) return false;
      else return true;
    }

  /** Erases the entry with key a.  */
  void Erase(const unsigned long a)
    {  m_HashMap.erase(a); }

  /** Erases all the entries in the table.   */
  void Clear()
    {      m_HashMap.clear();    }

  /** Returns TRUE if the table is empty, FALSE if it is not empty.   */
  bool Empty() const
    {      return m_HashMap.empty();    }

  /** Returns the number of entries in the table.   */
  HashTableType::size_type Size() const
    {    return m_HashMap.size(); }

  /** Returns an iterator pointing to the first element of the (unordered)
      table.   */
  Iterator Begin() { return m_HashMap.begin(); }

  /** Returns and iterator pointing to one position past the last element of the 
   * (unordered) table.   */
  Iterator End()   { return m_HashMap.end();   }

  /** Convenience method for debugging.   */
  //  void PrintHashTable();
protected:
  EquivalencyTable()  {}
  virtual ~EquivalencyTable() {}
  EquivalencyTable(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  HashTableType m_HashMap;
  
  void UpdateOutputInformation();
  bool VerifyRequestedRegion() { return true; }
  void SetRequestedRegionToLargestPossibleRegion () {}
  bool RequestedRegionIsOutsideOfTheBufferedRegion () { return false; }
  
  void SetRequestedRegion (itk::DataObject *) {}
};
}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedEquivalencyTable.txx"
#endif

#endif

