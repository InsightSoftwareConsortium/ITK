/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOneWayEquivalencyTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedOneWayEquivalencyTable_h
#define __itkWatershedOneWayEquivalencyTable_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactory.h"
#include "itkDataObject.h"
#include "itkProcessObject.h"
#include "itk_hash_map.h"

namespace itk
{
namespace watershed
{
/** \class OneWayEquivalencyTable
 *    A variation on itk::watershed::EquivalencyTable that preserves the order
 * of equivalencies.  For example, the entries { 5 = 4 } and {4 = 5} are not 
 * equivalent in this table, yet are the treated as the same entry in
 * EquivalencyTable.  Because of the one-way logic of the table, reflexive
 * equivalencies will result in cycling from recursive lookups or flattening of
 * the table.  The responsibility is on the user for preventing recursive
 * cycling. 
 *
 * \par
 * See itk::watershed::EquivalencyTable for more information
 * \ingroup WatershedSegmentation
 *
 * \sa EquivalencyTable  */
class OneWayEquivalencyTable : public DataObject
{
public:
  /**  Standard typedefs and smart pointer declarations.   */
  typedef OneWayEquivalencyTable Self;
  typedef DataObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(OneWayEquivalencyTable, DataObject);

  /** Define the container type for this table */
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
  * (most likely, the equivalency was already recorded directly or indirectly).
  */
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
  
  /**  Erases the entry with key a.   */
  void Erase(const unsigned long a)
    {  m_HashMap.erase(a); }

  /** Erases all the entries in the table.   */
  void Clear()
    {      m_HashMap.clear();    }

  /** Returns TRUE if the table is empty, FALSE if it is not empty.   */
  bool Empty() const
    {      return m_HashMap.empty();    }
  
  /** Returns an iterator pointing to the first element of the (unordered)
      table.    */
  Iterator Begin() { return m_HashMap.begin(); }

  /** Returns and iterator pointing to one position past the last element of the 
   * (unordered) table.    */  
  Iterator End()   { return m_HashMap.end();   }

  /** Convenience method for debugging.   */
  //  void PrintHashTable();
  
protected:
  OneWayEquivalencyTable()  {}
  virtual ~OneWayEquivalencyTable() {}
  OneWayEquivalencyTable(const Self&); // purposely not implemented
  void operator=(const Self&); // purposely not implemented
  void PrintSelf(std::ostream& os, Indent indent) const;

  HashTableType m_HashMap;
  
  void UpdateOutputInformation();
  bool VerifyRequestedRegion() {return true; }
  void SetRequestedRegionToLargestPossibleRegion () {}
  bool RequestedRegionIsOutsideOfTheBufferedRegion () { return false; }
  void SetRequestedRegion (itk::DataObject *) {}
};
}// end namespace watershed
}// end namespace itk

#endif

