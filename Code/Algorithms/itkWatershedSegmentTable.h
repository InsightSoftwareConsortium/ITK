/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedSegmentTable_h
#define __itkWatershedSegmentTable_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactory.h"
#include "itkDataObject.h"
#include "itkProcessObject.h"
#include "itk_hash_map.h"
#include <list>
#include "itkWatershedOneWayEquivalencyTable.h"

namespace itk
{
namespace watershed
{
/** \class SegmentTable
 * A table for storing segmentation information in various component filters of 
 * the watershed segmentation algorithm.  See itk::WatershedImageFilter for an
 * overview.
 *
 * \par
 * This is a hash table that holds information about labeled segments in an
 * image.  Keys in the table are label values (unsigned long). Each entry in
 * the table records the minimum value in the segment region and a list of all the
 * adjacent segments in the image.  The adjacency (edge) list also holds a
 * saliency value (likelihood of merge) for each adjacency.
 *
 * \ingroup WatershedSegmentation
 * \sa itk::WatershedImageFilter */
template <class TScalarType> 
class ITK_EXPORT SegmentTable : public DataObject
{
public:
  /** Define smart pointers for this object */
  typedef SegmentTable Self;
  typedef DataObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  typedef TScalarType ScalarType;
  itkNewMacro(Self);
  itkTypeMacro(SegmentTable, DataObject);

  /** The value type for lists of adjacencies contained in each table
      entry */
  struct edge_pair_t
  {
    edge_pair_t() {}
    edge_pair_t(unsigned long l, ScalarType s)
      : label(l), height(s) {}
    unsigned long label;
    ScalarType height;

    /** Necessary operator for sorting the adjacency lists */
    bool operator<(edge_pair_t &o)
      {
        if ( this->height < o.height ) return true;
        else return false;
      }
    
  };
  
  /** Structure for storing lists of adjacencies (edges) and their
      saliencies. */
  typedef std::list<edge_pair_t>  edge_list_t;

  /** Structure holding information about each segment in an image. */
  struct segment_t
  {
    ScalarType    min;
    edge_list_t   edge_list;
  };

  /** Define the container type for the table */
  typedef itk::hash_map<unsigned long, segment_t, itk::hash<unsigned long> >
    HashMapType;
  typedef typename HashMapType::iterator Iterator;
  typedef typename HashMapType::const_iterator ConstIterator;
  typedef typename HashMapType::value_type ValueType;
  typedef typename HashMapType::data_type  DataType;

  /** Inserts a segment into the table  */
  bool Add(unsigned long a, const segment_t &t);

  /** Iterates through the table and removes edges
   * in every edge list whose saliencies are above the
   * specified maximum.  Requires that the edge lists
   * have been sorted prior to calling this method.   */
  void PruneEdgeLists(ScalarType maximum_saliency);

  /** Lookup a segment in the table.  Returns a pointer to the
   * entry.  On failure, returns a null pointer.   */
  segment_t *Lookup(const unsigned long a)
    {
      Iterator result = m_HashMap.find(a);
      if ( result == m_HashMap.end() ) return 0;
      else return &((*result).second);
    }

  /** Lookup a segment in the table.  Returns a const pointer
   * to the entry.  On failure, returns a null pointer.   */
  const segment_t *Lookup(const unsigned long a) const
    {
      ConstIterator result = m_HashMap.find(a);
      if ( result == m_HashMap.end() ) return 0;
      else return &((*result).second);
    }

  /** Returns TRUE if the entry key is found in the table.  FALSE if the key is
   * not found in the table.   */
  bool IsEntry(const unsigned long a) const
    {
      if ( m_HashMap.find(a) == m_HashMap.end() ) return false;
      else return true;
    }

  /** Deletes an entry from the table.   */
  void Erase(const unsigned long a)
    {  m_HashMap.erase(a); }

  /** Removes all the entries in the table.   */
  void Clear()
    {      m_HashMap.clear();    }

  /** Returns true if the table is empty and false if the table is not empty.
   */
  bool Empty() const
    {      return m_HashMap.empty();    }

  /** Sorts all the entries in the edge lists from least to greatest saliency.
   */
  void SortEdgeLists();

  /** Returns the number of entries in the table.   */
  typename HashMapType::size_type  Size() const 
    {      return m_HashMap.size();     }

  /** Merges two entries of the table.  from->to  */
  //  void Merge(const unsigned long from, const unsigned long to);

  /** Returns an iterator pointing to the first element in the (unordered)
      table. */ 
  Iterator Begin() { return m_HashMap.begin(); }

  /** Returns an iterator pointing to one element past the last element in the
   * (unordered table).   */
  Iterator End()   { return m_HashMap.end();   }

  /** Returns a const iterator pointing to the first element in the (unordered)
   * table.  */
  ConstIterator Begin() const { return m_HashMap.begin(); }

  /** Returns a const iterator pointing to one element past the last element in
   * the  (unordered table).   */
  ConstIterator End()   const { return m_HashMap.end();   }

  /** Convenience methods for debugging   */
  unsigned int GetSegmentMemorySize() const
    {
      return sizeof(segment_t);
    }
  //  void PrintHashTable() const;
  
  /** Set/Get the maximum depth of image on which this segment table is based.
   * (Should set really be calling modified? jc 11/16/01) */
  void SetMaximumDepth(ScalarType s)
    {
      m_MaximumDepth = s;
      this->Modified();
    }
  ScalarType GetMaximumDepth() const
    { return m_MaximumDepth; }

  /** Copies the contents of another segment table into this segment table.
      This is really operator= in disguise, although superclass information is
      not copied. */
  void Copy(const Self& o)
    {
      m_HashMap = o.m_HashMap;
      m_MaximumDepth = o.m_MaximumDepth;
    }

protected:
  SegmentTable() {}
  virtual ~SegmentTable() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  HashMapType m_HashMap;
  ScalarType m_MaximumDepth;
  
  /** Methods required of an itk DataObject   */
  void UpdateOutputInformation();
  bool VerifyRequestedRegion() { return true; }
  void SetRequestedRegionToLargestPossibleRegion () {}
  bool RequestedRegionIsOutsideOfTheBufferedRegion () { return false; }
  void SetRequestedRegion (itk::DataObject *) {}

private:
  void operator=(const Self&) {}
  
};

}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedSegmentTable.txx"
#endif

#endif

