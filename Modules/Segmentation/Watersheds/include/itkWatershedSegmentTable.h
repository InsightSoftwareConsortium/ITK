/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkWatershedSegmentTable_h
#define itkWatershedSegmentTable_h


#include "itkDataObject.h"
#include <list>
#include "itkOneWayEquivalencyTable.h"

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
 * image.  Keys in the table are label values (IdentifierType). Each entry in
 * the table records the minimum value in the segment region and a list of all the
 * adjacent segments in the image.  The adjacency (edge) list also holds a
 * saliency value (likelihood of merge) for each adjacency.
 *
 * \ingroup WatershedSegmentation
 * \sa itk::WatershedImageFilter
 * \ingroup ITKWatersheds
 */
template< typename TScalar >
class ITK_TEMPLATE_EXPORT SegmentTable:public DataObject
{
public:
  /** Define smart pointers for this object */
  typedef SegmentTable               Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef TScalar                    ScalarType;

  itkNewMacro(Self);
  itkTypeMacro(WatershedSegmentTable, DataObject);

  /** The value type for lists of adjacencies contained in each table
      entry */
  struct edge_pair_t {
    edge_pair_t() {}
    edge_pair_t(IdentifierType l, ScalarType s):label(l), height(s) {}
    IdentifierType label;
    ScalarType height;

    /** Necessary operator for sorting the adjacency lists */
    bool operator<(const edge_pair_t & o) const
    {
      if ( this->height < o.height )
        {
        return true;
        }
      else
        {
        return false;
        }
    }
  };

  /** Structure for storing lists of adjacencies (edges) and their
      saliencies. */
  typedef std::list< edge_pair_t > edge_list_t;

  /** Structure holding information about each segment in an image. */
  struct segment_t {
    ScalarType min;
    edge_list_t edge_list;
  };

  /** Define the container type for the table */
  typedef itksys::hash_map< IdentifierType, segment_t,
    itksys::hash< IdentifierType > >           HashMapType;
  typedef typename HashMapType::iterator       Iterator;
  typedef typename HashMapType::const_iterator ConstIterator;
  typedef typename HashMapType::value_type     ValueType;
  typedef typename HashMapType::data_type      DataType;

  /** Inserts a segment into the table  */
  bool Add(IdentifierType a, const segment_t & t);

  /** Iterates through the table and removes edges
   * in every edge list whose saliencies are above the
   * specified maximum.  Requires that the edge lists
   * have been sorted prior to calling this method.   */
  void PruneEdgeLists(ScalarType maximum_saliency);

  /** Lookup a segment in the table.  Returns a pointer to the
   * entry.  On failure, returns a null pointer.   */
  segment_t * Lookup(const IdentifierType a)
  {
    Iterator result = m_HashMap.find(a);

    if ( result == m_HashMap.end() ) { return ITK_NULLPTR; }
    else { return &( ( *result ).second ); }
  }

  /** Lookup a segment in the table.  Returns a const pointer
   * to the entry.  On failure, returns a null pointer.   */
  const segment_t * Lookup(const IdentifierType a) const
  {
    ConstIterator result = m_HashMap.find(a);

    if ( result == m_HashMap.end() ) { return 0; }
    else { return &( ( *result ).second ); }
  }

  /** Returns TRUE if the entry key is found in the table.  FALSE if the key is
   * not found in the table.   */
  bool IsEntry(const IdentifierType a) const
  {
    if ( m_HashMap.find(a) == m_HashMap.end() ) { return false; }
    else { return true; }
  }

  /** Deletes an entry from the table.   */
  void Erase(const IdentifierType a)
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
  //  void Merge(const IdentifierType from, const IdentifierType to);

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
    return sizeof( segment_t );
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
  void Copy(const Self & o)
  {
    m_HashMap = o.m_HashMap;
    m_MaximumDepth = o.m_MaximumDepth;
  }

protected:
  SegmentTable() :
    m_MaximumDepth(0)
  {}
  virtual ~SegmentTable() ITK_OVERRIDE {}

  HashMapType m_HashMap;

  ScalarType m_MaximumDepth;

private:
  void operator=(const Self &) {}
};
} // end namespace watershed
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedSegmentTable.hxx"
#endif

#endif
