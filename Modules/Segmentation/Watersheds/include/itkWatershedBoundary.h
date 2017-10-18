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
#ifndef itkWatershedBoundary_h
#define itkWatershedBoundary_h


#include <list>
#include <vector>
#include "itkImage.h"
#include "itkProcessObject.h"
#include "itksys/hash_map.hxx"

namespace itk
{
namespace watershed
{
/** \class Boundary
 * \par
 * A data object for used by watershed segmentation process objects in
 * streaming applications. A "boundary" represents the single-pixel wide
 * surface of an image chunk. This class is used to store information needed to
 * resolve processing at chunk  boundaries after data streaming of the
 * watershed segmentation algorithm.
 *
 * \par
 * This is an itkDataObject.  It contains a matrix of "faces" of an
 * N-dimensional hypercube. A chunk of a volume with \f$ N \f$ dimensions
 * has \f$ 2N \f$ faces of dimension \f$ N-1 \f$. Some examples: A
 * 2-dimensional image has 4 faces that are lines.  A 3-dimensional image has 6
 * faces that are planes.  A 4-dimensional image has 8 faces which are
 * cubes. Faces are indexed as \f$ N \f$ sets of pairs \f$ (low, high) \f$.
 *
 * \sa WatershedSegmenter
 * \sa WatershedBoundaryResolver
 * \ingroup WatershedSegmentation
 * \ingroup ITKWatersheds
 */
template< typename TScalar, unsigned int TDimension >
class ITK_TEMPLATE_EXPORT Boundary:public DataObject
{
public:
  /** The dimensionality of this boundary.  For example, if the boundary
   * of a set of planes, it has dimensionality 2.  If the boundary is
   * a set of lines, it has dimensionality 1.  Dimensionality is one less
   * than the image chunks from which the boundary is derived.   */
  itkStaticConstMacro(Dimension, unsigned int, TDimension);

  /** A pair of values used to index into the boundary data structure.
   * The IndexType.first is the dimension of the face and IndexType.second is a
   * binary value 0 or 1 indicating the LOW face or the HIGH face,
   * respectively.    */
  typedef std::pair< unsigned, unsigned >     IndexType;
  typedef Image< IdentifierType, TDimension > ImageType;
  typedef typename ImageType::IndexType       ImageIndexType;
  typedef TScalar                             ScalarType;

  /** Data type stored at each pixel in a face.   */
  struct face_pixel_t {
    /**Index of the direction of watershed flow through this pixel.
     * A negative value indicates that the flow does not move out
     * of the region.  A positive value is the index into the
     * pixel neighborhood of the facing chunk boundary into which
     * flow moves.
     *
     * Note that the range of values of the index depends on the
     * the connectivity used by the watershed segmentation algorithm.
     * If the WS algorithm uses city-block style connectivity (4-connectivity
     * in 2D, 6-connectivity in 3D, etc) this could actually be a boolean
     * value indicating inward or outward flow since there is only one
     * valid neighbor to reference.  For extensibility to other
     * connectivities, this flow value can be used to index a number of
     * different neighbors.     */
    short flow;

    /** The label associated with this pixel.     */
    IdentifierType label;
  };

  /**    */
  struct flat_region_t {
    /** Indices into the associated Face containing boundary pixels.  These
     * give access to spatial information, label and flow associated with
     * this boundary pixel connection.     */
    std::list< IdentifierType > offset_list;

    /** The value of the lowest point (indicating the steepest descent) along
     * the boundary of the flat region of which this pixel is a member.     */
    ScalarType bounds_min;

    /** The label associated with the lowest point
     * point along this flat region boundary.     */
    IdentifierType min_label;

    /** The value of this flat region     */
    ScalarType value;
  };

  /** The face data structure.  This is just an Image of face pixel
      types. */
  typedef Image< face_pixel_t, TDimension > face_t;

  /** A hash table holding flat region data structures.   */
  typedef itksys::hash_map< IdentifierType, flat_region_t,
                            itksys::hash< IdentifierType > > flat_hash_t;
  typedef typename flat_hash_t::value_type                   FlatHashValueType;

  /** Itk typedefs and macros defining smart pointer and type identification.
   */
  typedef Boundary                   Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(WatershedBoundary, DataObject);

  /** The following averts an internal compiler error on microsoft compilers */
  typedef typename face_t::Pointer FacePointer;

  /** Returns the face at the specified index  */
  FacePointer GetFace(const IndexType & idx)
  {      return this->GetFace(idx.first, idx.second);    }

  /** Returns the face at the specified index, where dimension is
   * the number of the axial dimension and highlow is 0 for the LOW
   * face and 1 for the HIGH face.   */
  FacePointer GetFace(unsigned dimension, unsigned highlow)
  {
    if ( highlow == 0 ) { return m_Faces[dimension].first; }
    else { return m_Faces[dimension].second; }
  }

  void SetFace(FacePointer f, const IndexType & idx)
  { this->SetFace(f, idx.first, idx.second); }

  void SetFace(FacePointer f, unsigned dimension, unsigned highlow)
  {
    if ( highlow == 0 ) { m_Faces[dimension].first = f; }
    else { m_Faces[dimension].second = f; }
    this->Modified();
  }

  /** Get/Set the table of flat region connections specified by the index. */
  flat_hash_t * GetFlatHash(const IndexType & idx)
  { return this->GetFlatHash(idx.first, idx.second); }
  flat_hash_t * GetFlatHash(unsigned dimension, unsigned highlow)
  {
    if ( highlow == 0 ) { return &( m_FlatHashes[dimension].first ); }
    else { return &( m_FlatHashes[dimension].second ); }
  }

  void SetFlatHash(flat_hash_t & l, const IndexType & idx)
  { this->SetFlatHash(l, idx.first, idx.second); }
  void SetFlatHash(flat_hash_t & l, unsigned dimension,
                   unsigned highlow)
  {
    if ( highlow == 0 ) { m_FlatHashes[dimension].first = l; }
    else { m_FlatHashes[dimension].second = l; }
    this->Modified();
  }

  /** Marks a face in the boundary object as either valid (true) or
   * invalid (false).  A valid face is assumed to be initialized
   * and contain information.  No assumptions are made about an
   * invalid face.   */
  void SetValid(bool & l, const IndexType & idx)
  { this->SetValid(l, idx.first, idx.second); }
  void SetValid(bool b, unsigned dimension,
                unsigned highlow)
  {
    if ( highlow == 0 ) { m_Valid[dimension].first = b; }
    else { m_Valid[dimension].second = b; }
    this->Modified();
  }

  bool GetValid(const IndexType & idx) const
  { return this->GetValid(idx.first, idx.second); }
  bool GetValid(unsigned dimension, unsigned highlow) const
  {
    if ( highlow == 0 ) { return m_Valid[dimension].first; }
    else { return m_Valid[dimension].second; }
  }

protected:
  Boundary();
  virtual ~Boundary() ITK_OVERRIDE {}
  Boundary(const Self &) {}
  void operator=(const Self &) {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** The Nx2 matrix of faces of this boundary.   */
  std::vector< std::pair< FacePointer, FacePointer > > m_Faces;

  /** The Nx2 matrix flat region connections associated with
   * this boundary.   */
  std::vector< std::pair< flat_hash_t, flat_hash_t > > m_FlatHashes;

  /** The Nx2 matrix of boolean flags indicating which faces contain
   * information.    */
  std::vector< std::pair< bool, bool > > m_Valid;
};
} // end namespace watershed
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedBoundary.hxx"
#endif

#endif
