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
#ifndef __itkFlatStructuringElement_h
#define __itkFlatStructuringElement_h

#include "itkNeighborhood.h"
#include <vector>
#include "itkVector.h"

namespace itk
{
/** \class FlatStructuringElement
 * \brief A class to support a variety of flat structuring elements,
 * including versions created by decomposition of lines.
 *
 * FlatStructuringElement provides several static methods, which can
 * be used to create a structuring element with a particular shape,
 * size, etc. Currently, those methods allow to create a ball, a box,
 * a cross structuring element, and let create a structuring element
 * based on an image.
 *
 * \ingroup ITKMathematicalMorphology
 *
 * \wiki
 * \wikiexample{Morphology/FlatStructuringElement,Erode a binary image using a flat (box) structuring element}
 * \endwiki
 */

template< unsigned int VDimension >
class FlatStructuringElement:public Neighborhood< bool, VDimension >
{
public:
  /** Standard class typedefs. */
  typedef FlatStructuringElement< VDimension > Self;
  typedef Neighborhood< bool, VDimension >     Superclass;

  /** External support for pixel type. */
  typedef typename Superclass::PixelType PixelType;

  /** Iterator typedef support. Note the naming is intentional, i.e.,
  * AllocatorType::iterator and AllocatorType::const_iterator, because the
  * allocator may be a vnl object or other type, which uses this form. */
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;

  /** Size and value typedef support. */
  typedef typename Superclass::SizeType      SizeType;
  typedef typename Superclass::OffsetType    OffsetType;

  /** Radius typedef support. */
  typedef typename Superclass::RadiusType RadiusType;

  /** External slice iterator type typedef support. */
  typedef typename Superclass::SliceIteratorType SliceIteratorType;

  /** External support for dimensionality. */
  itkStaticConstMacro(NeighborhoodDimension, unsigned int, VDimension);

  typedef Vector< float, VDimension > LType;
  typedef std::vector< LType >        DecompType;

  /** Default destructor. */
  virtual ~FlatStructuringElement() {}

  /** Default consructor. */
  FlatStructuringElement() { m_Decomposable = false; }

  /** Various constructors */

  /**
   * Create a box structuring element. The structuring element is
   * is decomposable.
   */
  static Self Box(RadiusType radius);

  /** Create a ball structuring element */
  static Self Ball(RadiusType radius);

  /** Create a cross structuring element */
  static Self Cross(RadiusType radius);

  /** Create an annulus structuring element */
  static Self Annulus(RadiusType radius,
                      unsigned int thickness = 1,
                      bool includeCenter = false);

  /**
   * Create a polygon structuring element. The structuring element is
   * is decomposable.
   * lines is the number of elements in the decomposition
   */
  static Self Polygon(RadiusType radius, unsigned lines);

  /**
   * Returns wether the structuring element is decomposable or not. If the
   * structuring is decomposable, the set of lines associated with the
   * structuring may be used by an algorithm instead of the standard buffer.
   */
  bool GetDecomposable() const
  {
    return m_Decomposable;
  }
  void SetDecomposable( bool v )
  {
    m_Decomposable = v;
  }

  /** Return the lines associated with the structuring element */
  const DecompType & GetLines() const
  {
    return ( m_Lines );
  }
  void AddLine( LType l )
  {
    m_Lines.push_back(l);
  }

  bool CheckParallel(LType NewVec) const;

  /**
   * Fill the buffer of the structuring element based on the lines
   * associated to the structuring element
   */
  void ComputeBufferFromLines();

protected:

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  bool m_Decomposable;

  DecompType m_Lines;

  template< unsigned int VDimension3 >
  struct StructuringElementFacet {
    Vector< float, VDimension3 > P1, P2, P3;
  };
  typedef StructuringElementFacet< VDimension > FacetType;

  template<typename TStructuringElement, typename TRadius>
  static void GeneratePolygon(TStructuringElement & res,            TRadius      radius, unsigned lines);
  static void GeneratePolygon(itk::FlatStructuringElement<2> & res, itk::Size<2> radius, unsigned lines);
  static void GeneratePolygon(itk::FlatStructuringElement<3> & res, itk::Size<3> radius, unsigned lines);

  typedef Vector< float, 2 >           LType2;

  typedef Vector< float, 3 >           LType3;
  typedef StructuringElementFacet< 3 > FacetType3;

};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFlatStructuringElement.hxx"
#endif

#endif
