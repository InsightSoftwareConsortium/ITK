/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlatStructuringElement.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFlatStructuringElement_h
#define __itkFlatStructuringElement_h

#include "itkNeighborhood.h"
#include "itkSize.h"
#include "itkOffset.h"
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
 */

template< unsigned int VDimension >
class ITK_EXPORT FlatStructuringElement:public Neighborhood< bool, VDimension >
{
public:
  /** Standard class typedefs. */
  typedef FlatStructuringElement< VDimension > Self;
  typedef Neighborhood< bool, VDimension >     Superclass;

  /** External support for pixel type. */
  typedef typename Superclass::PixelType PixelType;

  /** Iterator typedef support. Note the naming is intentional, i.e.,
  * ::iterator and ::const_iterator, because the allocator may be a
  * vnl object or other type, which uses this form. */
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;

  /** Size and value typedef support. */
  typedef typename Superclass::SizeType      SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;
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
  static Self Poly(RadiusType radius, unsigned lines);

  /**
   * Returns wether the structuring element is decomposable or not. If the
   * structuring is decomposable, the set of lines associated with the
   * structuring may be used by an algorithm instead of the standard buffer.
   */
  bool GetDecomposable() const
  {
    return m_Decomposable;
  }

  /** Return the lines associated with the structuring element */
  const DecompType & GetLines() const
  {
    return ( m_Lines );
  }

  void PrintSelf(std::ostream & os, Indent indent) const;

protected:

  /**
   * Fill the buffer of the structuring element based on the lines
   * associated to the structuring element
   */
  void ComputeBufferFromLines();

private:
  bool m_Decomposable;

  DecompType m_Lines;

  // dispatch between 2D and 3D
  struct DispatchBase {};
  template< unsigned int VDimension2 >
  struct Dispatch: DispatchBase {};

  virtual FlatStructuringElement PolySub(const Dispatch< 2 > &,
                                         RadiusType radius,
                                         unsigned lines) const;

  virtual FlatStructuringElement PolySub(const Dispatch< 3 > &,
                                         RadiusType radius,
                                         unsigned lines) const;

  virtual FlatStructuringElement PolySub(const DispatchBase &,
                                         RadiusType radius,
                                         unsigned lines) const;

  bool checkParallel(LType NewVec, DecompType Lines);

  template< unsigned int VDimension3 >
  struct StructuringElementFacet {
    LType P1, P2, P3;
  };
  typedef StructuringElementFacet< VDimension > FacetType;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFlatStructuringElement.txx"
#endif

#endif
