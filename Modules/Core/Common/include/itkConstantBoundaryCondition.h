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
#ifndef __itkConstantBoundaryCondition_h
#define __itkConstantBoundaryCondition_h
#include "itkNumericTraits.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class ConstantBoundaryCondition
 * \brief This boundary condition returns a constant value for out-of-bounds
 * image pixels.
 *
 * For example, invoking this function object with a constant value of zero
 * (the default) on each out-of-bounds element of a 7x5 iterator that masks a
 * region at an image corner
 * (iterator is centered on the 2):
 *
 *               * * * * * * *
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 *
 * would produce the following neighborhood of values:
 *
 *               0 0 0 0 0 0 0
 *               0 0 0 0 0 0 0
 *               0 0 1 2 3 4 5
 *               0 0 3 3 5 5 6
 *               0 0 4 4 6 7 8
 *
 *
 * \note If you are using an image with Array as the pixel type, you will need
 * to set the constant explicitly with an array of the appropriate length. This
 * is also true if your image type is a VectorImage.
 *
 * \sa ImageBoundaryCondition
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITK-Common
 * \wikiexample{Iterators/ConstantBoundaryCondition,Make out of bounds pixels return a constant value}
 */
template< class TImage >
class ITK_EXPORT ConstantBoundaryCondition:
  public ImageBoundaryCondition< TImage >
{
public:
  /** Self & superclass typedefs */
  typedef ConstantBoundaryCondition        Self;
  typedef ImageBoundaryCondition< TImage > Superclass;

  /** Extract information from the image type */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  typedef typename Superclass::IndexType        IndexType;
  typedef typename Superclass::OffsetType       OffsetType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  typedef typename Superclass::NeighborhoodAccessorFunctorType
  NeighborhoodAccessorFunctorType;

  /** Save the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Default constructor. */
  ConstantBoundaryCondition()
  { m_Constant = NumericTraits< PixelType >::Zero; }

  /** Computes and returns appropriate out-of-bounds values from
   * neighborhood iterator data. */
  virtual PixelType operator()(const OffsetType &,
                               const OffsetType &,
                               const NeighborhoodType *) const
  { return m_Constant; }

  /** Computes and returns the appropriate pixel value from
   * neighborhood iterator data, using the functor. */
  virtual PixelType operator()(
    const OffsetType &,
    const OffsetType &,
    const NeighborhoodType *,
    const NeighborhoodAccessorFunctorType &) const
  { return m_Constant; }

  /** Set the value of the constant. */
  void SetConstant(const PixelType & c)
  {  m_Constant = c; }

  /** Get the value of the constant. */
  const PixelType & GetConstant() const
  {  return m_Constant;  }

  /** Tell if the boundary condition can index to any location within
    * the associated iterator's neighborhood or if it has some limited
    * subset (such as none) that it relies upon. */
  bool RequiresCompleteNeighborhood() { return false; }
private:
  PixelType m_Constant;
};
} // end namespace itk

#endif
