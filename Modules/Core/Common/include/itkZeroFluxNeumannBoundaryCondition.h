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
#ifndef __itkZeroFluxNeumannBoundaryCondition_h
#define __itkZeroFluxNeumannBoundaryCondition_h
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class ZeroFluxNeumannBoundaryCondition
 * \brief
 * A function object that determines a neighborhood of values at an
 * image boundary according to a Neumann boundary condition where first,
 * upwind derivatives on the boundary are zero.  This is a useful condition
 * in solving some classes of differential equations.
 *
 * For example, invoking this function object on a 7x5 iterator that masks
 * a region at an image corner (iterator is centered on the 2):
 * \code
 *               * * * * * * *
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 * \endcode
 * returns the following neighborhood of values:
 * \code
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               3 3 3 3 5 5 6   (note the corner values)
 *               4 4 4 4 6 7 8
 * \endcode
 * The input to this function object is a neighborhood iterator.  This boundary
 * condition object is designed to be given as a template argument to a
 * NeighborhoodIterator or any of the NeighborhoodIterator
 * subclasses.
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITK-Common
 */
template< class TImage >
class ITK_EXPORT ZeroFluxNeumannBoundaryCondition:
  public ImageBoundaryCondition< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ZeroFluxNeumannBoundaryCondition Self;
  typedef ImageBoundaryCondition< TImage > Superclass;

  /** Extract information from the image type. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  typedef typename Superclass::IndexType        IndexType;
  typedef typename Superclass::OffsetType       OffsetType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  typedef typename Superclass::NeighborhoodAccessorFunctorType
  NeighborhoodAccessorFunctorType;

  /** Extract information from the image type. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Default constructor. */
  ZeroFluxNeumannBoundaryCondition() {}

  /** Computes and returns a neighborhood of appropriate values from
   * neighborhood iterator data.. */
  virtual PixelType operator()(const OffsetType & point_index,
                               const OffsetType & boundary_offset,
                               const NeighborhoodType *data) const;

  /** Computes and returns the appropriate pixel value from
   * neighborhood iterator data, using the functor. */
  virtual PixelType operator()(
    const OffsetType & point_index,
    const OffsetType & boundary_offset,
    const NeighborhoodType *data,
    const NeighborhoodAccessorFunctorType & neighborhoodAccessorFunctor) const;
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_ZeroFluxNeumannBoundaryCondition(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                                    \
  {                                                                                \
  _( 1 ( class EXPORT ZeroFluxNeumannBoundaryCondition< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                                              \
  {                                                                                \
  typedef ZeroFluxNeumannBoundaryCondition< ITK_TEMPLATE_1 TypeX >                 \
  ZeroFluxNeumannBoundaryCondition##TypeY;                                       \
  }                                                                                \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkZeroFluxNeumannBoundaryCondition+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkZeroFluxNeumannBoundaryCondition.txx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroFluxNeumannBoundaryCondition.txx"
#endif
*/

#endif
