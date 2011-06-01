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
#ifndef __itkPeriodicBoundaryCondition_h
#define __itkPeriodicBoundaryCondition_h
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class PeriodicBoundaryCondition
 * \brief
 * A function object that determines values outside of image boundaries
 * according to periodic (wrap-around) conditions.
 *
 * The input to this function object is a neighborhood iterator.  This boundary
 * condition object is designed to be given as a template argument to a
 * NeighborhoodIterator or any of the NeighborhoodIterator subclasses.
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITK-Common
 */
template< class TImage >
class ITK_EXPORT PeriodicBoundaryCondition:
  public ImageBoundaryCondition< TImage >
{
public:
  /** Standard class typedefs. */
  typedef PeriodicBoundaryCondition        Self;
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
  PeriodicBoundaryCondition() {}

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
#define ITK_TEMPLATE_PeriodicBoundaryCondition(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                             \
  {                                                                         \
  _( 1 ( class EXPORT PeriodicBoundaryCondition< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                                       \
  {                                                                         \
  typedef PeriodicBoundaryCondition< ITK_TEMPLATE_1 TypeX >                 \
  PeriodicBoundaryCondition##TypeY;                                       \
  }                                                                         \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkPeriodicBoundaryCondition+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkPeriodicBoundaryCondition.txx"
#endif

#endif
