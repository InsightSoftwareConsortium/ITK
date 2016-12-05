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
#ifndef itkNeighborhoodOperator_h
#define itkNeighborhoodOperator_h

#include "itkNeighborhood.h"
#include "itkNumericTraits.h"
#include <vector>

namespace itk
{
/** \class NeighborhoodOperator
 * \brief Virtual class that defines a common interface to all
 *        neighborhood operator subtypes.
 *
 * A NeighborhoodOperator is a set of pixel values that can be applied to a
 * Neighborhood to perform a user-defined operation (i.e. convolution kernel,
 * morphological structuring element).  A NeighborhoodOperator is itself a
 * specialized Neighborhood, with functionality to generate its coefficients
 * according to user-defined parameters.  Because the operator is a subclass
 * of Neighborhood, it is a valid operand in any of the operations
 * defined on the Neighborhood object (convolution, inner product, etc.).
 *
 * NeighborhoodOperator is a pure virtual object that must be
 * subclassed to be used.  A user's subclass must implement two methods:
 *
 * (1) GenerateCoefficients -- the algorithm that computes the scalar
 *   coefficients of the operator.
 *
 * (2) Fill -- the algorithm that places the scalar coefficients into
 *   the memory buffer of the operator (arranges them spatially in the
 *   neighborhood).
 *
 * NeighborhoodOperator supports the concept of a "directional operator."
 * A directional operator is defined in this context to be an operator
 * that is applied along a single dimension.  Examples of this type of
 * operator are directional derivatives and the individual, directional
 * components of separable processes such as Gaussian smoothing.
 *
 * How a NeighborhoodOperator is applied to data is up to the user who
 * defines it.  One possible use of an operator would be to take its
 * inner product with a neighborhood of values to produce
 * a scalar result.  This process effects convolution when applied to
 * successive neighborhoods across a region of interest in an image.
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 */
template< typename TPixel, unsigned int VDimension,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT NeighborhoodOperator:
  public Neighborhood< TPixel, VDimension, TAllocator >
{
public:
  /**  Standard class typedefs. */
  typedef NeighborhoodOperator                           Self;
  typedef Neighborhood< TPixel, VDimension, TAllocator > Superclass;

  itkTypeMacro(NeighborhoodOperator, NeighborhoodOperator);

  /** Size object typedef support */
  typedef typename Superclass::SizeType      SizeType;

  /** External support for pixel type */
  typedef TPixel PixelType;

  /** Slice iterator typedef support */
  typedef SliceIterator< TPixel, Self > SliceIteratorType;

  /** Constructor. */
  NeighborhoodOperator()
  {  m_Direction = 0;  }

  /** Copy constructor */
  NeighborhoodOperator(const Self & orig):
    Neighborhood< TPixel, VDimension, TAllocator >(orig)
  {   m_Direction = orig.m_Direction;   }

  /** Assignment operator. */
  Self & operator=(const Self & orig)
  {
    Superclass::operator=(orig);
    m_Direction = orig.m_Direction;
    return *this;
  }

  /** Sets the dimensional direction of a directional operator. */
  void SetDirection(const unsigned long & direction)
  {  m_Direction = direction;   }

  /** Returns the direction (dimension number) of a directional operator. */
  unsigned long GetDirection() const
  {  return m_Direction;  }

  /** Creates the operator with length only in the specified direction.
   * The radius of the operator will be 0 except along the axis on which
   * the operator will work.
   * \sa CreateToRadius \sa FillCenteredDirectional \sa SetDirection() \sa GetDirection() */
  virtual void CreateDirectional();

  /** Creates the operator with a specified radius.  The spatial location of
   * the coefficients within the operator is defined by the subclass
   * implementation of the Fill method.
   * \sa CreateDirectional \sa Fill */
  virtual void CreateToRadius(const SizeType &);

  /** Creates the operator with a specified radius ("square", same length
   * on each side). The spatial location of the coefficients within the
   * operator is defined by the subclass implementation of the Fill method.
   * \sa CreateDirectional \sa Fill */
  virtual void CreateToRadius(const SizeValueType);

  /** Reverses the direction of all axes of the operator by reversing the order
    * of the coefficients. */
  virtual void FlipAxes();

  /** Prints some debugging information. */
  virtual void PrintSelf(std::ostream & os, Indent i) const ITK_OVERRIDE
  {
    os << i << "NeighborhoodOperator { this=" << this
       << " Direction = " << m_Direction << " }" << std::endl;
    Superclass::PrintSelf( os, i.GetNextIndent() );
  }

  typedef typename NumericTraits< TPixel >::RealType PixelRealType;

  /** Multiplies all of the coefficients of the kernel by a single scalar value.
    */
  void ScaleCoefficients(PixelRealType);

protected:
  /** Typedef support  for coefficient vector type.  Necessary
   * to fix bug in the microsoft VC++ compiler. */
  typedef std::vector< PixelRealType > CoefficientVector;

  /** A subclass-specific algorithm that computes the coefficients
   * of the operator. */
  virtual CoefficientVector GenerateCoefficients() = 0;

  /** A subclass-specific algorithm that positions the coefficients
   * spatially in the operator. */
  virtual void Fill(const CoefficientVector &) = 0;

  /** A pre-defined Fill function that can be called by a subclass
   * Fill function to center coefficients along the axis specified
   * by the SetDirection method.  Useful for creating directional
   * operators, or centering coefficients in an N-dimensional
   * neighborhood. */
  virtual void FillCenteredDirectional(const CoefficientVector &);

  /** Initializes all the coefficients in the neighborhood to zero values */
  void InitializeToZero()
  {
    for ( unsigned int i = 0; i < this->Size(); ++i )
      {
      this->operator[](i) = NumericTraits< PixelType >::ZeroValue();
      }
  }

private:
  /** Direction (dimension number) of the derivative. */
  unsigned long m_Direction;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperator.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperator.hxx"
#endif
*/
#endif
