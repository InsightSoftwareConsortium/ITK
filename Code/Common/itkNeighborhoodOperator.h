/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkNeighborhoodOperator_h
#define __itkNeighborhoodOperator_h

#include "itkNeighborhood.h"
#include "itkExceptionObject.h"

namespace itk {
/**
 * \class NeighborhoodOperator
 * \brief Virtual class that defines a common interface to all
 *        neighborhood operator subtypes.
 *
 * A NeighborhoodOperator is a set of values that can be applied to a
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
 * (1) GenerateCoefficients -- the algorithm that computes the coefficients
 *  of the operator.
 *
 * (2) Fill -- the algorithm that places the coefficients into the memory
 *  buffer of the operator (arranges them spatially in the neighborhood).
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
 */
template< class TDataType, unsigned int VDimension >
class NeighborhoodOperator : public Neighborhood<TDataType, VDimension>
{
public:
  /**
   *  Neighborhood typedef support.
   */
  typedef Neighborhood<TDataType, VDimension> Neighborhood;

  /**
   *  Standard "Self" typedef.
   */ 
  typedef NeighborhoodOperator Self;

  /**
   * Constructor.
   */
  NeighborhoodOperator() : m_Direction(0) {}
  
  /**
   * Sets the dimensional direction of a directional operator.
   */
  void SetDirection(const unsigned long &direction)
  {
    m_Direction = direction;
  }

  /**
   * Returns the direction (dimension number) of a directional operator.
   */
  unsigned long GetDirection() const
  {
    return m_Direction;
  }
  
  /**
   * Creates the operator with length only in the specified direction.
   * The radius of the operator will be 0 except along the axis on which
   * the operator will work.
   * \sa CreateToRadius
   * \sa FillCenteredDirectional
   * \sa SetDirection()
   * \sa GetDirection()
   */
  virtual void CreateDirectional();

  /**
   * Creates the operator with a specified radius.  The spatial location of
   * the coefficients within the operator is defined by the subclass
   * implementation of the Fill method.
   * \sa CreateDirectional
   * \sa Fill
   */
  virtual void CreateToRadius(const unsigned long *);

  /**
   * Creates the operator with a specified radius ("square", same length
   * on each side). The spatial location of the coefficients within the
   * operator is defined by the subclass implementation of the Fill method.
   * \sa CreateDirectional
   * \sa Fill
   */
  virtual void CreateToRadius(const unsigned long &);

  /**
   * Prints some debugging information.
   */
  void Print()
  {
    Neighborhood::Print(); 
    std::cout << "NeighborhoodOperator" << std::endl;
    std::cout << "\t Direction = " << m_Direction << std::endl;
  }

protected:
  /**
   * A subclass-specific algorithm that computes the coefficients
   * of the operator.
   */
  virtual std::vector<TDataType> GenerateCoefficients() = 0;

  /**
   * A subclass-specific algorithm that positions the coefficients
   * spatially in the operator.
   */
  virtual void Fill(const std::vector<TDataType> &) = 0;
  
  /**
   * A pre-defined Fill function that can be called by a subclass
   * Fill function to center coefficients along the axis specified
   * by the SetDirection method.  Useful for creating directional
   * operators, or centering coefficients in an N-dimensional
   * neighborhood.
   */
  virtual void FillCenteredDirectional(const std::vector<TDataType> &);
  
private:
  /**
   * Direction (dimension number) of the derivative.
   */
  unsigned long  m_Direction;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperator.txx"
#endif

#endif

