/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianOperator_h
#define __itkLaplacianOperator_h

#include "itkExceptionObject.h"
#include "itkNeighborhoodOperator.h"

namespace itk {

/**
 * \class LaplacianOperator
 *
 *  A NeighborhoodOperator for use in calculating the Laplacian at a pixel.
 *  The LaplacianOperator's coefficients are a tightest-fitting convolution
 *  kernel.
 *
 *  For example, the simplest Laplacian Operator for 2D has the form:
 *  \code
 *              0   1   0  
 *              1  -4   1
 *              0   1   0
 *  \endcode
 *              
 *  \par
 *  The LaplacianOperator is a non-directional NeighborhoodOperator that
 *  should be applied to a Neighborhood or NeighborhoodIterator using an inner
 *  product method (itkNeighborhoodInnerProduct).  To initialize the operator, you
 *  need call CreateOperator() before using it.
 *
 *  \par
 *  By default the operator will be created for an isotropic image, but you can
 *  modify the operator to handle different pixel spacings by calling
 *  SetDerivativeScalings.  The argument to SetDerivativeScalings is an array
 *  of doubles that is of length VDimension (the dimensionality of the image).
 *  Make sure to use 1/pixel_spacing to properly scale derivatives.
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \ingroup Operators */
template<class TPixel, unsigned int VDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT LaplacianOperator
  : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /** Standard "Self" typedef support.   */
  typedef LaplacianOperator Self;

  /** Standard "Superclass" typedef.   */
  typedef NeighborhoodOperator<TPixel, VDimension, TAllocator>  Superclass;

  typedef typename Superclass::PixelType PixelType;

 /**  Default constructor  */
  LaplacianOperator()
  {
    for (unsigned i = 0; i < VDimension; ++i)
      {
      m_DerivativeScalings[i] = 1.0;
      }
  }

  /** Copy constructor   */
  LaplacianOperator(const Self& other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other) 
  {
    for (unsigned i = 0; i < VDimension; ++i)
      {
      m_DerivativeScalings[i] = other.m_DerivativeScalings[i];
      }
  }
  
  /** This function is called to create the operator  */ 
  void CreateOperator();  

 
  /** Assignment operator   */
  Self &operator=(const Self& other)
  {
    Superclass::operator=(other);
    return *this;
  }
  
  /** Prints some debugging information   */
  virtual void PrintSelf(std::ostream &os, Indent i) const  
  { 
    os << i << "LaplacianOperator { this=" << this
       << "}" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }

  /** Sets the weights that are applied to the derivative in each axial
   *  direction when the kernel is computed.  These weights are all 1.0 by
   *  default. This method must be called BEFORE CreateOperator */
  void SetDerivativeScalings( const double *s );
           
protected:
  /** Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++.   */
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Calculates operator coefficients.   */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer, default
   * function was NOT used.   */
  void Fill(const CoefficientVector &);

private:
  /** Weights applied to derivatives in each axial direction */
  double m_DerivativeScalings[VDimension];
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianOperator.txx"
#endif

#endif


