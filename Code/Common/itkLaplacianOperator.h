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
 *  product method (itkNeighborhoodInnerProduct).  To create the operator, you
 *  need: call CreateOperator().
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
  LaplacianOperator() {}

  /** Copy constructor   */
  LaplacianOperator(const Self& other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other) 
  {  }
  
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

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianOperator.txx"
#endif

#endif


