/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkDerivativeOperator_h
#define __itkDerivativeOperator_h

#include "itkExceptionObject.h"
#include "itkNeighborhoodOperator.h"

namespace itk {

/**
 * \class DerivativeOperator
 * \brief A NeighborhoodOperator for taking an n-th order derivative
 * at a pixel
 * 
 * DerivativeOperator's coefficients are a tightest-fitting convolution
 * kernel for calculating the n-th order directional derivative at a pixel.
 * DerivativeOperator is a directional NeighborhoodOperator that should be
 * applied to a Neighborhood or NeighborhoodPointer using the inner product
 * method.
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \sa ForwardDifferenceOperator
 * \sa BackwardDifferenceOperator
 */
template<class TPixel, unsigned int VDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT DerivativeOperator
  : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{

public:
  /**
   * Standard "Self" typedef support.
   */
  typedef DerivativeOperator Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef NeighborhoodOperator<TPixel, VDimension, TAllocator>  Superclass;

  /**
   * Constructor
   */
  DerivativeOperator() : m_Order(1) {}

  /**
   * Copy constructor
   */
  DerivativeOperator(const Self& other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other)
  { m_Order = other.m_Order;  }
  
  /**
   * Assignment operator
   */
  Self &operator=(const Self& other)
  {
    Superclass::operator=(other);
    m_Order = other.m_Order;
    return *this;
  }

  /**
   * Sets the order of the derivative.
   */
  void SetOrder(const unsigned int &order)
  {  m_Order = order;  }

  /**
   * Returns the order of the derivative.
   */
  unsigned int GetOrder() const { return m_Order; }

  /**
   * Prints some debugging information
   */
  virtual void PrintSelf(std::ostream &os, Indent i) const  
  { 
    os << i << "DerivativeOperator { this=" << this
       << ", m_Order = " << m_Order << "}" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }
  
protected:
  /**
   * Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++.
   */
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /**
   * Calculates operator coefficients.
   */
  CoefficientVector GenerateCoefficients();

  /**
   * Arranges coefficients spatially in the memory buffer.
   */
  void Fill(const CoefficientVector &coeff)
  {   Superclass::FillCenteredDirectional(coeff);  }
 
private:
  /**
   * Order of the derivative.
   */
  unsigned int m_Order;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDerivativeOperator.txx"
#endif

#endif


