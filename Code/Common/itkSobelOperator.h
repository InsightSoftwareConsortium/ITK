/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSobelOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSobelOperator_h
#define __itkSobelOperator_h

#include "itkExceptionObject.h"
#include "itkNeighborhoodOperator.h"

namespace itk {

/**
 * \class SobelOperator
 * \brief A NeighborhoodOperator for doing Sobel operation
 * at a pixel
 * 
 * SobelOperator's coefficients are a tightest-fitting convolution
 * kernel for calculating the laplacian value at a pixel.
 * SobelOperator is a directional NeighborhoodOperator that should be
 * applied to a Neighborhood or NeighborhoodPointer using the inner product
 * method. To create the operator, you need:
 * 
 * 1) Set the direction  2) call CreateOperator() 
 * For example, the Sobel Operator in vertical direction is
 *             -1  -2  -1  
 *             0    0   0 
 *             1    2   1
 * while  the Sobel Operator in horizonal direction is
 *             -1   0   1      
 *             -2   0   2 
 *             -1   0   1
 *
 * The Sobel Operator in the Nth dimension can be calculated accordingly.
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \sa ForwardDifferenceOperator
 * \sa BackwardDifferenceOperator
 * 
 * \ingroup Operators
 */
template<class TPixel, unsigned int VDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT SobelOperator
  : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{

public:
  /**
   * Standard "Self" typedef support.
   */
  typedef SobelOperator Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef NeighborhoodOperator<TPixel, VDimension, TAllocator>  Superclass;


  /**
   * Default constructor
   */

  SobelOperator() {}

  /**
   * Copy constructor
   */
  
  SobelOperator(const Self& other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other) 
  {  }
  

  /** 
   * This function is called to create the operator
   */ 
  void CreateOperator();   
 
  /**
   * Assignment operator
   */
  Self &operator=(const Self& other)
  {
    Superclass::operator=(other);
    return *this;
  }
  /**
   * Prints some debugging information
   */
  virtual void PrintSelf(std::ostream &os, Indent i) const  
  { 
    os << i << "SobelOperator { this=" << this
       << "}" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }
  
protected:
  /**
   * Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++.
   */
  typedef typename Superclass::CoefficientVector CoefficientVector;
  typedef typename Superclass::PixelType PixelType;

  /**
   * Calculates operator coefficients.
   */
  CoefficientVector GenerateCoefficients();

  /**
   * Arranges coefficients spatially in the memory buffer.
   */
  void Fill(const CoefficientVector &);
//{   Superclass::FillCenteredDirectional(coeff);  }



 
private:

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSobelOperator.txx"
#endif

#endif


