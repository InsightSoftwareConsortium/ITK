/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageKernelOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageKernelOperator_h
#define __itkImageKernelOperator_h

#include "itkNeighborhoodOperator.h"

#include "itkImage.h"

namespace itk
{
/**
 * \class ImageKernelOperator
 * \brief A NeighborhoodOperator whose coefficients are from an image.
 *
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 *
 * \ingroup Operators
 */
template< class TPixel, unsigned int VDimension = 2,
          class TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_EXPORT ImageKernelOperator:
  public NeighborhoodOperator< TPixel, VDimension, TAllocator >
{
public:
  /** Standard class typedefs. */
  typedef ImageKernelOperator                                    Self;
  typedef NeighborhoodOperator< TPixel, VDimension, TAllocator > Superclass;

  typedef Image< TPixel, VDimension >            ImageType;
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Constructor. */
  ImageKernelOperator() {}

  /** Copy constructor */
  ImageKernelOperator(const Self & orig):
    Neighborhood< TPixel, VDimension, TAllocator >(orig)
  {}

  /** Assignment operator. */
  Self & operator=(const Self & orig)
  {
    Superclass::operator=(orig);
    return *this;
  }

  void SetImageKernel(ImageType *kernel)
  {
    this->m_ImageKernel = kernel;
  }

  /** Prints some debugging information. */
  virtual void PrintSelf(std::ostream & os, Indent i) const
  {
    os << i << "ImageKernelOperator { this=" << this
       << "} "  << std::endl;
    Superclass::PrintSelf( os, i.GetNextIndent() );
  }

protected:

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector & coeff);

private:

  typename ImageType::Pointer m_ImageKernel;

  /** For compatibility with itkWarningMacro */
  const char * GetNameOfClass()
  { return "itkImageKernelOperator"; }
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageKernelOperator.txx"
#endif

#endif
