/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumProjectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaximumProjectionImageFilter_h
#define __itkMaximumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class MaximumProjectionImageFilter
 * \brief Maximum projection
 *
 * This class was contributed to the Insight Journal by
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *  http://hdl.handle.net/1926/164
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa SigmaProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 */


namespace Function {
template <class TInputPixel>
class MaximumAccumulator
{
public:
  MaximumAccumulator( unsigned long size ) {}
  ~MaximumAccumulator(){}

  inline void Init()
    {
    m_Maximum = NumericTraits< TInputPixel >::NonpositiveMin();
    }

  inline void operator()( const TInputPixel &input )
    {
    m_Maximum = vnl_math_max( m_Maximum, input );
    }

  inline TInputPixel GetValue()
    {
    return m_Maximum;
    }

  TInputPixel m_Maximum;
};
} // end namespace Function


template <class TInputImage, class TOutputImage>
class ITK_EXPORT MaximumProjectionImageFilter :
public ProjectionImageFilter<TInputImage, TOutputImage, 
    Function::MaximumAccumulator< typename TInputImage::PixelType > >
{
public:
  typedef MaximumProjectionImageFilter Self;
  typedef ProjectionImageFilter<TInputImage, TOutputImage, 
  Function::MaximumAccumulator< typename TInputImage::PixelType > > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MaximumProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  MaximumProjectionImageFilter() {}
  virtual ~MaximumProjectionImageFilter() {}

private:
  MaximumProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end MaximumProjectionImageFilter

} //end namespace itk

#endif
