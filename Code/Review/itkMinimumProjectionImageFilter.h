/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumProjectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMinimumProjectionImageFilter_h
#define __itkMinimumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class MinimumProjectionImageFilter
 * \brief Minimum projection
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 */


namespace Function {
template <class TInputPixel>
class MinimumAccumulator
{
public:
  MinimumAccumulator( unsigned long size ) {}
  ~MinimumAccumulator(){}

  inline void Init()
    {
    m_Minimum = NumericTraits< TInputPixel >::max();
    }

  inline void operator()( const TInputPixel &input )
    {
    m_Minimum = vnl_math_min( m_Minimum, input );
    }

  inline TInputPixel GetValue()
    {
    return m_Minimum;
    }

  TInputPixel m_Minimum;
};
} // end namespace Function


template <class TInputImage, class TOutputImage>
class ITK_EXPORT MinimumProjectionImageFilter :
    public
    ProjectionImageFilter<TInputImage, TOutputImage, Function::MinimumAccumulator< typename TInputImage::PixelType > >
{
public:
  typedef MinimumProjectionImageFilter Self;
  typedef ProjectionImageFilter<TInputImage, TOutputImage, Function::MinimumAccumulator< typename TInputImage::PixelType > > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MinimumProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  MinimumProjectionImageFilter() {}
  virtual ~MinimumProjectionImageFilter() {}

private:
  MinimumProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end MinimumProjectionImageFilter

} //end namespace itk
#endif
