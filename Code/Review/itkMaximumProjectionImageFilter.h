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
#include "itkConceptChecking.h"

namespace itk {
/** \class MaximumProjectionImageFilter
 * \brief Maximum projection
 *
 * this class was contributed to the insight journal by Gaetan Lehmann.
 * the original paper can be found at 
 *          http://hdl.handle.net/1926/164
 *
 * \author Gaetan Lehmann. Biologie du Développement et de la reproduction,
 *  inra de jouy-en-josas, France.
 *  
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
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

  inline void Initialize()
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

  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType InputPixelType; 

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MaximumProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputPixelTypeGreaterThanComparable,
    (Concept::GreaterThanComparable<InputPixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck,
    (Concept::HasNumericTraits<InputPixelType>));
  /** End concept checking */
#endif


protected:
  MaximumProjectionImageFilter() {}
  virtual ~MaximumProjectionImageFilter() {}

private:
  MaximumProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


}; // end MaximumProjectionImageFilter

} //end namespace itk

#endif
