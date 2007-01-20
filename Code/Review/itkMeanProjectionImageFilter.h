/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanProjectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeanProjectionImageFilter_h
#define __itkMeanProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class MeanProjectionImageFilter
 * \brief Mean projection
 *
 * This class was contributed to the Insight Journal by
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *      http://hdl.handle.net/1926/164
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa SigmaProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 */


namespace Function {
template <class TInputPixel, class TAccumulate>
class MeanAccumulator
{
public:
  typedef typename NumericTraits<TInputPixel>::RealType RealType;

  MeanAccumulator( unsigned long size )
    {
    m_Size = size;
    }
  ~MeanAccumulator(){}

  inline void Init()
    {
    m_Sum = NumericTraits< TAccumulate >::Zero;
    }

  inline void operator()( const TInputPixel &input )
    {
    m_Sum = m_Sum + input;
    }

  inline RealType GetValue()
    {
    return ((RealType) m_Sum) / m_Size;
    }

  TAccumulate m_Sum;
  unsigned long m_Size;
};
} // end namespace Function


template <class TInputImage, class TOutputImage, 
class TAccumulate= 
  ITK_TYPENAME NumericTraits<
    ITK_TYPENAME TOutputImage::PixelType >::AccumulateType >
class ITK_EXPORT MeanProjectionImageFilter : public
  ProjectionImageFilter<TInputImage, TOutputImage,
    Function::MeanAccumulator< typename TInputImage::PixelType, TAccumulate > >
{
public:
  typedef MeanProjectionImageFilter Self;
  typedef ProjectionImageFilter<TInputImage, TOutputImage, 
    Function::MeanAccumulator< 
      typename TInputImage::PixelType, TAccumulate > > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MeanProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  MeanProjectionImageFilter() {}
  virtual ~MeanProjectionImageFilter() {}

private:
  MeanProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end MeanProjectionImageFilter

} //end namespace itk

#endif
