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
#include "itkConceptChecking.h"

namespace itk {
/** \class MeanProjectionImageFilter
 * \brief Mean projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * The original paper can be found at 
 *          http://hdl.handle.net/1926/164
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
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

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType InputPixelType; 

  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::PixelType OutputPixelType; 

  /** Runtime information support. */
  itkTypeMacro(MeanProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputPixelToOutputPixelTypeGreaterAdditiveOperatorCheck,
    (Concept::AdditiveOperators<OutputPixelType,
                                InputPixelType,
                                OutputPixelType>));

  itkConceptMacro(InputHasNumericTraitsCheck,
    (Concept::HasNumericTraits<InputPixelType>));
  /** End concept checking */
#endif



protected:
  MeanProjectionImageFilter() {}
  virtual ~MeanProjectionImageFilter() {}

private:
  MeanProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // end MeanProjectionImageFilter

} //end namespace itk

#endif
