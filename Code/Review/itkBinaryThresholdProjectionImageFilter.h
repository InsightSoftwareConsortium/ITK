/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdProjectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdProjectionImageFilter_h
#define __itkBinaryThresholdProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkNumericTraits.h"
#include "itkConceptChecking.h"

namespace itk {
/** \class BinaryThresholdProjectionImageFilter
 * \brief BinaryThreshold projection
 *
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * the original paper can be found at 
 *          http://hdl.handle.net/1926/164
 *
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *    http://hdl.handle.net/1926/164
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 */


namespace Function {
template <class TInputPixel, class TOutputPixel>
class BinaryThresholdAccumulator
{
public:
  BinaryThresholdAccumulator( unsigned long size ) {}
  ~BinaryThresholdAccumulator(){}

  inline void Init()
    {
    m_IsForeground = false;
    }

  inline void operator()( const TInputPixel &input )
    {
    if( input >= m_ThresholdValue )
      {
      m_IsForeground = true; 
      }
    }

  inline TOutputPixel GetValue()
    {
    if( m_IsForeground )
      {
      return (TOutputPixel)m_ForegroundValue; 
      }
    else
      {
      return m_BackgroundValue; 
      }
    }

  bool m_IsForeground;

  TInputPixel                 m_ThresholdValue;
  TInputPixel                 m_ForegroundValue;
  TOutputPixel                m_BackgroundValue;
};
} // end namespace Function


template <class TInputImage, class TOutputImage>
class ITK_EXPORT BinaryThresholdProjectionImageFilter :
    public ProjectionImageFilter<TInputImage, TOutputImage, 
      Function::BinaryThresholdAccumulator< 
        typename TInputImage::PixelType, 
        typename TOutputImage::PixelType > >
{
public:
  typedef BinaryThresholdProjectionImageFilter Self;
  typedef ProjectionImageFilter<TInputImage, TOutputImage, 
    Function::BinaryThresholdAccumulator< 
      typename TInputImage::PixelType, 
      typename TOutputImage::PixelType > > Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(BinaryThresholdProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage           InputImageType;
  typedef TOutputImage          OutputImageType;

  /** Image typedef support. */
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  typedef typename Superclass::AccumulatorType AccumulatorType;

  /** Set/Get the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType .*/
  itkSetMacro(ForegroundValue, InputPixelType);
  itkGetConstMacro(ForegroundValue, InputPixelType);

  /** Set/Get the value used as "background". Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkSetMacro(BackgroundValue, OutputPixelType);
  itkGetConstMacro(BackgroundValue, OutputPixelType);
 
  /** Set/Get the value in the image to consider as "threshold". Defaults to
   *  NumericTraits<InputPixelType>::max() */
  itkSetMacro(ThresholdValue, InputPixelType);
  itkGetConstMacro(ThresholdValue, InputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputPixelTypeGreaterThanComparable,
    (Concept::GreaterThanComparable<InputPixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck,
    (Concept::HasNumericTraits<InputPixelType>));
  /** End concept checking */
#endif



protected:
  BinaryThresholdProjectionImageFilter()
    {
    m_ForegroundValue = NumericTraits<InputPixelType>::max();
    m_BackgroundValue = NumericTraits<OutputPixelType>::NonpositiveMin();
    }
  virtual ~BinaryThresholdProjectionImageFilter() {}

  void PrintSelf(std::ostream& os, Indent indent) const
    {
    Superclass::PrintSelf(os,indent);

    typedef typename NumericTraits<InputPixelType>::PrintType
                                              InputPixelPrintType;

    os << indent << "ForegroundValue: " 
                    << static_cast< InputPixelPrintType > (m_ForegroundValue) 
                    << std::endl;
 
    typedef typename NumericTraits<OutputPixelType>::PrintType
                                              OutputPixelPrintType;

    os << indent << "BackgroundValue: " 
                    << static_cast< OutputPixelPrintType > (m_BackgroundValue) 
                    << std::endl;

    os << indent << "ThresholdValue: " 
                    << static_cast< InputPixelPrintType > (m_ThresholdValue) 
                    << std::endl;
    }


  virtual AccumulatorType NewAccumulator( unsigned long size ) const
    {
    AccumulatorType accumulator( size );
    accumulator.m_ForegroundValue = m_ForegroundValue;
    accumulator.m_BackgroundValue = m_BackgroundValue;
    return accumulator;
    }

  /** Pixel value to dilate */
  InputPixelType m_ForegroundValue;

  /** Pixel value for background */
  OutputPixelType m_BackgroundValue;
  
  /** Pixel value for Threshold */
  OutputPixelType m_ThresholdValue;
  
private:
  BinaryThresholdProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // end BinaryThresholdProjectionImageFilter

} //end namespace itk

#endif
