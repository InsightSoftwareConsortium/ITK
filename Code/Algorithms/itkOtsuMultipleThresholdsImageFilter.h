/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOtsuMultipleThresholdsImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOtsuMultipleThresholdsImageFilter_h
#define __itkOtsuMultipleThresholdsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"
#include "itkOtsuMultipleThresholdsCalculator.h"
#include "itkScalarImageToHistogramGenerator.h"

namespace itk {

/** \class OtsuMultipleThresholdsImageFilter 
 * \brief Threshold an image using multiple Otsu Thresholds.
 *
 * This filter creates a labeled image that separates the input
 * image into various classes. The filter
 * computes the thresholds using the OtsuMultipleThresholdsCalculator and
 * applies those thesholds to the input image using the
 * ThresholdLabelerImageFilter. The NumberOfHistogramBins and
 * NumberOfThresholds can be set
 * for the Calculator. The LabelOffset can be set
 * for the ThresholdLabelerImageFilter.
 *
 * \sa ScalarImageToHistogramGenerator
 * \sa OtsuMultipleThresholdsCalculator
 * \sa ThresholdLabelerImageFilter
 * \ingroup IntensityImageFilters  Multithreaded
 */

template<class TInputImage, class TOutputImage>
class ITK_EXPORT OtsuMultipleThresholdsImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard Self typedef */
  typedef OtsuMultipleThresholdsImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(OtsuMultipleThresholdsImageFilter, ImageToImageFilter);
  
  /** Image pixel value typedef. */
  typedef typename TInputImage::PixelType   InputPixelType;
  typedef typename TOutputImage::PixelType   OutputPixelType;
  
  /** Image related typedefs. */
  typedef typename TInputImage::Pointer InputImagePointer;
  typedef typename TOutputImage::Pointer OutputImagePointer;

  typedef typename TInputImage::SizeType  InputSizeType;
  typedef typename TInputImage::IndexType  InputIndexType;
  typedef typename TInputImage::RegionType InputImageRegionType;
  typedef typename TOutputImage::SizeType  OutputSizeType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Threshold vector types. */
  typedef itk::Statistics::ScalarImageToHistogramGenerator< 
                                           TInputImage > HistogramGeneratorType;
  typedef typename HistogramGeneratorType::HistogramType HistogramType;
  typedef OtsuMultipleThresholdsCalculator< HistogramType > OtsuCalculatorType;
  typedef typename OtsuCalculatorType::OutputType  ThresholdVectorType;
  
  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension ) ;
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension ) ;

  /** Set/Get the number of histogram bins. Default is 128. */
  itkSetClampMacro( NumberOfHistogramBins, unsigned long, 1, NumericTraits<unsigned long>::max() );
  itkGetConstMacro( NumberOfHistogramBins, unsigned long );

  /** Set/Get the number of thresholds. Default is 1. */
  itkSetClampMacro(NumberOfThresholds, unsigned long, 1, NumericTraits<unsigned long>::max() );
  itkGetConstMacro(NumberOfThresholds,unsigned long);

  /** Set/Get the offset which labels have to start from. Default is 0. */
  itkSetClampMacro(LabelOffset,OutputPixelType, NumericTraits<OutputPixelType>::Zero,NumericTraits<OutputPixelType>::max() );
  itkGetConstMacro(LabelOffset,OutputPixelType);

  /** Get the computed threshold. */
  const ThresholdVectorType & GetThresholds() const
    {
    return m_Thresholds;
    }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(OutputComparableCheck,
    (Concept::Comparable<OutputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck,
    (Concept::OStreamWritable<OutputPixelType>));
  /** End concept checking */
#endif

protected:
  OtsuMultipleThresholdsImageFilter();
  ~OtsuMultipleThresholdsImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateInputRequestedRegion();
  void GenerateData ();

private:
  OtsuMultipleThresholdsImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned long           m_NumberOfHistogramBins;
  unsigned long           m_NumberOfThresholds;
  OutputPixelType         m_LabelOffset;
  ThresholdVectorType     m_Thresholds;


} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOtsuMultipleThresholdsImageFilter.txx"
#endif

#endif
