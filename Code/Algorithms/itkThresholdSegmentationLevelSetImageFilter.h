/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdSegmentationLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkThresholdSegmentationLevelSetImageFilter_h_
#define __itkThresholdSegmentationLevelSetImageFilter_h_

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkThresholdSegmentationLevelSetFunction.h"

namespace itk {

template <class TInputImage, class TOutputImage>
class ThresholdSegmentationLevelSetImageFilter
  : public SegmentationLevelSetImageFilter<TInputImage, TOutputImage>
{
public:
   /** Standard class typedefs */
  typedef ThresholdSegmentationLevelSetImageFilter Self;
  typedef SegmentationLevelSetImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;

  /** Type of the segmentation function */
  typedef ThresholdSegmentationLevelSetFunction<TOutputImage> ThresholdFunctionType;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdSegmenationLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);
  
  /** Get/Set the threshold values that will be used to calculate the speed function. */
  void SetUpperThreshold(ValueType v)
  {
    m_ThresholdFunction->SetUpperThreshold(v);
    this->Modified();
  }
  void SetLowerThreshold(ValueType v)
  {
    m_ThresholdFunction->SetLowerThreshold(v);
    this->Modified();
  }
  ValueType GetUpperThreshold() const
  {
    m_ThresholdFunction->GetUpperThreshold();
  }
  ValueType GetLowerThreshold() const
  {
    m_ThresholdFunction->GetLowerThreshold();
  }
  
protected:
  ~ThresholdSegmentationLevelSetImageFilter() {}
  ThresholdSegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream os, Indent indent) const; 

  
private:
  typename ThresholdFunctionType::Pointer m_ThresholdFunction;  
};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdSegmentationLevelSetImageFilter.txx"
#endif

#endif
