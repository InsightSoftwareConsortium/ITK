/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdSegmentationLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkThresholdSegmentationLevelSetFunction_h_
#define __itkThresholdSegmentationLevelSetFunction_h_

#include "itkSegmentationLevelSetFunction.h"

namespace itk {


/**
 *
 *
 * Assumes a strictly POSITIVE feature image
 */
template <class TImageType, class TFeatureImageType = TImageType>
class ITK_EXPORT ThresholdSegmentationLevelSetFunction
  : public SegmentationLevelSetFunction<TImageType, TFeatureImageType>
{
public:
  /** Standard class typedefs. */
  typedef ThresholdSegmentationLevelSetFunction Self;
  typedef SegmentationLevelSetFunction<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ThresholdSegmentationLevelSetFunction, SegmentationLevelSetFunction );

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Set/Get threshold values */
  void SetUpperThreshold(FeatureScalarType f)
  { m_UpperThreshold = f; }
  FeatureScalarType GetUpperThreshold() const
  { return m_UpperThreshold; }
  void SetLowerThreshold(FeatureScalarType f)
  { m_LowerThreshold = f; }
  FeatureScalarType GetLowerThreshold() const
  { return m_LowerThreshold; }

  virtual void CalculateSpeedImage();
  
protected:

  FeatureScalarType m_UpperThreshold;
  FeatureScalarType m_LowerThreshold;
  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdSegmentationLevelSetFunction.txx"
#endif

#endif
