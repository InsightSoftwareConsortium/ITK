/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannySegmentationLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCannySegmentationLevelSetFunction_h_
#define __itkCannySegmentationLevelSetFunction_h_

#include "itkSegmentationLevelSetFunction.h"

namespace itk {

/**
 *
 *
 * 
 */
template <class TImageType, class TFeatureImageType = TImageType>
class ITK_EXPORT CannySegmentationLevelSetFunction
  : public SegmentationLevelSetFunction<TImageType, TFeatureImageType>
{
public:
  /** Standard class typedefs. */
  typedef CannySegmentationLevelSetFunction Self;
  typedef SegmentationLevelSetFunction<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  typedef TFeatureImageType FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( CannySegmentationLevelSetFunction, SegmentationLevelSetFunction );

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;
  typedef typename Superclass::RadiusType RadiusType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** */
  void SetThreshold(ScalarValueType v)
  { m_Threshold = v; }
  ScalarValueType GetThreshold() const
  { return m_Threshold; }

  /** */
  void SetVariance(double v)
  { m_Variance = v; }
  double GetVariance() const
  { return m_Variance; }

  /** This method also fills in the Speed image, everything is done in one step
      so that CalculateSpeedImage does not need to be implemented.*/
  virtual void CalculateAdvectionImage();

  virtual void Initialize(const RadiusType &r)
  {
    Superclass::Initialize(r);
    
    this->SetAdvectionWeight(-1.0 * NumericTraits<ScalarValueType>::One);
    this->SetPropagationWeight(-1.0 * NumericTraits<ScalarValueType>::One);
    this->SetCurvatureWeight(NumericTraits<ScalarValueType>::One);
  }

protected:
  CannySegmentationLevelSetFunction()
  {
    m_Variance = 0.0;
    m_Threshold = NumericTraits<ScalarValueType>::Zero;
  }
  virtual ~CannySegmentationLevelSetFunction() {}

  CannySegmentationLevelSetFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent );
  }

private:
  ScalarValueType m_Variance;
  double m_Threshold;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCannySegmentationLevelSetFunction.txx"
#endif

#endif
