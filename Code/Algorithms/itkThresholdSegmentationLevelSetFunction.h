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
#include "itkNumericTraits.h"
namespace itk {

/** \class ThresholdSegmentationLevelSetFunction
 *    
 * \brief This function is used in ThresholdSegmentationLevelSetImageFilter to
 * segment structures in images based on intensity values.
 *
 * \par  SegmentationLevelSetFunction is a subclass of the generic LevelSetFunction.
 * It useful for segmentations based on intensity values in an image.  It works
 * by constructing a speed term (feature image) with positive values inside an
 * intensity window (between a low and high threshold) and negative values
 * outside that intensity window.  The evolving level set front will lock onto
 * regions that are at the edges of the intensity window.
 *
 *  \par
 *  Image $f$ is thresholded pixel by pixel using upper threshold
 *  $U$ and lower threshold $L$ according to the following formula.
 *
 * \par
 *  \f$  f(x) = \left\{ \begin{array}{ll} g(x) - L & \mbox{if $(g)x < (U-L)/2 + L$} \\ U - g(x) & \mbox{otherwise} \end{array} \right. \f$ 
 *
 * \sa SegmentationLevelSetImageFunction
 *  \sa ThresholdSegmentationLevelSetImageFilter */
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
  typedef TFeatureImageType FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ThresholdSegmentationLevelSetFunction, SegmentationLevelSetFunction );

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;
  typedef typename Superclass::RadiusType RadiusType;

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

  virtual void Initialize(const RadiusType &r)
  {
    Superclass::Initialize(r);
    
    this->SetAdvectionWeight( NumericTraits<ScalarValueType>::Zero);
    this->SetPropagationWeight(-1.0 * NumericTraits<ScalarValueType>::One);
    this->SetCurvatureWeight(NumericTraits<ScalarValueType>::One);
  }
  
protected:
  ThresholdSegmentationLevelSetFunction()
  {
    m_UpperThreshold = NumericTraits<FeatureScalarType>::max();
    m_LowerThreshold = NumericTraits<FeatureScalarType>::NonpositiveMin();
    this->SetAdvectionWeight(0.0);
    this->SetPropagationWeight(1.0);
    this->SetCurvatureWeight(1.0);
  }
  virtual ~ThresholdSegmentationLevelSetFunction(){}

  ThresholdSegmentationLevelSetFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent );
    os << indent << "UpperThreshold: " << m_UpperThreshold << std::endl;
    os << indent << "LowerThreshold: " << m_LowerThreshold << std::endl;
  }
  
  FeatureScalarType m_UpperThreshold;
  FeatureScalarType m_LowerThreshold;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdSegmentationLevelSetFunction.txx"
#endif

#endif
