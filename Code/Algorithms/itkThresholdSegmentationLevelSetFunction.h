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
 *  You may optionally add a Laplacian calculation on the image to the
 *  threshold-based speed term by setting the EdgeWeight parameter to a
 *  non-zero value.  The Laplacian term will cause the evolving surface to
 *  be more strongly attracted to image edges.   Several parameters control a
 *  preprocessing FeatureImage smoothing stage applied only to the Laplacian
 *  calculation.
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
  typedef SegmentationLevelSetFunction<TImageType, TFeatureImageType> Superclass;
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

  /** Set/Get the weight applied to the edge (Laplacian) attractor in the speed
   *  term function. Zero will turn this term off. */
  void SetEdgeWeight(const ScalarValueType p)
  {
    m_EdgeWeight = p;
  }
  ScalarValueType GetEdgeWeight() const
  {
    return m_EdgeWeight;
  }
  
  /** Anisotropic diffusion is applied to the FeatureImage before calculatign
   * the Laplacian (edge) term. This method sets/gets the smoothing
   * conductance. */
  void SetSmoothingConductance(const ScalarValueType p)
  {
    m_SmoothingConductance = p;
  }
  ScalarValueType GetSmoothingConductance() const
  {
    return m_SmoothingConductance;
  }
  
  /** Anisotropic diffusion is applied to the FeatureImage before calculating
   * the Laplacian (edge) term. This method sets/gets the number of diffusion
   * iterations. */ 
  void SetSmoothingIterations(const int p)
  {
    m_SmoothingIterations = p;
  }
  int GetSmoothingIterations() const
  {
    return m_SmoothingIterations;
  }

  /** Anisotropic diffusion is applied to the FeatureImage before calculating
   * the Laplacian (edge) term. This method sets/gets the diffusion time
   * step. */
  void SetSmoothingTimeStep(const ScalarValueType i)
  {
    m_SmoothingTimeStep = i;
  }
  ScalarValueType GetSmoothingTimeStep() const
  {
    return m_SmoothingTimeStep;
  }

protected:
  ThresholdSegmentationLevelSetFunction()
  {
    m_UpperThreshold = NumericTraits<FeatureScalarType>::max();
    m_LowerThreshold = NumericTraits<FeatureScalarType>::NonpositiveMin();
    this->SetAdvectionWeight(0.0);
    this->SetPropagationWeight(1.0);
    this->SetCurvatureWeight(1.0);
    this->SetSmoothingIterations(5);
    this->SetSmoothingConductance(0.8);
    this->SetSmoothingTimeStep(0.1);
    this->SetEdgeWeight(0.0);
  }
  virtual ~ThresholdSegmentationLevelSetFunction(){}

  ThresholdSegmentationLevelSetFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent );
    os << indent << "UpperThreshold: " << m_UpperThreshold << std::endl;
    os << indent << "LowerThreshold: " << m_LowerThreshold << std::endl;
    os << indent << "EdgeWeight: " << m_EdgeWeight << std::endl;
    os << indent << "SmoothingTimeStep: " << m_SmoothingTimeStep << std::endl;
    os << indent << "SmoothingIterations: " << m_SmoothingIterations << std::endl;
    os << indent << "SmoothingConductance: " << m_SmoothingConductance << std::endl;
  }
  
  FeatureScalarType m_UpperThreshold;
  FeatureScalarType m_LowerThreshold;
  ScalarValueType   m_EdgeWeight;
  ScalarValueType   m_SmoothingConductance;
  int               m_SmoothingIterations;
  ScalarValueType   m_SmoothingTimeStep;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdSegmentationLevelSetFunction.txx"
#endif

#endif
