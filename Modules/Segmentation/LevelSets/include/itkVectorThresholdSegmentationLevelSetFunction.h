/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVectorThresholdSegmentationLevelSetFunction_h
#define itkVectorThresholdSegmentationLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"
#include "itkNumericTraits.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
namespace itk
{
/** \class VectorThresholdSegmentationLevelSetFunction
 *
 * \brief This function is used in VectorThresholdSegmentationLevelSetImageFilter to
 * segment structures in images based on the Mahalanobis distance.
 *
 *   \par CREDITS
 *   This class was contributed to ITK by Stefan Lindenau
 *   https://www.itk.org/pipermail/insight-users/2003-December/005969.html
 *
 * \par  SegmentationLevelSetFunction is a subclass of the generic LevelSetFunction.
 * It useful for segmentations based on intensity values in an image.  It works
 * by constructing a speed term (feature image) with positive values inside an
 * intensity window (between a low and high threshold) and negative values
 * outside that intensity window.  The evolving level set front will lock onto
 * regions that are at the edges of the intensity window.
 *
 *
 *  \par
 *  Image \f$ f(x) \f$ is thresholded pixel by pixel using threshold \f$T\f$
 *  according to the following formula.
 *
 *  \par
 *  \f[
 *           f(x) = T - MahalanobisDistance(x)
 *  \f]
 *
 *  \sa SegmentationLevelSetImageFunction
 *  \sa ThresholdSegmentationLevelSetImageFilter
 *  \sa MahalanobisDistanceMembershipFunction
 * \ingroup ITKLevelSets
 */
template< typename TImageType, typename TFeatureImageType >
class ITK_TEMPLATE_EXPORT VectorThresholdSegmentationLevelSetFunction:
  public SegmentationLevelSetFunction< TImageType, TFeatureImageType >
{
public:
  /** Standard class typedefs. */
  typedef VectorThresholdSegmentationLevelSetFunction Self;
  typedef SegmentationLevelSetFunction< TImageType, TFeatureImageType >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef TFeatureImageType          FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorThresholdSegmentationLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType         ImageType;
  typedef typename Superclass::ScalarValueType   ScalarValueType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;
  typedef typename Superclass::RadiusType        RadiusType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Extract the number of components in the vector pixel type . */
  typedef typename FeatureImageType::PixelType FeatureImagePixelType;
  itkStaticConstMacro(NumberOfComponents, unsigned int,
                      FeatureImagePixelType::Dimension);

  typedef Statistics::MahalanobisDistanceMembershipFunction< FeatureScalarType > MahalanobisFunctionType;
  typedef typename MahalanobisFunctionType::Pointer                              MahalanobisFunctionPointer;
  typedef typename MahalanobisFunctionType::MeanVectorType                       MeanVectorType;
  typedef typename MahalanobisFunctionType::CovarianceMatrixType                 CovarianceMatrixType;

  /** Set/Get mean and covariance */
  void SetMean(const MeanVectorType & mean) {  m_Mahalanobis->SetMean(mean); }
  const MeanVectorType & GetMean() const {  return m_Mahalanobis->GetMean(); }

  void SetCovariance(const CovarianceMatrixType & cov) { m_Mahalanobis->SetCovariance(cov); }
  const CovarianceMatrixType & GetCovariance() const { return m_Mahalanobis->GetCovariance(); }

  /** Set/Get the threshold value for the MahanalobisDistance */
  void SetThreshold(ScalarValueType thr)
  {
    m_Threshold = thr;
  }

  ScalarValueType GetThreshold()
  {
    return m_Threshold;
  }

  virtual void CalculateSpeedImage() ITK_OVERRIDE;

  virtual void Initialize(const RadiusType & r) ITK_OVERRIDE
  {
    Superclass::Initialize(r);

    this->SetAdvectionWeight(NumericTraits< ScalarValueType >::ZeroValue());
    this->SetPropagationWeight(-1.0 * NumericTraits< ScalarValueType >::OneValue());
    this->SetCurvatureWeight(NumericTraits< ScalarValueType >::OneValue());
  }

protected:
  VectorThresholdSegmentationLevelSetFunction()
  {
    MeanVectorType       mean(NumberOfComponents);
    CovarianceMatrixType covariance(NumberOfComponents, NumberOfComponents);

    mean.Fill(NumericTraits< typename FeatureScalarType::ValueType >::ZeroValue());
    covariance.Fill(NumericTraits< typename FeatureScalarType::ValueType >::ZeroValue());

    m_Mahalanobis = MahalanobisFunctionType::New();
    m_Mahalanobis->SetMean(mean);
    m_Mahalanobis->SetCovariance(covariance);

    this->SetAdvectionWeight(0.0);
    this->SetPropagationWeight(1.0);
    this->SetThreshold(1.8);
  }

  virtual ~VectorThresholdSegmentationLevelSetFunction() ITK_OVERRIDE {}

  ITK_DISALLOW_COPY_AND_ASSIGN(VectorThresholdSegmentationLevelSetFunction);

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "MahalanobisFunction: " << m_Mahalanobis << std::endl;
    os << indent << "ThresholdValue: " << m_Threshold << std::endl;
  }

  MahalanobisFunctionPointer m_Mahalanobis;
  ScalarValueType            m_Threshold;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorThresholdSegmentationLevelSetFunction.hxx"
#endif

#endif
