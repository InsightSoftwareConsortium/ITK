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
#ifndef itkVectorThresholdSegmentationLevelSetImageFilter_h
#define itkVectorThresholdSegmentationLevelSetImageFilter_h

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkVectorThresholdSegmentationLevelSetFunction.h"

namespace itk
{
/** \class VectorThresholdSegmentationLevelSetImageFilter
 *    \brief Segments structures in images based on intensity values.
 *
 *   \par IMPORTANT
 *   The SegmentationLevelSetImageFilter class and the
 *   VectorThresholdSegmentationLevelSetFunction class contain additional information necessary
 *   to the full understanding of how to use this filter.
 *
 *   \par CREDITS
 *   This class was contributed to ITK by Stefan Lindenau
 *   https://www.itk.org/pipermail/insight-users/2003-December/005969.html
 *
 *    \par OVERVIEW
 *    This class is a level set method segmentation filter.  It constructs a
 *    speed function which is close to zero where the Mahalabonian Distance
 *    exceeds a certain threshold, effectively locking the propagating front onto those
 *    edges.  Elsewhere, the front will propagate quickly.
 *
 *    \par INPUTS
 *    This filter requires two inputs.  The first input is a seed
 *    image.  This seed image must contain an isosurface that you want to use as the
 *    seed for your segmentation.  It can be a binary, graylevel, or floating
 *    point image.  The only requirement is that it contain a closed isosurface
 *    that you will identify as the seed by setting the IsosurfaceValue parameter
 *    of the filter.  For a binary image you will want to set your isosurface
 *    value halfway between your on and off values (i.e. for 0's and 1's, use an
 *    isosurface value of 0.5).
 *
 *    \par
 *    The second input is the feature image.  This is the image from which the
 *    speed function will be calculated the feature image has to be a Vector Image.
 *    For most applications, this is the
 *    image that you want to segment. The desired isosurface in your seed image
 *    should lie within the region of your feature image that you are trying to
 *    segment. Note that this filter does no preprocessing of the feature image
 *    before thresholding.
 *
 *    \par
 *    See SegmentationLevelSetImageFilter for more information on Inputs.
 *
 *    \par OUTPUTS
 *    The filter outputs a single, scalar, real-valued image.
 *    Positive values in the output image are inside the segmentated region
 *    and negative values in the image are outside of the inside region.  The
 *    zero crossings of the image correspond to the position of the level set
 *    front.
 *
 *   \par
 *   See SparseFieldLevelSetImageFilter and
 *   SegmentationLevelSetImageFilter for more information.
 *
 *   \par PARAMETERS
 *   In addition to parameters described in SegmentationLevelSetImageFilter,
 *   this filter adds the Threshold, the Mean and the Covariance.  See
 *   VectorThresholdSegmentationLevelSetFunction for a description of how this value
 *   affect the segmentation.
 *
 *   \sa SegmentationLevelSetImageFilter
 *   \sa ThresholdSegmentationLevelSetFunction,
 *   \sa SparseFieldLevelSetImageFilter
 * \ingroup ITKLevelSets
 */
template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputPixelType = float >
class ITK_TEMPLATE_EXPORT VectorThresholdSegmentationLevelSetImageFilter:
  public SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
{
public:
  /** Standard class typedefs */
  typedef VectorThresholdSegmentationLevelSetImageFilter                                   Self;
  typedef  SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType > Superclass;
  typedef SmartPointer< Self >                                                             Pointer;
  typedef SmartPointer< const Self >                                                       ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType        ValueType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;

  /** Type of the segmentation function */
  typedef VectorThresholdSegmentationLevelSetFunction< OutputImageType, FeatureImageType > ThresholdFunctionType;
  typedef typename ThresholdFunctionType::Pointer                                          ThresholdFunctionPointer;
  typedef typename ThresholdFunctionType::MeanVectorType                                   MeanVectorType;
  typedef typename ThresholdFunctionType::CovarianceMatrixType                             CovarianceMatrixType;
  typedef typename ThresholdFunctionType::ScalarValueType                                  ScalarValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorThresholdSegmentationLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Set/Get mean and covariance that will be used to calculate the speed
    function */
  void SetMean(const MeanVectorType & mean)
  {
    m_ThresholdFunction->SetMean(mean);
    this->Modified();
  }

  const MeanVectorType & GetMean() const
  {
    return m_ThresholdFunction->GetMean();
  }

  void SetCovariance(const CovarianceMatrixType & cov)
  {
    m_ThresholdFunction->SetCovariance(cov);
    this->Modified();
  }

  const CovarianceMatrixType & GetCovariance() const
  {
    return m_ThresholdFunction->GetCovariance();
  }

  /** Set/Get the threshold for the Mahanalobis Distance */
  void SetThreshold(ScalarValueType thr)
  {
    m_ThresholdFunction->SetThreshold(thr);
    this->Modified();
  }

  ScalarValueType GetThreshold()
  {
    return m_ThresholdFunction->GetThreshold();
  }

protected:
  ~VectorThresholdSegmentationLevelSetImageFilter() ITK_OVERRIDE {}
  VectorThresholdSegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  ITK_DISALLOW_COPY_AND_ASSIGN(VectorThresholdSegmentationLevelSetImageFilter);

private:
  ThresholdFunctionPointer m_ThresholdFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorThresholdSegmentationLevelSetImageFilter.hxx"
#endif

#endif
