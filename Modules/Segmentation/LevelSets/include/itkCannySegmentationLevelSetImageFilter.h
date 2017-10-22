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
#ifndef itkCannySegmentationLevelSetImageFilter_h
#define itkCannySegmentationLevelSetImageFilter_h

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkCannySegmentationLevelSetFunction.h"

namespace itk
{
/**   \class CannySegmentationLevelSetImageFilter
 *    \brief Segments structures in images based on image features derived from
 *           pseudo-canny-edges.
 *
 *   \par IMPORTANT
 *   The SegmentationLevelSetImageFilter class and the
 *   CannySegmentationLevelSetFunction class contain additional information necessary
 *   to the full understanding of how to use this filter.
 *
 *    \par OVERVIEW
 *    This class is a level set method segmentation filter.  It constructs a
 *    speed function which is designed to lock onto edges as detected by a Canny
 *    filter.
 *
 *   \par
 *    The CannySegmentationLevelSetImageFilter can be a tool for refining an
 *    existing segmentation, or it can be used to try to segment a region by
 *    itself.  Like all other level-set based segmentation filters (see
 *    SegmentationLevelSetImageFilter), it works by first constructing a scalar
 *    speed term and a vector advection field based on edge features in the
 *    image.  The level set front is then moved according to these two terms
 *    with the addition of a third curvature term to contol the smoothness of
 *    the solution.
 *
 *  \par
 *    The speed term is constructed as the Danielsson distance transform of the
 *    Canny edge image, as calculated by the CannyEdgeDetectionImageFilter.
 *    This scalar speed can be tuned in and out of the final evolution equation
 *    by setting the PropagationScaling parameter (a value of 0 removes the
 *    speed term).
 *
 *   \par
 *   The advection field term is constructed by minimizing Danielsson distance
 *   squared.  i.e. \f$ \mbox{min} \int D^2 \Rightarrow D \nabla D \f$.  This
 *   term  moves the level set down the gradient of the distance transform.
 *
 *   \par
 *   In practice, you may set the speed (propagation) term to zero if your
 *   initialization is already close to the edge you are interested in.  If you
 *   are trying to segment a region by seeding with a small surface (blob,
 *   sphere) then you will likely want to add speed (propagation) to the
 *   equation so that the levelsets can expand along zero gradients.  The
 *   relative influence of these two terms are controlled by the
 *   SetPropagationScaling and SetAdvectionScaling parameters.
 *
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
 *    speed function will be calculated.  For most applications, this is the
 *    image that you want to segment. The desired isosurface in your seed image
 *    should lie within the region of your feature image that you are trying to
 *    segment.
 *
 *    \par
 *    See SegmentationLevelSetImageFilter for more information on Inputs.
 *
 *    \par OUTPUTS
 *    The filter outputs a single, scalar, real-valued image.
 *    Positive *values in the output image are inside the segmentated region
 *    and negative *values in the image are outside of the inside region.  The
 *    zero crossings of *the image correspond to the position of the level set
 *    front.
 *
 *   \par
 *   See SparseFieldLevelSetImageFilter and
 *   SegmentationLevelSetImageFilter for more information.
 *
 *   \par PARAMETERS
 *   There are five parameters important for controlling the behavior of this
 *   filter.
 *   \par
 *   (1) Threshold.  Sets the thresholding value of the Canny edge detection.
 *   See CannyEdgeDetectionImageFilter for more information.
 *   \par
 *   (2) Variance.  Controls the smoothing parameter of the gaussian filtering
 *   done during Canny edge detection.
 *   \par
 *   (3) CurvatureScaling.  Controls the degree to which curvature influences
 *   the evolution of the level set.  Higher values relative to Propagation and
 *   Advection scalings will yield a smoother surface.
 *   \par
 *   (4) PropagationScaling.  Scales the propagation (speed) term of the level
 *   set equation.  Set this term to zero to allow the level set to flow _only_
 *   down the gradient of the distance transform.
 *   \par
 *   (5) AdvectionScaling.  Scales influence of the advection field relative to
 *   curvature and propagation terms.
 *
 *   \sa SegmentationLevelSetImageFilter
 *   \sa CannySegmentationLevelSetFunction,
 *   \sa SparseFieldLevelSetImageFilter
 * \ingroup ITKLevelSets
 */
template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputPixelType = float >
class ITK_TEMPLATE_EXPORT CannySegmentationLevelSetImageFilter:
  public SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
{
public:
  /** Standard class typedefs */
  typedef CannySegmentationLevelSetImageFilter Self;
  typedef  SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType        ValueType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;
  typedef typename Superclass::VectorImageType  VectorImageType;
  typedef typename Superclass::SpeedImageType   SpeedImageType;

  /** Type of the segmentation function */
  typedef::itk::CannySegmentationLevelSetFunction< OutputImageType,
                                                   FeatureImageType > CannyFunctionType;

  typedef typename CannyFunctionType::ScalarValueType ScalarValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CannySegmentationLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Set the Threshold parameter of the CannyEdgeDetectionImageFilter
   * used by the underlying level set function. */
  void SetThreshold(ScalarValueType v)
  { this->m_CannyFunction->SetThreshold(v); }
  ScalarValueType GetThreshold() const
  { return this->m_CannyFunction->GetThreshold(); }

  /** Set the Variance parameter of the CannyEdgeDetectionImageFilter
   * used by the underlying level set function. */
  void SetVariance(double v)
  { this->m_CannyFunction->SetVariance(v); }
  double GetVariance() const
  { return this->m_CannyFunction->GetVariance(); }

  /** Get the Canny image that was used to create the speed and
      advection images */
  OutputImageType * GetCannyImage(void)
  { return this->m_CannyFunction->GetCannyImage(); }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< TOutputPixelType > ) );
  // End concept checking
#endif

protected:
  ~CannySegmentationLevelSetImageFilter() ITK_OVERRIDE {}
  CannySegmentationLevelSetImageFilter();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CannySegmentationLevelSetImageFilter);

  typename CannyFunctionType::Pointer m_CannyFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCannySegmentationLevelSetImageFilter.hxx"
#endif

#endif
