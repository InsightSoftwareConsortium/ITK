/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkNarrowBandCurvesLevelSetImageFilter_h
#define itkNarrowBandCurvesLevelSetImageFilter_h

#include "itkNarrowBandLevelSetImageFilter.h"
#include "itkCurvesLevelSetFunction.h"

namespace itk
{
/** \class NarrowBandCurvesLevelSetImageFilter
 *  \brief Segments structures in images based on user supplied edge potential map.
 *
 *   \par IMPORTANT
 *   The NarrowBandLevelSetImageFilter class and the
 *   CurvesLevelSetFunction class contain additional information necessary
 *   to the full understanding of how to use this filter.
 *
 *    \par OVERVIEW
 *    This class is a level set method segmentation filter. An initial contour
 *    is propagated outwards (or inwards) until it sticks to the shape boundaries.
 *    This is done by using a level set speed function based on a user supplied
 *    edge potential map.
 *
 *    \par INPUTS
 *    This filter requires two inputs.  The first input is a initial level set.
 *    The initial level set is a real image which contains the initial contour/surface
 *    as the zero level set. For example, a signed distance function from the initial
 *    contour/surface is typically used. Unlike the simpler ShapeDetectionLevelSetImageFilter
 *    the initial contour does not have to lie wholly within the shape to be segmented.
 *    The initial contour is allow to overlap the shape boundary. The extra advection term
 *    in the update equation behaves like a doublet and attracts the contour to the boundary.
 *    This approach for segmentation follows that of Lorigo et al (2001).
 *
 *    \par
 *    The second input is the feature image.  For this filter, this is the edge
 *    potential map. General characteristics of an edge potential map is that
 *    it has values close to zero in regions near the edges and values close
 *    to one inside the shape itself. Typically, the edge potential map is compute
 *    from the image gradient, for example:
 *
 *    \f[ g(I) = 1 / ( 1 + | (\nabla * G)(I)| ) \f]
 *    \f[ g(I) = \exp^{-|(\nabla * G)(I)|} \f]
 *
 *    where \f$ I \f$ is image intensity and
 *    \f$ (\nabla * G) \f$ is the derivative of Gaussian operator.
 *
 *    \par
 *    See NarrowBandLevelSetImageFilter and NarrowBandImageFilterBase
 *    for more information on Inputs.
 *
 *    \par PARAMETERS
 *    The method SetUseNegatiiveFeatures() can be used to switch from propagating inwards (false)
 *    versus propagating outwards (true).
 *
 *    This implementation allows the user to set the weights between the propagation, advection
 *    and curvature term using methods SetPropagationScaling(), SetAdvectionScaling(),
 *    SetCurvatureScaling(). In general, the larger the CurvatureScaling, the smoother the
 *    resulting contour. To follow the implementation in Caselles's paper,
 *    set the PropagationScaling to \f$ c \f$ (the inflation or ballon force) and
 *    AdvectionScaling and CurvatureScaling both to 1.0.
 *
 *    \par OUTPUTS
 *    The filter outputs a single, scalar, real-valued image.
 *    Negative values in the output image are inside the segmented region
 *    and positive values in the image are outside of the inside region.  The
 *    zero crossings of the image correspond to the position of the level set
 *    front.
 *
 *    \par REFERENCES
 *    L. Lorigo, O. Faugeras, W.E.L. Grimson, R. Keriven, R. Kikinis, A. Nabavi,
 *    and C.-F. Westin, Curves: Curve evolution for vessel segmentation.
 *    Medical Image Analysis, 5:195-206, 2001.
 *
 *   \par
 *   See NarrowBandImageFilterBase and
 *   NarrowBandLevelSetImageFilter for more information.
 *
 *   \sa NarrowBandLevelSetImageFilter
 *   \sa CurvesLevelSetFunction
 *
 *   \ingroup LevelSetSegmentation
 * \ingroup ITKLevelSets
 */
template <typename TInputImage, typename TFeatureImage, typename TOutputPixelType = float>
class ITK_TEMPLATE_EXPORT NarrowBandCurvesLevelSetImageFilter
  : public NarrowBandLevelSetImageFilter<TInputImage,
                                         TFeatureImage,
                                         TOutputPixelType,
                                         Image<TOutputPixelType, TInputImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NarrowBandCurvesLevelSetImageFilter);

  /** Standard class type aliases */
  using Self = NarrowBandCurvesLevelSetImageFilter;
  using Superclass = NarrowBandLevelSetImageFilter<TInputImage,
                                                   TFeatureImage,
                                                   TOutputPixelType,
                                                   Image<TOutputPixelType, TInputImage::ImageDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Inherited type alias from the superclass. */
  using ValueType = typename Superclass::ValueType;
  using OutputImageType = typename Superclass::OutputImageType;
  using FeatureImageType = typename Superclass::FeatureImageType;

  /** Type of the segmentation function */
  using CurvesFunctionType = CurvesLevelSetFunction<OutputImageType, FeatureImageType>;
  using CurvesFunctionPointer = typename CurvesFunctionType::Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NarrowBandCurvesLevelSetImageFilter, NarrowBandLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Set the value of sigma used to compute derivatives */
  void
  SetDerivativeSigma(float value)
  {
    m_CurvesFunction->SetDerivativeSigma(value);
    this->Modified();
  }

  float
  GetDerivativeSigma() const
  {
    return m_CurvesFunction->GetDerivativeSigma();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<TOutputPixelType>));
  // End concept checking
#endif

protected:
  ~NarrowBandCurvesLevelSetImageFilter() override = default;
  NarrowBandCurvesLevelSetImageFilter();

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Overridden from Superclass to handle the case when Propagation
   *  Scaling is zero.*/
  void
  GenerateData() override;

private:
  CurvesFunctionPointer m_CurvesFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNarrowBandCurvesLevelSetImageFilter.hxx"
#endif

#endif
