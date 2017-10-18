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
#ifndef itkCurvesLevelSetFunction_h
#define itkCurvesLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"

namespace itk
{
/** \class CurvesLevelSetFunction
 *
 * \brief This function is used in CurvesLevelSetImageFilter to
 * segment structures in images based on user supplied edge potential map.
 *
 * \par CurvesLevelSetFunction is a subclass of the generic LevelSetFunction.
 * It is useful for segmentations based on a user supplied edge potential map which
 * has values close to zero in regions near edges (or high image gradient) and values
 * close to one in regions with relatively constant intensity. Typically, the edge
 * potential map is a function of the gradient, for example:
 *
 * \f[ g(I) = 1 / ( 1 + | (\nabla * G)(I)| ) \f]
 * \f[ g(I) = \exp^{-|(\nabla * G)(I)|} \f]
 *
 * where \f$ I \f$ is image intensity and
 * \f$ (\nabla * G) \f$ is the derivative of Gaussian operator.
 *
 * \par In this function both the propagation term \f$ P(\mathbf{x}) \f$
 * and the curvature spatial modifier term $\f$ Z(\mathbf{x}) \f$ are taken directly
 * from the edge potential image. The edge potential image is set via the
 * SetFeatureImage() method. An advection term \f$ A(\mathbf{x}) \f$ is constructed
 * from the negative gradient of the edge potential image. This term behaves like
 * a doublet attracting the contour to the edges.
 *
 * \par This implementation is based on:
 *  L. Lorigo, O. Faugeras, W.E.L. Grimson, R. Keriven, R. Kikinis, A. Nabavi,
 *  and C.-F. Westin, Curves: Curve evolution for vessel segmentation.
 *  Medical Image Analysis, 5:195-206, 2001.
 *
 * \sa LevelSetFunction
 * \sa SegmentationLevelSetImageFunction
 * \sa GeodesicActiveContourLevelSetImageFilter
 *
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKLevelSets
 */
template< typename TImageType, typename TFeatureImageType = TImageType >
class ITK_TEMPLATE_EXPORT CurvesLevelSetFunction:
  public SegmentationLevelSetFunction< TImageType, TFeatureImageType >
{
public:
  /** Standard class typedefs. */
  typedef CurvesLevelSetFunction                                        Self;
  typedef SegmentationLevelSetFunction< TImageType, TFeatureImageType > Superclass;
  typedef LevelSetFunction< TImageType >                                SuperSuperclass;
  typedef SmartPointer< Self >                                          Pointer;
  typedef SmartPointer< const Self >                                    ConstPointer;
  typedef TFeatureImageType                                             FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CurvesLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  typedef typename SuperSuperclass::PixelType        PixelType;
  typedef typename Superclass::ImageType             ImageType;
  typedef typename Superclass::NeighborhoodType      NeighborhoodType;
  typedef typename Superclass::ScalarValueType       ScalarValueType;
  typedef typename Superclass::FeatureScalarType     FeatureScalarType;
  typedef typename Superclass::RadiusType            RadiusType;
  typedef typename SuperSuperclass::FloatOffsetType  FloatOffsetType;
  typedef typename SuperSuperclass::GlobalDataStruct GlobalDataStruct;
  typedef typename Superclass::VectorImageType       VectorImageType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Compute speed image from feature image. */
  virtual void CalculateSpeedImage() ITK_OVERRIDE;

  /** Compute the advection field from feature image. */
  virtual void CalculateAdvectionImage() ITK_OVERRIDE;

  /** The curvature speed is same as the propagation speed. */
  virtual ScalarValueType CurvatureSpeed(const NeighborhoodType & neighborhood,
                                         const FloatOffsetType & offset, GlobalDataStruct *gd) const ITK_OVERRIDE
  { return this->PropagationSpeed(neighborhood, offset, gd); }

  /** Set/Get the sigma for the Gaussian kernel used to compute the gradient
   * of the feature image needed for the advection term of the equation. */
  void SetDerivativeSigma(const double v)
  { m_DerivativeSigma = v; }
  double GetDerivativeSigma()
  { return m_DerivativeSigma; }

  virtual void Initialize(const RadiusType & r) ITK_OVERRIDE;

protected:
  CurvesLevelSetFunction() :
    m_Center(0),
    m_DerivativeSigma(1.0)
  {
    //Curvature term is the minimal curvature.
    this->UseMinimalCurvatureOn();
    this->SetAdvectionWeight(NumericTraits< ScalarValueType >::OneValue());
    this->SetPropagationWeight(NumericTraits< ScalarValueType >::OneValue());
    this->SetCurvatureWeight(NumericTraits< ScalarValueType >::OneValue());
  }

  virtual ~CurvesLevelSetFunction() ITK_OVERRIDE {}

  ITK_DISALLOW_COPY_AND_ASSIGN(CurvesLevelSetFunction);

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "DerivativeSigma: " << m_DerivativeSigma << std::endl;
  }

private:

  /** Slices for the ND neighborhood. */
  std::slice x_slice[ImageDimension];

  /** The offset of the center pixel in the neighborhood. */
  OffsetValueType m_Center;

  /** Stride length along the y-dimension. */
  OffsetValueType m_xStride[ImageDimension];

  double m_DerivativeSigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvesLevelSetFunction.hxx"
#endif

#endif
