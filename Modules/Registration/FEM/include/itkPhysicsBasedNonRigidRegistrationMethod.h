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

#ifndef itkPhysicsBasedNonRigidRegistrationMethod_h
#define itkPhysicsBasedNonRigidRegistrationMethod_h


#include "itkMaskFeaturePointSelectionFilter.h"
#include "itkBlockMatchingImageFilter.h"
#include "itkFEMScatteredDataPointSetToImageFilter.h"
#include "itkConceptChecking.h"


namespace itk
{
namespace fem
{

/** \class PhysicsBasedNonRigidRegistrationMethod
 * \brief Perform a non-rigid registration of two 3D images using a set of
 * feature points, block matching and linear elastic model.
 *
 * PhysicsBasedNonRigidRegistrationMethod takes a fixed image, a moving
 * image with an optional mask and a mesh as inputs and generates a
 * dense deformation field image as an output.
 *
 * This filter is intended to be used for driving the process of Physics-
 * Based Non-Rigid Registration. It computes feature points from the
 * moving image, then computes displacements of the feature points in the
 * fixed image via block-matching, then computes deformation field of a
 * whole image using linear elastic model[ M. Bierling, Displacement
 * estimation by hierarchical block matching, Proc. SPIE Vis. Comm. and
 * Image Proc., vol. 1001, pp. 942-951, 1988. ].
 *
 * The filter is templated over fixed image, moving image, mask, mesh and
 * deformation field image.
 *
 * \author Andriy Kot, Center for Real-Time Computing, Old Dominion University,
 * Norfolk, VA
 *
 * \ingroup ITKFEMRegistration
 */


template <typename TFixedImage, typename TMovingImage, typename TMaskImage, typename TMesh, typename TDeformationField>
class ITK_TEMPLATE_EXPORT PhysicsBasedNonRigidRegistrationMethod
  : public ImageToImageFilter<TMovingImage, TDeformationField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PhysicsBasedNonRigidRegistrationMethod);

  using Self = PhysicsBasedNonRigidRegistrationMethod;
  using Superclass = ImageToImageFilter<TMovingImage, TDeformationField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(PhysicsBasedNonRigidRegistrationMethod, ImageToImageFilter);

  using MovingImageType = TMovingImage;
  using FixedImageType = TFixedImage;
  using MaskImageType = TMaskImage;
  using MeshType = TMesh;
  using DeformationFieldType = TDeformationField;

  static constexpr unsigned int ImageDimension = FixedImageType::ImageDimension;

  /** Not input specific type alias */
  using ImageRegionType = ImageRegion<ImageDimension>;
  using ImageSizeType = Size<ImageDimension>;
  using ImageIndexType = Index<ImageDimension>;

  /** Typedefs for the components filters. */
  using FeatureSelectionFilterType = MaskFeaturePointSelectionFilter<MovingImageType, MaskImageType>;
  using BlockMatchingFilterType = BlockMatchingImageFilter<FixedImageType, MovingImageType>;
  using FEMFilterType = FEMScatteredDataPointSetToImageFilter<
    typename BlockMatchingFilterType::DisplacementsType,
    MeshType,
    DeformationFieldType,
    typename BlockMatchingFilterType::SimilaritiesType,
    typename FeatureSelectionFilterType::FeaturePointsType>; // tensors are optional pixel values of feature points
                                                             // pointset

  /** set fraction of eligible points to select */
  itkSetMacro(SelectFraction, double);
  itkGetConstMacro(SelectFraction, double);

  /** set/get non-connectivity */
  itkSetMacro(NonConnectivity, unsigned int);
  itkGetConstMacro(NonConnectivity, unsigned int);

  /** set/get half size */
  itkSetMacro(BlockRadius, ImageSizeType);
  itkGetConstReferenceMacro(BlockRadius, ImageSizeType);

  /** set/get half window */
  itkSetMacro(SearchRadius, ImageSizeType);
  itkGetConstReferenceMacro(SearchRadius, ImageSizeType);

  /** set/get number of approximation steps */
  itkSetMacro(ApproximationSteps, unsigned int);
  itkGetMacro(ApproximationSteps, unsigned int);

  /** set/get number of outlier rejection steps */
  itkSetMacro(OutlierRejectionSteps, unsigned int);
  itkGetMacro(OutlierRejectionSteps, unsigned int);

  /** set/get fixed image */
  itkSetInputMacro(FixedImage, FixedImageType);
  itkGetInputMacro(FixedImage, FixedImageType);

  /** set/get moving image */
  itkSetInputMacro(MovingImage, MovingImageType);
  itkGetInputMacro(MovingImage, MovingImageType);

  /** set/get moving image */
  itkSetInputMacro(MaskImage, MaskImageType);
  itkGetInputMacro(MaskImage, MaskImageType);

  /** set/get moving image */
  itkSetInputMacro(Mesh, MeshType);
  itkGetInputMacro(Mesh, MeshType);

  /** get FEMFilter */
  itkGetConstObjectMacro(FEMFilter, FEMFilterType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  /* Currently only the 3D implementation is available due to a narrow
     definition of the filter in the original proposal
     and lack of available resources. */
  itkConceptMacro(FixedImageDimensionShouldBe3, (Concept::SameDimension<TFixedImage::ImageDimension, 3u>));
  itkConceptMacro(MovingImageDimensionShouldBe3, (Concept::SameDimension<TMovingImage::ImageDimension, 3u>));
  itkConceptMacro(MaskImageDimensionShouldBe3, (Concept::SameDimension<TMaskImage::ImageDimension, 3u>));
  itkConceptMacro(MeshDimensionShouldBe3, (Concept::SameDimension<TMesh::PointType::PointDimension, 3u>));
  itkConceptMacro(DeformationFieldImageDimensionShouldBe3,
                  (Concept::SameDimension<TDeformationField::ImageDimension, 3u>));
  // End concept checking
#endif

protected:
  PhysicsBasedNonRigidRegistrationMethod();
  ~PhysicsBasedNonRigidRegistrationMethod() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
  GenerateData() override;

private:
  double        m_SelectFraction{ 0.1 };
  unsigned int  m_NonConnectivity{ 0 };
  ImageSizeType m_BlockRadius;
  ImageSizeType m_SearchRadius;
  unsigned int  m_ApproximationSteps{ 10 };
  unsigned int  m_OutlierRejectionSteps{ 10 };

  typename FeatureSelectionFilterType::Pointer m_FeatureSelectionFilter;
  typename BlockMatchingFilterType::Pointer    m_BlockMatchingFilter;
  typename FEMFilterType::Pointer              m_FEMFilter;
};
} // end namespace fem
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhysicsBasedNonRigidRegistrationMethod.hxx"
#endif

#endif
