/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPointFeature_h
#define itkPointFeature_h

#include "itkPointsLocator.h"
#include "itkMeshToMeshFilter.h"

namespace itk
{

/** \class PointFeature
 *
 * \brief Filters a image by iterating over its pixels.
 *
 * Filters a image by iterating over its pixels in a multi-threaded way
 * and {to be completed by the developer}.
 *
 * \ingroup Fpfh
 *
 */
template <typename TInputPointSet, typename TOutputPointSet>
class PointFeature : public itk::MeshToMeshFilter<TInputPointSet, TOutputPointSet>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointFeature);

  static constexpr unsigned int InputDimension = TInputPointSet::PointDimension;
  static constexpr unsigned int OutputDimension = TOutputPointSet::PointDimension;

  using InputPointSetType = TInputPointSet;
  using OutputPointSetType = TOutputPointSet;
  using InputPixelType = typename InputPointSetType::PixelType;
  using OutputPixelType = typename OutputPointSetType::PixelType;

  /** Standard class aliases. */
  using Self = PointFeature<InputPointSetType, OutputPointSetType>;
  using Superclass = MeshToMeshFilter<InputPointSetType, OutputPointSetType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using PointType = typename TInputPointSet::PointType;
  using PointIdentifier = typename TInputPointSet::PointIdentifier;
  using PointsVectorContainer = typename TInputPointSet::PointsVectorContainer;

  using InputPointSetPointsContainerConstPointer = typename TInputPointSet::PointsContainerConstPointer;
  using PointsContainerConstIterator = typename InputPointSetType::PointsContainer::ConstIterator;
  using Vector3d = typename itk::Vector<double, 3>;
  using Vector4d = typename itk::Vector<double, 4>;

  using PointsLocatorType = typename itk::PointsLocator<itk::VectorContainer<PointIdentifier, PointType>>;
  using PointsLocatorTypePointer = typename PointsLocatorType::Pointer;
  // using FeatureType = std::vector<double>;
  using FeatureType = typename itk::VectorContainer<PointIdentifier, double>;
  using FeatureTypePointer = typename FeatureType::Pointer;

  /** Run-time type information. */
  itkOverrideGetNameOfClassMacro(PointFeature);

  /** Standard New macro. */
  itkNewMacro(Self);

  itkGetConstObjectMacro(FpfhFeature, FeatureType);

  void
  ComputeFPFHFeature(InputPointSetType * input,
                     InputPointSetType * input_normals,
                     double              radius,
                     unsigned int        neighbors);

protected:
  PointFeature();
  ~PointFeature() override = default;

  void
  GenerateData() override;

  Vector4d
  ComputePairFeatures(const Vector3d & p1, const Vector3d & n1, const Vector3d & p2, const Vector3d & n2);

  FeatureTypePointer
  ComputeSPFHFeature(InputPointSetType * input,
                     InputPointSetType * input_normals,
                     double              radius,
                     unsigned int        neighbors);

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  FeatureTypePointer m_FpfhFeature;
#ifdef ITK_USE_CONCEPT_CHECKING
  // Add concept checking such as
  // itkConceptMacro( FloatingPointPixel, ( itk::Concept::IsFloatingPoint< typename InputImageType::PixelType > ) );
#endif
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointFeature.hxx"
#endif

#endif // itkPointFeature
