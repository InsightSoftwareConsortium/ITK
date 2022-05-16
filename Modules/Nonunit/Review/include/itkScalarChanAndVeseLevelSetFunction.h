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
#ifndef itkScalarChanAndVeseLevelSetFunction_h
#define itkScalarChanAndVeseLevelSetFunction_h

#include "itkScalarRegionBasedLevelSetFunction.h"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkConstrainedRegionBasedLevelSetFunctionSharedData.h"

namespace itk
{
/** \class ScalarChanAndVeseLevelSetFunction
 *
 * \brief LevelSet function that computes a speed image based on regional integrals of probabilities
 *
 * This class implements a level set function that computes the speed image by
 * integrating values on the image domain.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 * Based on the papers:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 *         "Segmenting and Tracking Fluorescent Cells in Dynamic 3-D
 *          Microscopy With Coupled Active Surfaces"
 *          Dufour, Shinin, Tajbakhsh, Guillen-Aghion, Olivo-Marin
 *          In IEEE Transactions on Image Processing, vol. 14, No 9, Sep. 2005
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      https://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      https://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      https://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 * \ingroup ITKReview
 *
 * \sphinx
 * \sphinxexample{Nonunit/Review/MultiphaseChanAndVeseSparseFieldLevelSetSegmentation,Multiphase Chan And Vese Sparse
 * Field Level Set Segmentation}
 * \sphinxexample{Nonunit/Review/SinglephaseChanAndVeseSparseFieldLevelSetSegmentation,Single-phase Chan And Vese Sparse
 * Field Level Set Segmentation}
 * \sphinxexample{Nonunit/Review/SinglephaseChanAndVeseSparseFieldLevelSetSegmentation2,Single-phase Chan And Vese Dense
 * Field Level Set Segmentation} \endsphinx
 */
template <typename TInputImage,
          typename TFeatureImage,
          typename TSharedData = ConstrainedRegionBasedLevelSetFunctionSharedData<
            TInputImage,
            TFeatureImage,
            ScalarChanAndVeseLevelSetFunctionData<TInputImage, TFeatureImage>>>
class ITK_TEMPLATE_EXPORT ScalarChanAndVeseLevelSetFunction
  : public ScalarRegionBasedLevelSetFunction<TInputImage, TFeatureImage, TSharedData>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarChanAndVeseLevelSetFunction);

  using Self = ScalarChanAndVeseLevelSetFunction;
  using Superclass = ScalarRegionBasedLevelSetFunction<TInputImage, TFeatureImage, TSharedData>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarChanAndVeseLevelSetFunction, ScalarLevelSetFunction);

  static constexpr unsigned int ImageDimension = TFeatureImage::ImageDimension;

  using InputImageType = TInputImage;
  using typename Superclass::InputImageConstPointer;
  using typename Superclass::InputImagePointer;
  using typename Superclass::InputPixelType;
  using typename Superclass::InputIndexType;
  using typename Superclass::InputIndexValueType;
  using typename Superclass::InputSizeType;
  using typename Superclass::InputSizeValueType;
  using typename Superclass::InputRegionType;
  using typename Superclass::InputPointType;

  using FeatureImageType = TFeatureImage;
  using FeatureImageConstPointer = typename FeatureImageType::ConstPointer;
  using typename Superclass::FeaturePixelType;
  using typename Superclass::FeatureIndexType;
  using typename Superclass::FeatureOffsetType;

  using typename Superclass::ScalarValueType;
  using typename Superclass::NeighborhoodType;
  using typename Superclass::FloatOffsetType;
  using typename Superclass::RadiusType;
  using typename Superclass::TimeStepType;
  using typename Superclass::GlobalDataStruct;
  using typename Superclass::PixelType;
  using typename Superclass::VectorType;

  using typename Superclass::SharedDataType;
  using typename Superclass::SharedDataPointer;

  using typename Superclass::ImageIteratorType;
  using typename Superclass::ConstImageIteratorType;
  using typename Superclass::FeatureImageIteratorType;
  using typename Superclass::ConstFeatureIteratorType;

  using typename Superclass::ListPixelType;
  using typename Superclass::ListPixelConstIterator;
  using typename Superclass::ListPixelIterator;
  using typename Superclass::ListImageType;

protected:
  ScalarChanAndVeseLevelSetFunction()
    : Superclass()
  {}
  ~ScalarChanAndVeseLevelSetFunction() override = default;

  void
  ComputeParameters() override;

  void
  UpdateSharedDataParameters() override;

  ScalarValueType
  ComputeInternalTerm(const FeaturePixelType & iValue, const FeatureIndexType & iIdx) override;

  ScalarValueType
  ComputeExternalTerm(const FeaturePixelType & iValue, const FeatureIndexType & iIdx) override;

  void
  UpdateSharedDataInsideParameters(const unsigned int &     iId,
                                   const FeaturePixelType & iVal,
                                   const ScalarValueType &  iChange) override;

  void
  UpdateSharedDataOutsideParameters(const unsigned int &     iId,
                                    const FeaturePixelType & iVal,
                                    const ScalarValueType &  iChange) override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarChanAndVeseLevelSetFunction.hxx"
#endif

#endif
