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
#ifndef itkScalarChanAndVeseLevelSetFunctionData_h
#define itkScalarChanAndVeseLevelSetFunctionData_h

#include "itkRegionBasedLevelSetFunctionData.h"

namespace itk
{
/** \class ScalarChanAndVeseLevelSetFunctionData
 *
 * \brief Helper class used to share data in the ScalarChanAndVeseLevelSetFunction.
 *
 * This class holds cache data used during the computation of the LevelSet updates.
 *
 * Based on the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 *
 * \ingroup ITKReview
 */
template< typename TInputImage, typename TFeatureImage >
class ScalarChanAndVeseLevelSetFunctionData:
  public RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage >
{
public:

  typedef ScalarChanAndVeseLevelSetFunctionData                         Self;
  typedef RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage > Superclass;
  typedef SmartPointer< Self >                                          Pointer;
  typedef SmartPointer< const Self >                                    ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, TFeatureImage::ImageDimension);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(ScalarChanAndVeseLevelSetFunctionData, RegionBasedLevelSetFunctionData);

  typedef TInputImage                                 InputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputPixelType         InputPixelType;
  typedef typename Superclass::InputRegionType        InputRegionType;
  typedef typename Superclass::InputSizeType          InputSizeType;
  typedef typename Superclass::InputSizeValueType     InputSizeValueType;
  typedef typename Superclass::InputSpacingType       InputSpacingType;
  typedef typename Superclass::InputIndexType         InputIndexType;
  typedef typename Superclass::InputIndexValueType    InputIndexValueType;
  typedef typename Superclass::InputPointType         InputPointType;

  typedef TFeatureImage                                 FeatureImageType;
  typedef typename Superclass::FeatureImagePointer      FeatureImagePointer;
  typedef typename Superclass::FeatureImageConstPointer FeatureImageConstPointer;
  typedef typename Superclass::FeaturePixelType         FeaturePixelType;
  typedef typename Superclass::FeatureRegionType        FeatureRegionType;
  typedef typename Superclass::FeatureSizeType          FeatureSizeType;
  typedef typename Superclass::FeatureSizeValueType     FeatureSizeValueType;
  typedef typename Superclass::FeatureSpacingType       FeatureSpacingType;
  typedef typename Superclass::FeatureIndexType         FeatureIndexType;
  typedef typename Superclass::FeaturePointType         FeaturePointType;

  double m_BackgroundConstantValues;
  double m_ForegroundConstantValues;
  double m_WeightedSumOfPixelValuesInsideLevelSet;
  double m_WeightedSumOfPixelValuesOutsideLevelSet;

protected:
  ScalarChanAndVeseLevelSetFunctionData():Superclass()
  {
    m_BackgroundConstantValues = 0.;
    m_ForegroundConstantValues = 0.;
    m_WeightedSumOfPixelValuesInsideLevelSet = 0.;
    m_WeightedSumOfPixelValuesOutsideLevelSet = 0.;
  }

  virtual ~ScalarChanAndVeseLevelSetFunctionData() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarChanAndVeseLevelSetFunctionData);
};
} //end namespace itk

#endif
