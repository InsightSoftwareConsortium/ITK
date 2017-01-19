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
 * \ingroup ITKReview
 *
 * \wiki
 * \wikiexample{Segmentation/MultiphaseChanAndVeseSparseFieldLevelSetSegmentation,Multiphase Chan And Vese Sparse Field Level Set Segmentation}
 * \wikiexample{Segmentation/SinglephaseChanAndVeseSparseFieldLevelSetSegmentation,Single-phase Chan And Vese Sparse Field Level Set Segmentation}
 * \wikiexample{Segmentation/SinglephaseChanAndVeseDenseFieldLevelSetSegmentation,Single-phase Chan And Vese Dense Field Level Set Segmentation}
 * \endwiki
 */
template< typename TInputImage,
          typename TFeatureImage,
          typename TSharedData = ConstrainedRegionBasedLevelSetFunctionSharedData< TInputImage, TFeatureImage,
                                                                                ScalarChanAndVeseLevelSetFunctionData<
                                                                                  TInputImage, TFeatureImage > > >
class ITK_TEMPLATE_EXPORT ScalarChanAndVeseLevelSetFunction:
  public ScalarRegionBasedLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
{
public:
  typedef ScalarChanAndVeseLevelSetFunction                                            Self;
  typedef ScalarRegionBasedLevelSetFunction< TInputImage, TFeatureImage, TSharedData > Superclass;
  typedef SmartPointer< Self >                                                         Pointer;
  typedef SmartPointer< const Self >                                                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarChanAndVeseLevelSetFunction, ScalarLevelSetFunction);

  itkStaticConstMacro(ImageDimension, unsigned int, TFeatureImage::ImageDimension);

  typedef TInputImage                                 InputImageType;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::InputPixelType         InputPixelType;
  typedef typename Superclass::InputIndexType         InputIndexType;
  typedef typename Superclass::InputIndexValueType    InputIndexValueType;
  typedef typename Superclass::InputSizeType          InputSizeType;
  typedef typename Superclass::InputSizeValueType     InputSizeValueType;
  typedef typename Superclass::InputRegionType        InputRegionType;
  typedef typename Superclass::InputPointType         InputPointType;

  typedef TFeatureImage                           FeatureImageType;
  typedef typename FeatureImageType::ConstPointer FeatureImageConstPointer;
  typedef typename Superclass::FeaturePixelType   FeaturePixelType;
  typedef typename Superclass::FeatureIndexType   FeatureIndexType;
  typedef typename Superclass::FeatureOffsetType  FeatureOffsetType;

  typedef typename Superclass::ScalarValueType  ScalarValueType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::VectorType       VectorType;

  typedef typename Superclass::SharedDataType    SharedDataType;
  typedef typename Superclass::SharedDataPointer SharedDataPointer;

  typedef typename Superclass::ImageIteratorType        ImageIteratorType;
  typedef typename Superclass::ConstImageIteratorType   ConstImageIteratorType;
  typedef typename Superclass::FeatureImageIteratorType FeatureImageIteratorType;
  typedef typename Superclass::ConstFeatureIteratorType ConstFeatureIteratorType;

  typedef typename Superclass::ListPixelType          ListPixelType;
  typedef typename Superclass::ListPixelConstIterator ListPixelConstIterator;
  typedef typename Superclass::ListPixelIterator      ListPixelIterator;
  typedef typename Superclass::ListImageType          ListImageType;

protected:
  ScalarChanAndVeseLevelSetFunction():Superclass() {}
  ~ScalarChanAndVeseLevelSetFunction(){}

  void ComputeParameters() ITK_OVERRIDE;

  void UpdateSharedDataParameters() ITK_OVERRIDE;

  ScalarValueType ComputeInternalTerm(const FeaturePixelType & iValue,
                                      const FeatureIndexType & iIdx) ITK_OVERRIDE;

  ScalarValueType ComputeExternalTerm(const FeaturePixelType & iValue,
                                      const FeatureIndexType & iIdx) ITK_OVERRIDE;

  void UpdateSharedDataInsideParameters(const unsigned int & iId,
                                        const FeaturePixelType & iVal, const ScalarValueType & iChange) ITK_OVERRIDE;

  void UpdateSharedDataOutsideParameters(const unsigned int & iId,
                                         const FeaturePixelType & iVal, const ScalarValueType & iChange) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarChanAndVeseLevelSetFunction);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarChanAndVeseLevelSetFunction.hxx"
#endif

#endif
