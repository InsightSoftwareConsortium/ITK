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
#ifndef itkScalarRegionBasedLevelSetFunction_h
#define itkScalarRegionBasedLevelSetFunction_h

#include "itkRegionBasedLevelSetFunction.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class ScalarRegionBasedLevelSetFunction
 *
 * \brief LevelSet function that computes a speed image based on regional integrals
 *
 * This class implements a level set function that computes the speed image by
 * integrating values on the image domain. NOTE: The convention followed is
 * inside of the level-set function is negative and outside is positive.
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
template< typename TInputImage, typename TFeatureImage, typename TSharedData >
class ITK_TEMPLATE_EXPORT ScalarRegionBasedLevelSetFunction:
  public RegionBasedLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
{
public:
  typedef ScalarRegionBasedLevelSetFunction                                      Self;
  typedef RegionBasedLevelSetFunction< TInputImage, TFeatureImage, TSharedData > Superclass;
  typedef SmartPointer< Self >                                                   Pointer;
  typedef SmartPointer< const Self >                                             ConstPointer;

  // itkNewMacro() is not provided since this is an abstract class.

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarRegionBasedLevelSetFunction, RegionBasedLevelSetFunction);

  itkStaticConstMacro(ImageDimension, unsigned int, TFeatureImage::ImageDimension);

  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::InputPixelType         InputPixelType;
  typedef typename Superclass::InputIndexType         InputIndexType;
  typedef typename Superclass::InputIndexValueType    InputIndexValueType;
  typedef typename Superclass::InputSizeType          InputSizeType;
  typedef typename Superclass::InputSizeValueType     InputSizeValueType;
  typedef typename Superclass::InputRegionType        InputRegionType;
  typedef typename Superclass::InputPointType         InputPointType;

  typedef typename Superclass::FeatureImageType   FeatureImageType;
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

  typedef ImageRegionIteratorWithIndex< InputImageType >      ImageIteratorType;
  typedef ImageRegionConstIteratorWithIndex< InputImageType > ConstImageIteratorType;
  typedef ImageRegionIteratorWithIndex< FeatureImageType >    FeatureImageIteratorType;
  typedef ImageRegionConstIterator< FeatureImageType >        ConstFeatureIteratorType;

  typedef std::list< unsigned int >              ListPixelType;
  typedef typename ListPixelType::const_iterator ListPixelConstIterator;
  typedef typename ListPixelType::iterator       ListPixelIterator;
  typedef Image< ListPixelType, itkGetStaticConstMacro(ImageDimension) >
  ListImageType;

  /** \brief Performs the narrow-band update of the Heaviside function for each
  voxel. The characteristic function of each region is recomputed (note the
  shared data which contains information from the other level sets). Using the
  new H values, the previous c_i are updated. */
  void UpdatePixel(const unsigned int & idx,
                   NeighborhoodIterator< TInputImage > & iterator,
                   InputPixelType & newValue,
                   bool & status);

protected:
  ScalarRegionBasedLevelSetFunction():Superclass(){}
  ~ScalarRegionBasedLevelSetFunction(){}

  ScalarValueType ComputeOverlapParameters(const FeatureIndexType & featIndex,
                                           ScalarValueType & product) ITK_OVERRIDE;

  // update the background and foreground constants for pixel updates
  // Called only when sparse filters are used to prevent iteration through the
  // entire image
  virtual void UpdateSharedDataInsideParameters(const unsigned int & iId,
                                                const FeaturePixelType & iVal, const ScalarValueType & iChange) = 0;

  virtual void UpdateSharedDataOutsideParameters(const unsigned int & iId,
                                                 const FeaturePixelType & iVal, const ScalarValueType & iChange) = 0;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarRegionBasedLevelSetFunction);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarRegionBasedLevelSetFunction.hxx"
#endif

#endif
