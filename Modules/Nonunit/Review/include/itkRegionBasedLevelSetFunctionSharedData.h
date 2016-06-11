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
#ifndef itkRegionBasedLevelSetFunctionSharedData_h
#define itkRegionBasedLevelSetFunctionSharedData_h

#include "itkLightObject.h"

#include "itkVector.h"
#include "itkListSample.h"
#include "itkKdTreeGenerator.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class RegionBasedLevelSetFunctionSharedData
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
template< typename TInputImage, typename TFeatureImage, typename TSingleData >
class RegionBasedLevelSetFunctionSharedData:public LightObject
{
public:

  typedef RegionBasedLevelSetFunctionSharedData Self;
  typedef LightObject                           Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, TFeatureImage::ImageDimension);

  itkTypeMacro(RegionBasedLevelSetFunctionSharedData, LightObject);

  typedef TInputImage                             InputImageType;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::PixelType      InputPixelType;
  typedef typename InputImageType::RegionType     InputRegionType;
  typedef typename InputImageType::SizeType       InputSizeType;
  typedef typename InputSizeType::SizeValueType   InputSizeValueType;
  typedef typename InputImageType::SpacingType    InputSpacingType;
  typedef typename InputImageType::IndexType      InputIndexType;
  typedef typename InputIndexType::IndexValueType InputIndexValueType;
  typedef typename InputImageType::PointType      InputPointType;

  typedef TFeatureImage                           FeatureImageType;
  typedef typename FeatureImageType::Pointer      FeatureImagePointer;
  typedef typename FeatureImageType::ConstPointer FeatureImageConstPointer;
  typedef typename FeatureImageType::PixelType    FeaturePixelType;
  typedef typename FeatureImageType::RegionType   FeatureRegionType;
  typedef typename FeatureImageType::SizeType     FeatureSizeType;
  typedef typename FeatureSizeType::SizeValueType FeatureSizeValueType;
  typedef typename FeatureImageType::SpacingType  FeatureSpacingType;
  typedef typename FeatureImageType::IndexType    FeatureIndexType;
  typedef typename FeatureImageType::PointType    FeaturePointType;

  typedef std::list< unsigned int > ListPixelType;
  typedef Image< ListPixelType, itkGetStaticConstMacro(ImageDimension) >
  ListImageType;
  typedef typename ListImageType::Pointer               ListImagePointer;
  typedef typename ListImageType::ConstPointer          ListImageConstPointer;
  typedef typename ListImageType::RegionType            ListRegionType;
  typedef typename ListImageType::SizeType              ListSizeType;
  typedef typename ListSizeType::SizeValueType          ListSizeValueType;
  typedef typename ListImageType::SpacingType           ListSpacingType;
  typedef typename ListImageType::IndexType             ListIndexType;
  typedef typename ListIndexType::IndexValueType        ListIndexValueType;
  typedef typename ListImageType::PointType             ListPointType;
  typedef ImageRegionIteratorWithIndex< ListImageType > ListIteratorType;

  typedef Vector< float, itkGetStaticConstMacro(ImageDimension) >
  CentroidVectorType;
  typedef itk::Statistics::ListSample< CentroidVectorType > SampleType;
  typedef itk::Statistics::KdTreeGenerator< SampleType >    TreeGeneratorType;
  typedef typename TreeGeneratorType::Pointer               TreePointer;
  typedef typename TreeGeneratorType::KdTreeType            TreeType;
  typedef typename TreeType::Pointer                        KdTreePointer;

  typedef TSingleData                                  LevelSetDataType;
  typedef typename LevelSetDataType::Pointer           LevelSetDataPointer;
  typedef std::vector< LevelSetDataPointer >           LevelSetDataPointerVector;
  typedef typename LevelSetDataPointerVector::iterator LevelSetDataPointerVectorIterator;

  void SetFunctionCount(const unsigned int & n)
  {
    this->m_FunctionCount = n;
    this->m_LevelSetDataPointerVector.resize(n, ITK_NULLPTR);

    LevelSetDataPointerVectorIterator it = m_LevelSetDataPointerVector.begin();
    LevelSetDataPointerVectorIterator end = m_LevelSetDataPointerVector.end();
    while ( it != end )
      {
      ( *it ) = LevelSetDataType::New();
      it++;
      }
  }

  void SetNumberOfNeighbors(const unsigned int & n)
  {
    this->m_NumberOfNeighbors = n;
  }

  void CreateHeavisideFunctionOfLevelSetImage(const unsigned int & j, const InputImageType *image)
  {
    m_LevelSetDataPointerVector[j]->CreateHeavisideFunctionOfLevelSetImage(image);
  }

  void SetKdTree(KdTreePointer kdtree)
  {
    this->m_KdTree = kdtree;
  }

  void AllocateListImage(const FeatureImageType *featureImage)
  {
    this->m_NearestNeighborListImage = ListImageType::New();
    this->m_NearestNeighborListImage->CopyInformation(featureImage);
    this->m_NearestNeighborListImage->SetRegions( featureImage->GetLargestPossibleRegion() );
    this->m_NearestNeighborListImage->Allocate();
  }

  virtual void PopulateListImage() = 0;

  LevelSetDataPointerVector m_LevelSetDataPointerVector;

  unsigned int     m_FunctionCount;
  unsigned int     m_NumberOfNeighbors;
  ListImagePointer m_NearestNeighborListImage;
  KdTreePointer    m_KdTree;

protected:
  RegionBasedLevelSetFunctionSharedData():m_NumberOfNeighbors(6), m_KdTree(ITK_NULLPTR){}
  ~RegionBasedLevelSetFunctionSharedData(){}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionBasedLevelSetFunctionSharedData);
};
} //end namespace itk

#endif
