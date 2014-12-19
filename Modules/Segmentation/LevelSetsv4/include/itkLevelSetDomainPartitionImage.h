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
#ifndef itkLevelSetDomainPartitionImage_h
#define itkLevelSetDomainPartitionImage_h

#include "itkLevelSetDomainPartitionBase.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class LevelSetDomainPartitionImage
 *
 * \brief Helper class used to partition domain and efficiently compute overlap.
 * \ingroup ITKLevelSetsv4
 */
template< typename TImage >
class LevelSetDomainPartitionImage : public LevelSetDomainPartitionBase< TImage >
{
public:

  typedef LevelSetDomainPartitionImage          Self;
  typedef LevelSetDomainPartitionBase< TImage > Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro( LevelSetDomainPartitionImage, LevelSetDomainPartitionBase );

  typedef TImage                             ImageType;
  typedef typename ImageType::Pointer        ImagePointer;
  typedef typename ImageType::ConstPointer   ImageConstPointer;
  typedef typename ImageType::PixelType      PixelType;
  typedef typename ImageType::RegionType     RegionType;
  typedef typename ImageType::SizeType       SizeType;
  typedef typename SizeType::SizeValueType   SizeValueType;
  typedef typename ImageType::SpacingType    SpacingType;
  typedef typename ImageType::IndexType      IndexType;
  typedef typename IndexType::IndexValueType IndexValueType;
  typedef typename ImageType::PointType      PointType;

  typedef typename Superclass::IdentifierListType IdentifierListType;

  typedef Image< IdentifierListType, ImageDimension >   ListImageType;
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

  typedef std::vector< RegionType >                     LevelSetDomainRegionVectorType;

  /** Set the input image that will be used to compute an image with the list
   * of level sets domain overlaps. */
  itkSetConstObjectMacro( Image, ImageType );
  itkGetConstObjectMacro(Image, ImageType );

  /** Get the image with the list of level set domains. */
  itkGetModifiableObjectMacro(ListDomain, ListImageType );

  void SetLevelSetDomainRegionVector( const LevelSetDomainRegionVectorType& domain );
  const LevelSetDomainRegionVectorType& GetLevelSetDomainRegionVector() const;

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  virtual void PopulateListDomain();

protected:
  LevelSetDomainPartitionImage();
  virtual ~LevelSetDomainPartitionImage();

  /** Allocate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void AllocateListDomain();

  ImageConstPointer               m_Image;
  ListImagePointer                m_ListDomain;
  LevelSetDomainRegionVectorType  m_LevelSetDomainRegionVector;

private:
  LevelSetDomainPartitionImage(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetDomainPartitionImage.hxx"
#endif

#endif
