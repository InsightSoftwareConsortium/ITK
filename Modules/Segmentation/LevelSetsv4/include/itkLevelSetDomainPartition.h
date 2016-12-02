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
#ifndef itkLevelSetDomainPartition_h
#define itkLevelSetDomainPartition_h

#include "itkLevelSetDomainPartitionBase.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class LevelSetDomainPartition
 *
 * \brief Helper class used to share data in the ScalarChanAndVeseLevelSetFunction.
 * \ingroup ITKLevelSetsv4
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT LevelSetDomainPartition:
  public LevelSetDomainPartitionBase< TImage >
{
public:

  typedef LevelSetDomainPartition                 Self;
  typedef LevelSetDomainPartitionBase< TImage >   Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(LevelSetDomainPartition, LevelSetDomainPartitionBase );

  typedef TImage                                ImageType;
  typedef typename ImageType::Pointer           ImagePointer;

  typedef typename Superclass::ListPixelType    ListPixelType;

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void PopulateListImage();

protected:
  LevelSetDomainPartition();
  ~LevelSetDomainPartition();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetDomainPartition);
};
} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetDomainPartition.hxx"
#endif

#endif
