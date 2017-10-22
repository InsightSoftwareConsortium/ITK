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
#ifndef itkExtensionVelocitiesImageFilter_h
#define itkExtensionVelocitiesImageFilter_h

#include "itkLevelSetVelocityNeighborhoodExtractor.h"
#include "itkFastMarchingExtensionImageFilter.h"
#include "itkReinitializeLevelSetImageFilter.h"

namespace itk
{
/** \class ExtensionVelocitiesImageFilter
 *  \brief Extend velocities smoothly from a particular level set.
 *
 * ExtensionVelocitiesImageFilter extends velocities smoothly from a particular
 * level set.
 *
 * This class is templated over the image type which represents
 * the level set, the type of the velocity and the
 * number of velocities to be extended.
 *
 * This class supports narrowbanding. If the input narrowband is provided,
 * the algorithm will only locate the level set within the input narrowband.
 * For the output, the extended velocity is only valid for a distance
 * of OutputNarrowBandwidth / 2 of either side of the level set of interest.
 *
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation
 * \ingroup ITKLevelSets
 */
template<
  typename TLevelSet,
  typename TAuxValue = float,
  unsigned int VAuxDimension = 1
  >
class ITK_TEMPLATE_EXPORT ExtensionVelocitiesImageFilter:
  public ReinitializeLevelSetImageFilter< TLevelSet >
{
public:

  /** Standard class typedefs. */
  typedef ExtensionVelocitiesImageFilter               Self;
  typedef ReinitializeLevelSetImageFilter< TLevelSet > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtensionVelocitiesImageFilter, ReinitializeLevelSetImageFilter);

  /** The type of level set and the pointer type. */
  typedef LevelSetTypeDefault< TLevelSet >            LevelSetType;
  typedef typename LevelSetType::LevelSetPointer      LevelSetPointer;
  typedef typename LevelSetType::LevelSetConstPointer LevelSetConstPointer;
  typedef typename LevelSetType::PixelType            PixelType;
  typedef typename LevelSetType::NodeType             NodeType;
  typedef typename LevelSetType::NodeContainer        NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** The dimension of the level set. */
  itkStaticConstMacro(SetDimension, unsigned int, LevelSetType::SetDimension);

  /** AuxVarType typedef support. */
  typedef AuxVarTypeDefault< TAuxValue, VAuxDimension,
                             itkGetStaticConstMacro(SetDimension) >
  AuxVarType;
  typedef typename AuxVarType::AuxValueType         AuxValueType;
  typedef typename AuxVarType::AuxValueVectorType   AuxValueVectorType;
  typedef typename AuxVarType::AuxValueContainer    AuxValueContainer;
  typedef typename AuxVarType::AuxImageType         AuxImageType;
  typedef typename AuxVarType::AuxImagePointer      AuxImagePointer;
  typedef typename AuxVarType::AuxImageConstPointer AuxImageConstPointer;

  /** Number of velocity images to be extended. */
  itkStaticConstMacro(AuxDimension, unsigned int, VAuxDimension);

  /** Set/Get one of the input velocity images to be extended. */
  void SetInputVelocityImage(const AuxImageType *ptr, unsigned int idx = 0);

  const AuxImageType * GetInputVelocityImage(unsigned int idx = 0);

  /** Get one of the extended velocity images. */
  AuxImageType * GetOutputVelocityImage(unsigned int idx = 0);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( AuxValueHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< TAuxValue > ) );
  itkConceptMacro( LevelSetOStreamWritableCheck,
                   ( Concept::OStreamWritable< PixelType > ) );
  // End concept checking
#endif

protected:
  ExtensionVelocitiesImageFilter();
  ~ExtensionVelocitiesImageFilter() ITK_OVERRIDE {}

  virtual void GenerateDataFull() ITK_OVERRIDE;

  virtual void GenerateDataNarrowBand() ITK_OVERRIDE;

  virtual void AllocateOutput() ITK_OVERRIDE;

  virtual void EnlargeOutputRequestedRegion(DataObject *) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExtensionVelocitiesImageFilter);

  /** Internal typedefs. */
  typedef Image< float, itkGetStaticConstMacro(SetDimension) > SpeedImageType;

  typedef LevelSetVelocityNeighborhoodExtractor< TLevelSet, TAuxValue, VAuxDimension > LocatorType;
  typedef FastMarchingExtensionImageFilter< TLevelSet, TAuxValue, VAuxDimension,
                                            SpeedImageType > FastMarchingImageFilterType;

  typename LocatorType::Pointer m_Locator;

  typename FastMarchingImageFilterType::Pointer m_Marcher;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtensionVelocitiesImageFilter.hxx"
#endif

#endif
