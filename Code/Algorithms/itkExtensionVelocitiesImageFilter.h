/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtensionVelocitiesImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkExtensionVelocitiesImageFilter_h
#define _itkExtensionVelocitiesImageFilter_h

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
 */
template <
  class TLevelSet,
  class TAuxValue = float,
  unsigned int VAuxDimension = 1
  >
class ITK_EXPORT ExtensionVelocitiesImageFilter :
    public ReinitializeLevelSetImageFilter<TLevelSet>
{
public:

  /** Standard class typedefs. */
  typedef ExtensionVelocitiesImageFilter Self;
  typedef ReinitializeLevelSetImageFilter<TLevelSet> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtensionVelocitiesImageFilter, ReinitializeLevelSetImageFilter);

  /** The type of level set and the pointer type. */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::LevelSetConstPointer  LevelSetConstPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** The dimension of the level set. */
  itkStaticConstMacro(SetDimension, unsigned int,LevelSetType::SetDimension);

  /** AuxVarType typedef support. */
  typedef AuxVarTypeDefault<TAuxValue,VAuxDimension,
                            itkGetStaticConstMacro(SetDimension)> AuxVarType;
  typedef typename AuxVarType::AuxValueType AuxValueType;
  typedef typename AuxVarType::AuxValueVectorType AuxValueVectorType;
  typedef typename AuxVarType::AuxValueContainer AuxValueContainer;
  typedef typename AuxVarType::AuxImageType AuxImageType;
  typedef typename AuxVarType::AuxImagePointer AuxImagePointer;
  typedef typename AuxVarType::AuxImageConstPointer AuxImageConstPointer;

  /** Number of velocity images to be extended. */
  itkStaticConstMacro(AuxDimension, unsigned int,VAuxDimension);

  /** Set/Get one of the input velocity images to be extended. */
  void SetInputVelocityImage(const AuxImageType * ptr, unsigned int idx = 0);
  const AuxImageType * GetInputVelocityImage(unsigned int idx = 0);

  /** Get one of the extended velocity images. */
  AuxImageType * GetOutputVelocityImage( unsigned int idx = 0 );

protected:
  ExtensionVelocitiesImageFilter();
  ~ExtensionVelocitiesImageFilter(){};

  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void AllocateOutput();

  virtual void EnlargeOutputRequestedRegion( DataObject * );

private:
  ExtensionVelocitiesImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Internal typedefs. SpeedImageType defined to work around the Borland
   * compiler's improper handling of default template parameters that use
   * dependent non-type templates. */
  typedef Image<float, itkGetStaticConstMacro(SetDimension) > SpeedImageType;
  typedef LevelSetVelocityNeighborhoodExtractor<TLevelSet,TAuxValue,VAuxDimension> 
  LocatorType;
  typedef FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,SpeedImageType> 
  FastMarchingImageFilterType;

  typename LocatorType::Pointer                    m_Locator;
  typename FastMarchingImageFilterType::Pointer    m_Marcher;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtensionVelocitiesImageFilter.txx"
#endif

#endif
