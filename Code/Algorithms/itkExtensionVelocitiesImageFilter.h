/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtensionVelocitiesImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * number of veclocities to be extended.
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

  /**
   * Standard "Self" typedef
   */
  typedef ExtensionVelocitiesImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ReinitializeLevelSetImageFilter<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ExtensionVelocitiesImageFilter, ReinitializeLevelSetImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * SetDimension enumeration.
   * Although already defined in the superclass, needed here for gcc 2.95.2-5
   * to compile.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  enum { SetDimension = LevelSetType::SetDimension};

  /**
   * AuxVarType typedef support.
   */
  typedef AuxVarTypeDefault<TAuxValue,VAuxDimension,SetDimension> AuxVarType;
  typedef typename AuxVarType::AuxValueType AuxValueType;
  typedef typename AuxVarType::AuxValueVectorType AuxValueVectorType;
  typedef typename AuxVarType::AuxValueContainer AuxValueContainer;
  typedef typename AuxVarType::AuxImageType AuxImageType;
  typedef typename AuxVarType::AuxImagePointer AuxImagePointer;

  /**
   * Get one of the extended velocity images.
   */
  AuxImagePointer GetVelocityImage( unsigned int idx = 0) const
  { if( idx >= VAuxDimension ) return NULL;
    return m_OutputAuxImage[idx]; }

  /**
   * Set one of the input velocity images to be extended.
   */
  void SetVelocityImage(AuxImageType * ptr, unsigned int idx = 0);

protected:
  ExtensionVelocitiesImageFilter();
  ~ExtensionVelocitiesImageFilter(){};
  ExtensionVelocitiesImageFilter(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void AllocateOutput();

  virtual void GenerateInputRequestedRegion();
  virtual void EnlargeOutputRequestedRegion( DataObject * );

private:

  typedef LevelSetVelocityNeighborhoodExtractor<TLevelSet,TAuxValue,VAuxDimension> 
    LocatorType;
  typedef FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension> 
    FastMarchingImageFilterType;

  typename LocatorType::Pointer         m_Locator;
  typename FastMarchingImageFilterType::Pointer    m_Marcher;
  AuxImagePointer                       m_InputAuxImage[VAuxDimension];
  AuxImagePointer                       m_OutputAuxImage[VAuxDimension];

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtensionVelocitiesImageFilter.txx"
#endif

#endif
