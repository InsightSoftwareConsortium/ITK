/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingExtensionImageFilter.h
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
#ifndef _itkFastMarchingExtensionImageFilter_h
#define _itkFastMarchingExtensionImageFilter_h

#include "itkFastMarchingImageFilter.h"
#include "itkImage.h"
#include "itkLevelSet.h"

namespace itk
{

/** \class FastMarchingExtensionImageFilter
 * \brief Extend auxiliary variables smoothly using Fast Marching.
 *
 * Fast marching can be used to extend auxiliary variables smoothly
 * from the zero level set. Starting from an initial position on the
 * front, this class simultaneously calculate the signed distance and
 * extend a set of auxiliary values.
 *
 * This class is templated over the level set image type, the auxiliary
 * variable type and the number of auxiliary variables to extended. The initial
 * front is specified by two containers: one containing the known points
 * and one containing the trial points. The auxiliary variables on the front
 * are represented by two auxiliary variable containers: one containing
 * the value of the variables at the know points and on containing the
 * value of the variables at the trail points.
 *
 * Implemenation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \sa FastMarchingImageFilter
 * \sa LevelSetTypeDefault
 * \sa AuxVarTypeDefault
 */
template <
  class TLevelSet, 
  class TAuxValue,
  unsigned int VAuxDimension = 1 
>
class ITK_EXPORT FastMarchingExtensionImageFilter :
  public FastMarchingImageFilter<TLevelSet>
{
public:

  /** 
   * Standard "Self" typdedef
   */
  typedef FastMarchingExtensionImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */ 
  typedef FastMarchingImageFilter<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FastMarchingExtensionImageFilter, FastMarchingImageFilter);

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
   * Index typedef support.
   */
  typedef Index<SetDimension> IndexType;

  /**
   * Get one of the extended auxiliary variable image.
   */
  AuxImagePointer GetAuxiliaryImage( unsigned int idx ) const
    { return m_AuxImage[idx]; }

  /**
   * Set the container auxiliary values at the initial alive points.
   */
  void SetAuxiliaryAliveValues( AuxValueContainer * values )
    { m_AuxAliveValues = values; }

  /**
   * Get the container of auxiliary values at the initial alive points.
   */
  typename AuxValueContainer::Pointer GetAuxiliaryAliveValues()
    { return m_AuxAliveValues; }

  /**
   * Set the container of auxiliary values at the initial trial points.
   */
  void SetAuxiliaryTrialValues( AuxValueContainer * values )
    { m_AuxTrialValues = values; }

  /**
   * Get the container of auxiliary values at the initial trial points.
   */
  typename AuxValueContainer::Pointer GetAuxiliaryTrialValues()
    { return m_AuxTrialValues; }


protected:
  FastMarchingExtensionImageFilter();
  ~FastMarchingExtensionImageFilter(){};
  FastMarchingExtensionImageFilter( const Self& ) {};
  void operator= ( const Self& ) {};
  void PrintSelf( std::ostream& os, Indent indent );

  virtual void Initialize();
  virtual double UpdateValue( IndexType & index );
  virtual void GenerateOutputInformation();
  virtual void EnlargeOutputRequestedRegion( DataObject *output );

private:

  typename AuxValueContainer::Pointer    m_AuxAliveValues;
  typename AuxValueContainer::Pointer    m_AuxTrialValues;
  AuxImagePointer                        m_AuxImage[VAuxDimension] ;
  
  // temporary debugging flag
  bool m_DebugOn;

};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastMarchingExtensionImageFilter.txx"
#endif

#endif
