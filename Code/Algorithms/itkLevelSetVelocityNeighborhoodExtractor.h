/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetVelocityNeighborhoodExtractor.h
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
#ifndef _itkLevelSetVelocityNeighborhoodExtractor_h
#define _itkLevelSetVelocityNeighborhoodExtractor_h

#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkLevelSet.h"
#include "itkIndex.h"

namespace itk
{

/** \class LevelSetVelocityNeighborhoodExtractor
 * \brief Locate pixels of a particular level set.
 *
 * LevelSetNeighborhoodExtractor locates a particular level set in the input level
 * set. Specifically, the method Locate() fills 
 * two containers: one containing pixels immediately 
 * inside the contour defined by the level set and the other
 * containing pixels immediately outside.
 * For each located pixel, an estimated distance to the
 * particular level set is also calculated.
 *
 * This class also calculates the values of auxiliary variables
 * at located pixel.
 *
 * This class is templated over the image type representing
 * the level set, the type of the auxiliary variables and the
 * number of auxiliary variables.
 *
 * Implemenation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation 
 *
 */
template <
  class TLevelSet,
  class TAuxValue,
  unsigned int VAuxDimension = 1
>
class ITK_EXPORT LevelSetVelocityNeighborhoodExtractor :
  public LevelSetNeighborhoodExtractor<TLevelSet>
{
public:
  /** 
   * Standard "Self" typdedef
   */
  typedef LevelSetVelocityNeighborhoodExtractor Self;

  /**
   * Standard "Superclass" typedef
   */ 
  typedef LevelSetNeighborhoodExtractor<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(LevelSetVelocityNeighborhoodExtractor, LevelSetNeighborhoodExtractor);

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
   * Index typedef support
   */
  typedef Index<SetDimension> Index;

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
   * Set the auxiliary images
   */
  void SetAuxImage( AuxImageType * ptr, unsigned int idx = 0 )
  { 
    if( idx < VAuxDimension )
      {
      m_AuxImage[idx] = ptr;
      }
  }

  /**
   * Get the container of auxiliary values associated with the inside points
   */
  typename AuxValueContainer::Pointer GetAuxInsideValues()
    { return m_AuxInsideValues; }

  /**
   * Get the container of auxiliary values associate with the outside points
   */
  typename AuxValueContainer::Pointer GetAuxOutsideValues()
    { return m_AuxOutsideValues; }


protected:
  LevelSetVelocityNeighborhoodExtractor();
  ~LevelSetVelocityNeighborhoodExtractor(){};
  LevelSetVelocityNeighborhoodExtractor( const Self& ) {};
  void operator= ( const Self& ) {};
  void PrintSelf( std::ostream& os, Indent indent ) const;

  virtual void Initialize();
  virtual double CalculateDistance( Index & index );

  void GenerateDate();

private:

  typename AuxValueContainer::Pointer    m_AuxInsideValues;
  typename AuxValueContainer::Pointer    m_AuxOutsideValues;
  AuxImagePointer                        m_AuxImage[VAuxDimension];

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetVelocityNeighborhoodExtractor.txx"
#endif

#endif


