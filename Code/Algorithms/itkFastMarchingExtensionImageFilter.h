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
 * \ingroup LevelSetSegmentation 
 */
template <
  class TLevelSet, 
  class TAuxValue,
  unsigned int VAuxDimension = 1,
  class TSpeedImage = Image<float,TLevelSet::ImageDimension>
>
class ITK_EXPORT FastMarchingExtensionImageFilter :
  public FastMarchingImageFilter<TLevelSet,TSpeedImage>
{
public:
  /** Standard class typdedefs. */
  typedef FastMarchingExtensionImageFilter Self;
  typedef FastMarchingImageFilter<TLevelSet,TSpeedImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingExtensionImageFilter, FastMarchingImageFilter);

  /** Inherited typedefs. */
  typedef typename Superclass::LevelSetType  LevelSetType;
  typedef typename Superclass::SpeedImageType SpeedImageType;
  typedef typename Superclass::LevelSetImageType  LevelSetImageType;

  /** The dimension of the level set. */
  enum { SetDimension = Superclass::SetDimension};

  /** Number of auxiliary variables to be extended. */
  enum { AuxDimension = VAuxDimension };

  /** AuxVarType typedef support. */
  typedef AuxVarTypeDefault<TAuxValue,AuxDimension,SetDimension> AuxVarType;
  typedef typename AuxVarType::AuxValueType AuxValueType;
  typedef typename AuxVarType::AuxValueVectorType AuxValueVectorType;
  typedef typename AuxVarType::AuxValueContainer AuxValueContainer;
  typedef typename AuxVarType::AuxImageType AuxImageType;
  typedef typename AuxVarType::AuxImagePointer AuxImagePointer;

  /** Index typedef support. */
  typedef Index<SetDimension> IndexType;

  /** Get one of the extended auxiliary variable image. */
  AuxImagePointer GetAuxiliaryImage( unsigned int idx );

  /** Set the container auxiliary values at the initial alive points. */
  void SetAuxiliaryAliveValues( AuxValueContainer * values )
    { m_AuxAliveValues = values; }

  /** Get the container of auxiliary values at the initial alive points. */
  typename AuxValueContainer::Pointer GetAuxiliaryAliveValues()
    { return m_AuxAliveValues; }

  /** Set the container of auxiliary values at the initial trial points. */
  void SetAuxiliaryTrialValues( AuxValueContainer * values )
    { m_AuxTrialValues = values; }

  /** Get the container of auxiliary values at the initial trial points. */
  typename AuxValueContainer::Pointer GetAuxiliaryTrialValues()
    { return m_AuxTrialValues; }

protected:
  FastMarchingExtensionImageFilter();
  ~FastMarchingExtensionImageFilter(){};
  void PrintSelf( std::ostream& os, Indent indent ) const;

  virtual void Initialize( LevelSetImageType * );
  virtual double UpdateValue( IndexType & index,
    SpeedImageType * speed, LevelSetImageType * output);

  /** Generate the output image meta information */
  virtual void GenerateOutputInformation();
  virtual void EnlargeOutputRequestedRegion( DataObject *output );

private:
  FastMarchingExtensionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  typename AuxValueContainer::Pointer    m_AuxAliveValues;
  typename AuxValueContainer::Pointer    m_AuxTrialValues;
  
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastMarchingExtensionImageFilter.txx"
#endif

#endif
