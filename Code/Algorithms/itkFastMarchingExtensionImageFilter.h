/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingExtensionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  class TSpeedImage = Image<float,::itk::GetImageDimension<TLevelSet>::ImageDimension>
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
  itkStaticConstMacro(SetDimension, unsigned int,Superclass::SetDimension);

  /** Number of auxiliary variables to be extended. */
  itkStaticConstMacro(AuxDimension, unsigned int,VAuxDimension);

  /** AuxVarType typedef support. */
  typedef AuxVarTypeDefault<TAuxValue,
                            itkGetStaticConstMacro(AuxDimension),
                            itkGetStaticConstMacro(SetDimension)> AuxVarType;
  typedef typename AuxVarType::AuxValueType AuxValueType;
  typedef typename AuxVarType::AuxValueVectorType AuxValueVectorType;
  typedef typename AuxVarType::AuxValueContainer AuxValueContainer;
  typedef typename AuxVarType::AuxImageType AuxImageType;
  typedef typename AuxVarType::AuxImagePointer AuxImagePointer;

  /** Index typedef support. */
  typedef Index<itkGetStaticConstMacro(SetDimension)> IndexType;

  /** Get one of the extended auxiliary variable image. */
  AuxImageType * GetAuxiliaryImage( unsigned int idx );

  /** Set the container auxiliary values at the initial alive points. */
  void SetAuxiliaryAliveValues( AuxValueContainer * values )
  { m_AuxAliveValues = values; }

  /** Get the container of auxiliary values at the initial alive points. */
  AuxValueContainer * GetAuxiliaryAliveValues(void)
  { return m_AuxAliveValues.GetPointer(); }

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
  virtual double UpdateValue( const IndexType & index,
                              const SpeedImageType * speed, LevelSetImageType * output);

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
