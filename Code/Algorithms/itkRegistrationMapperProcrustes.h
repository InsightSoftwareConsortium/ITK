/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperProcrustes.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegistrationMapperProcrustes_h
#define __itkRegistrationMapperProcrustes_h

#include "itkRegistrationMapper.h"
#include "itkVectorContainer.h"

namespace itk
{
  
/** \class RegistrationMapperProcrustes
 * \brief N-D points from one coodinate system to another
 *
 *  This class is templated over the type of the transformation
 *  used to convert between the coordinate systems
 *
 * \ingroup Functions
 */
template <class TTransformation, unsigned int NDimension> 
class ITK_EXPORT RegistrationMapperProcrustes : 
    public RegistrationMapper< 
        VectorContainer< unsigned long, typename TTransformation::InputPointType >,
        TTransformation >
{
public:
  /** Standard class typedefs. */
  typedef RegistrationMapperProcrustes        Self;

  /**  Type of the transformation. */
  typedef TTransformation                                   TransformationType;
  typedef typename TransformationType::InputPointType       InputPointType;
  typedef typename TransformationType::OutputPointType      OutputPointType;
  typedef typename TransformationType::Pointer              TransformationPointer;

  typedef VectorContainer< unsigned long, InputPointType >  DomainType;
  typedef RegistrationMapper< DomainType, TTransformation > Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(RegistrationMapperProcrustes, RegistrationMapper);

  /**  Pointer type for the reference.  */
  typedef typename DomainType::Pointer DomainPointer;

  /** Connect the domain. */
  void SetDomain( DomainType * );

  /** Connect the transformation. */
  void SetTransformation( TransformationType * );

  /** Transform a point from one coordinate system. */
  OutputPointType Transform( const InputPointType & );
  
protected:
  RegistrationMapperProcrustes();
  virtual ~RegistrationMapperProcrustes() {};

private:
  RegistrationMapperProcrustes(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  DomainPointer            m_Domain;
  TransformationPointer    m_Transformation;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMapperProcrustes.txx"
#endif

#endif



