/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperProcrustes.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationMapperProcrustes_h
#define __itkRegistrationMapperProcrustes_h

#include "itkRegistrationMapper.h"

namespace itk
{
  
/** \class RegistrationMapperProcrustes
 * \brief N-D points from one coodinate system to another
 *
 *  This class is templated over the type of the transformation
 *  used to convert between the coordinate systems
 */

template <class TTransformation, unsigned int NDimension> 
class ITK_EXPORT RegistrationMapperProcrustes : 
    public RegistrationMapper< 
        VectorContainer< unsigned long, Point<double,NDimension> >,
        TTransformation >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationMapperProcrustes  Self;



  /**
   *  Type of the Domain
   */
  typedef VectorContainer< unsigned long, 
                           Point<double,NDimension> >    DomainType;


  /**
   * Standard "Superclass" typedef.
   */
  typedef RegistrationMapper< DomainType, TTransformation > Superclass;



  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;



  /**
   *  Type of the Transformation
   */
  typedef TTransformation       TransformationType;
  

  /**
   *  Pointer type for the Reference 
   */
  typedef typename DomainType::Pointer DomainPointer;


  /**
   *  Pointer type for the Transformation
   */
  typedef typename TransformationType::Pointer TransformationPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationMapperProcrustes, RegistrationMapper);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  


  /**
   * Connect the Domain
   */
   void SetDomain( DomainType * );


  /**
   * Connect the Transformation
   */
   void SetTransformation( TransformationType * );


  /**
   * Transform a point from one coordinate system
   */
   Point<double,NDimension> Transform( const Point<double,NDimension> & );


  
protected:

  DomainPointer            m_Domain;
  TransformationPointer    m_Transformation;

  RegistrationMapperProcrustes();
  virtual ~RegistrationMapperProcrustes() {};
  RegistrationMapperProcrustes(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMapperProcrustes.txx"
#endif

#endif



