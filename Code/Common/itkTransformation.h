/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTransformation_h
#define __itkTransformation_h

#include "itkObject.h"
#include "itkPoint.h"


namespace itk
{
  
/** \class Transformation
 * \brief Generic concept of transformation methods
 *
 * This Abstract Class define the generic interface for a transformation. 
 * It contains a Transform method.
 *
 */

template <class TScalarType,int NDimensions>
class ITK_EXPORT  Transformation : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Transformation  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Self, Superclass);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Apply Transformation
   */
  typedef itk::Point<TScalarType, NDimensions> PointType;
  virtual PointType Transform(PointType &) = 0;


  
   Transformation();
   virtual ~Transformation() {};
  

protected:
  
  Transformation(const Self&);
  const Self & operator=(const Self&);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformation.txx"
#endif

#endif



