/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineRegistrationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAffineRegistrationTransform_h
#define __itkAffineRegistrationTransform_h

#include "itkObject.h"
#include "itkTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkVectorContainer.h"
#include "itkAffineTransformation.h"


namespace itk
{
  
/** \class AffineRegistrationTransform
 * \brief Generic Affine Transformation for a registration method
 *
 * This Class define the generic interface for an Affine Transformation 
 *
 */

template <class TScalarType,unsigned int NDimensions>
class ITK_EXPORT  AffineRegistrationTransform : 
            public Transformation<TScalarType,NDimensions>
				
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AffineRegistrationTransform  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef Transformation<TScalarType,NDimensions> Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Type of the input parameters
   */
  typedef VectorContainer<unsigned int,TScalarType>  ParametersType;


  /** 
   * Pointer Type to input parameters
   */
  typedef  typename ParametersType::Pointer  ParametersPointer;


  /** 
   * Affine Transform Type
   */
  typedef  AffineTransformation<TScalarType,NDimensions>      AffineTransformType;


  /** 
   * Point Type
   */
  typedef  typename AffineTransformType::PointType     PointType;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(AffineRegistrationTransform, Transform);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Transform a Point using the Affine transformation
   */
  PointType Transform( PointType & point );

  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
  void SetParameters(const ParametersPointer &);

  enum { ParametersDimension = NDimensions * (NDimensions + 1)};


protected:

  AffineRegistrationTransform();
  virtual ~AffineRegistrationTransform() {};
  AffineRegistrationTransform(const Self&);
  const Self & operator=(const Self&);


private:

  AffineTransformType      m_AffineTransform;
  typename ParametersType::Pointer       m_Parameters;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineRegistrationTransform.txx"
#endif

#endif



