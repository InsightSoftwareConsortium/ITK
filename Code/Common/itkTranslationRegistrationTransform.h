/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationRegistrationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTranslationRegistrationTransform_h
#define __itkTranslationRegistrationTransform_h

#include "itkObject.h"
#include "itkTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkVectorContainer.h"
#include "itkAffineTransformation.h"


namespace itk
{
  
/** \class TranslationRegistrationTransform
 * \brief Generic Translation Transformation for a registration method
 *
 * This Class define the generic interface for a Translation Transformation 
 * 
 *
 */

template <class TScalarType,unsigned int NDimensions>
class ITK_EXPORT  TranslationRegistrationTransform : 
    public Transformation<TScalarType,NDimensions>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TranslationRegistrationTransform  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef  Transformation<TScalarType,NDimensions>  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Type of the input parameters
   */
  typedef    VectorContainer<unsigned int,TScalarType>   ParametersType;


 /** 
   * Pointer Type to input parameters
   */
  typedef  typename ParametersType::Pointer  ParametersPointer;


  /** 
   * Affine Transform Type
   */
  typedef AffineTransformation<double,2>      TranslationTransformType;


  /** 
   * Point Type
   */
  typedef  typename TranslationTransformType::PointType     PointType;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(TranslationRegistrationTransform, LightObject);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Transform a Point using the Affine transformation
   */
   PointType Transform(PointType & point );


  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
   void SetParameters(const ParametersPointer &);

   enum { ParametersDimension = NDimensions };

protected:

  TranslationRegistrationTransform();
  virtual ~TranslationRegistrationTransform() {};
  TranslationRegistrationTransform(const Self&);
  const Self & operator=(const Self&);


private:

  TranslationTransformType      m_TranslationTransform;
  ParametersType::Pointer       m_Parameters;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTranslationRegistrationTransform.txx"
#endif

#endif



