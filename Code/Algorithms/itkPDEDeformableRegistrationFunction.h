/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPDEDeformableRegistrationFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPDEDeformableRegistrationFunction_h_
#define _itkPDEDeformableRegistrationFunction_h_

#include "itkFiniteDifferenceFunction.h"

namespace itk {

/** \class PDEDeformableRegistrationFunction
 *
 * This is an abstract base class for all PDE functions which drives a
 * deformable registration algorithm. It is used by 
 * PDEDeformationRegistrationFilter subclasses to compute the
 * output deformation field which will map a moving image onto
 * a fixed image.
 *
 * This class is templated over the fixed image type, moving image type
 * and the deformation field type.
 *
 * \sa PDEDeformableRegistrationFilter
 * \ingroup FiniteDifferenceFunctions
 */
template<class TFixedImage, class TMovingImage, class TDeformationField>
class ITK_EXPORT PDEDeformableRegistrationFunction : 
    public FiniteDifferenceFunction<TDeformationField>
{
public:
  /** Standard class typedefs. */
  typedef PDEDeformableRegistrationFunction    Self;
  typedef FiniteDifferenceFunction<TDeformationField>    Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( PDEDeformableRegistrationFunction, 
                FiniteDifferenceFunction );

  /** MovingImage image type. */
  typedef TMovingImage   MovingImageType;
  typedef typename MovingImageType::ConstPointer  MovingImagePointer;

  /** FixedImage image type. */
  typedef TFixedImage    FixedImageType;
  typedef typename FixedImageType::ConstPointer  FixedImagePointer;
  
  /** Deformation field type. */
  typedef TDeformationField    DeformationFieldType;
  typedef typename DeformationFieldType::Pointer   
  DeformationFieldTypePointer;

  /** Set the moving image.  */
  void SetMovingImage( const MovingImageType * ptr )
  { m_MovingImage = ptr; }

  /** Get the moving image. */
  MovingImageType * GetMovingImage(void)
  { return m_MovingImage; }

  /** Set the fixed image. */
  void SetFixedImage( const FixedImageType * ptr )
  { m_FixedImage = ptr; }

  /** Get the fixed image. */
  FixedImageType * GetFixedImage(void)
  { return m_FixedImage; }

protected:
  PDEDeformableRegistrationFunction()
  {
    m_MovingImage = NULL;
    m_FixedImage = NULL;
  }

  ~PDEDeformableRegistrationFunction() {}

  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "MovingImage: ";
    os << m_MovingImage.GetPointer() << std::endl;
    os << indent << "FixedImage: ";
    os << m_FixedImage.GetPointer() << std::endl;

  };

  /** The moving image. */
  MovingImagePointer                m_MovingImage;
  
  /** The fixed image. */
  FixedImagePointer                   m_FixedImage;

private:
  PDEDeformableRegistrationFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};


} // end namespace itk


#endif
