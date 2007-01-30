/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredVersorTransformInitializer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCenteredVersorTransformInitializer_h
#define __itkCenteredVersorTransformInitializer_h


#include "itkCenteredTransformInitializer.h"
#include "itkVersorRigid3DTransform.h"


namespace itk
{

/** \class CenteredVersorTransformInitializer
 * \brief CenteredVersorTransformInitializer is a helper class intended to
 * initialize the center of rotation, versor, and translation of the 
 * VersorRigid3DTransform.
 * 
 * This class derived from the CenteredTransformInitializer and uses it in
 * a more constrained context. It always uses the Moments mode, and also
 * takes advantage of the second order moments in order to initialize the
 * Versor representing rotation.
 * 
 * \ingroup Transforms
 */
template < class TFixedImage,
           class TMovingImage > 
class ITK_EXPORT CenteredVersorTransformInitializer : 
                      public CenteredTransformInitializer< 
                                    VersorRigid3DTransform<double>, 
                                    TFixedImage,TMovingImage> 
{
public:
  /** Standard class typedefs. */
  typedef CenteredVersorTransformInitializer      Self;
  typedef CenteredTransformInitializer< 
               VersorRigid3DTransform<double>, 
               TFixedImage,TMovingImage>          Superclass;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( CenteredVersorTransformInitializer, Object );

  /** Type of the transform to initialize */
  typedef typename Superclass::TransformType        TransformType;
  typedef typename Superclass::TransformPointer     TransformPointer;

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, Superclass::SpaceDimension);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 
                                               Superclass::InputSpaceDimension);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 
                                              Superclass::OutputSpaceDimension);

  
  /** Image Types to use in the initialization of the transform */
  typedef   typename Superclass::FixedImageType   FixedImageType;
  typedef   typename Superclass::MovingImageType  MovingImageType;

  typedef   typename Superclass::FixedImagePointer   FixedImagePointer;
  typedef   typename Superclass::MovingImagePointer  MovingImagePointer;

  /** Offset type. */
  typedef typename Superclass::OffsetType  OffsetType;

  /** Point type. */
  typedef typename Superclass::InputPointType   InputPointType;
  
  /** Vector type. */
  typedef typename Superclass::OutputVectorType  OutputVectorType;
  
  /** Initialize the transform using data from the images */
  void InitializeTransform() const;

protected:
  CenteredVersorTransformInitializer();
  ~CenteredVersorTransformInitializer(){};

  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  CenteredVersorTransformInitializer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; //class CenteredVersorTransformInitializer


}  // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_CenteredVersorTransformInitializer(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT CenteredVersorTransformInitializer< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef CenteredVersorTransformInitializer< ITK_TEMPLATE_2 x > \
                               CenteredVersorTransformInitializer##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkCenteredVersorTransformInitializer+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkCenteredVersorTransformInitializer.txx"
#endif

#endif /* __itkCenteredVersorTransformInitializer_h */
