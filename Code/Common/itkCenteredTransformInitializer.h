/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredTransformInitializer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCenteredTransformInitializer_h
#define __itkCenteredTransformInitializer_h

#include <itkObject.h>

#include <iostream>

namespace itk
{

/** \brief CenteredTransformInitializer is a helper class intended to
 * initialize the center of rotation and the translation of Transforms having
 * the center of rotation among their parameters.
 * 
 * This class is connected to the fixed image, moving image and transform
 * involved in the registration. Two modes of operation are possible:
 * 
 * - Geometrical, 
 * - Center of mass
 *
 * In the first mode, the geometrical center of the moving image is passed as
 * initial center of rotation to the transform and the vector from the center
 * of the  fixed image to the center of the moving image is passed as the
 * initial translation. This mode basically assumes that the anatomical objects
 * to be registered are centered in their respective images. Hence the best
 * initial guess for the registration is the one that superimposes those two
 * centers.
 *
 * In the second mode, the moments of gray level values are computed
 * for both images. The center of mass of the moving image is then
 * used as center of rotation. The vector between the two centers of
 * mass is passes as the initial translation to the transform. This
 * second approach assumes that the moments of the anatomical objects
 * are similar for both images and hence the best initial guess for
 * registration is to superimpose both mass centers.  Note that this
 * assumption will probably not hold in multi-modality registration.
 * 
 * \ingroup Transforms
 */
template < class TTransform,     
           class TFixedImage,
           class TMovingImage > 
class ITK_EXPORT CenteredTransformInitializer : 
            public Object
{
public:
  /** Standard class typedefs. */
  typedef CenteredTransformInitializer     Self;
  typedef Object                           Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( CenteredTransformInitializer, Object );

  /** Type of the transform to initialize */
  typedef TTransform                        TransformType;
  typedef typename TransformType::Pointer   TransformPointer;

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, TransformType::SpaceDimension);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, TransformType::InputSpaceDimension);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, TransformType::OutputSpaceDimension);

  
  /** Image Types to use in the initialization of the transform */
  typedef   TFixedImage              FixedImageType;
  typedef   TMovingImage             MovingImageType;

  typedef   typename FixedImageType::ConstPointer   FixedImagePointer;
  typedef   typename MovingImageType::ConstPointer  MovingImagePointer;



  /** Offset type. */
  typedef typename TransformType::OffsetType  OffsetType;

  /** Point type. */
  typedef typename TransformType::InputPointType   InputPointType;
  
  /** Vector type. */
  typedef typename TransformType::OutputVectorType  OutputVectorType;
  
  /** Set the transform to be initialized */
  itkSetObjectMacro( Transform,   TransformType   );

  /** Set the fixed image used in the registration process */
  itkSetConstObjectMacro( FixedImage,  FixedImageType  );

  /** Set the moving image used in the registration process */
  itkSetConstObjectMacro( MovingImage, MovingImageType );


  /** Initialize the transform using data from the images */
  void InitializeTransform() const;

  /** Select between using the geometrical center of the images or 
      using the center of mass given by the image intensities. */
  void GeometryOn() { m_UseMoments = false; }
  void MomentsOn()  { m_UseMoments = true; }


protected:
  CenteredTransformInitializer() {};
  ~CenteredTransformInitializer(){};

  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  CenteredTransformInitializer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  TransformPointer    m_Transform;

  FixedImagePointer   m_FixedImage;

  MovingImagePointer  m_MovingImage;

  bool                m_UseMoments;

}; //class CenteredTransformInitializer


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredTransformInitializer.txx"
#endif

#endif /* __itkCenteredTransformInitializer_h */
