/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLandmarkBasedTransformInitializer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLandmarkBasedTransformInitializer_h
#define __itkLandmarkBasedTransformInitializer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkVersorRigid3DTransform.h"
#include "itkRigid2DTransform.h"
#include <vector>
#include <iostream>

namespace itk
{

/** \brief LandmarkBasedTransformInitializer is a helper class intended to
 * The class computes the transform that aligns the fixed and moving images
 * given a set of landmarks. The class is templated over the Transform type.
 *    The transform computed gives the best fit transform that maps the fixed
 * and moving images in a least squares sense. The indices are taken to 
 * correspond, so point 1 in the first set will get mapped close to point 
 * 1 in the second set, etc. An equal number of fixed and moving landmarks 
 * need to be specified using SetFixedLandmarks() SetMovingLandmarks(). 
 * Any number of landmarks may be specified. 
 * Call InitializeTransform() to initialize the transform. 
 * 
 * Currently, the  following transforms are supported by the class:
 *    VersorRigid3DTransform
 *    Rigid2DTansform
 *
 * The class is based in part on Hybrid/vtkLandmarkTransform originally 
 * implemented in python by David G. Gobbi.
 *
 * The solution is based on
 * Berthold K. P. Horn (1987), "Closed-form solution of absolute orientation 
 * using unit quaternions,"
 * http://people.csail.mit.edu/bkph/papers/Absolute_Orientation.pdf
 *
 * 
 * \ingroup Transforms
 */
template < class TTransform,
           class TFixedImage,
           class TMovingImage > 
class ITK_EXPORT LandmarkBasedTransformInitializer : 
            public Object
{
public:
  /** Standard class typedefs. */
  typedef LandmarkBasedTransformInitializer     Self;
  typedef Object                           Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( LandmarkBasedTransformInitializer, Object );

  /** Type of the transform to initialize */
  typedef TTransform                        TransformType;
  typedef typename TransformType::Pointer   TransformPointer;
  
  /** Dimension of parameters. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, TransformType::InputSpaceDimension);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, TransformType::OutputSpaceDimension);

  /** Set the transform to be initialized */
  itkSetObjectMacro( Transform,   TransformType   );

  /** Image Types to use in the initialization of the transform */
  typedef   TFixedImage              FixedImageType;
  typedef   TMovingImage             MovingImageType;

  typedef   typename FixedImageType::ConstPointer   FixedImagePointer;
  typedef   typename MovingImageType::ConstPointer  MovingImagePointer;


  /** Set the fixed image */
  itkSetConstObjectMacro( FixedImage,  FixedImageType  );

  /** Set the moving image */
  itkSetConstObjectMacro( MovingImage, MovingImageType );

  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, FixedImageType::ImageDimension );
      
  /** Convenience typedefs */
  typedef typename TransformType::InputPointType   InputPointType;
  typedef typename TransformType::OutputVectorType  OutputVectorType;
  typedef Point< double, itkGetStaticConstMacro(ImageDimension) > LandmarkPointType;
  typedef std::vector< LandmarkPointType >              LandmarkPointContainer;
  typedef typename
          LandmarkPointContainer::const_iterator        PointsContainerConstIterator;
  typedef typename TransformType::ParametersType ParametersType;
  typedef typename ParametersType::ValueType     ParameterValueType;


    
  /** Set the Fixed landmark point containers */
  void SetFixedLandmarks(LandmarkPointContainer & fixedLandmarks)
    {
    this->m_FixedLandmarks = fixedLandmarks;
    } 
  
  /** Set the Moving landmark point containers */
  void SetMovingLandmarks(LandmarkPointContainer & movingLandmarks)
    {
    this->m_MovingLandmarks = movingLandmarks;
    }
    

  /**  Supported Transform typedefs */
  typedef VersorRigid3DTransform< ParameterValueType >   
                                        VersorRigid3DTransformType;
  typedef Rigid2DTransform< ParameterValueType > Rigid2DTransformType;
  
  /** Initialize the transform from the landmarks */
  virtual void InitializeTransform() ;
  

protected:
  LandmarkBasedTransformInitializer();
  ~LandmarkBasedTransformInitializer(){};

  void PrintSelf(std::ostream &os, Indent indent) const;

  // Supported Transform types
  typedef enum{
    VersorRigid3Dtransform=1,
    Rigid2Dtransfrom,
    Else
  }InputTransformType;

private:
  LandmarkBasedTransformInitializer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  FixedImagePointer   m_FixedImage;
  MovingImagePointer  m_MovingImage;

  LandmarkPointContainer m_FixedLandmarks;
  LandmarkPointContainer m_MovingLandmarks;
 
  TransformPointer    m_Transform;
  
}; //class LandmarkBasedTransformInitializer


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLandmarkBasedTransformInitializer.txx"
#endif

#endif /* __itkLandmarkBasedTransformInitializer_h */
