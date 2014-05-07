/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkLandmarkBasedTransformInitializer_h
#define __itkLandmarkBasedTransformInitializer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkVersorRigid3DTransform.h"
#include "itkRigid2DTransform.h"
#include "itkAffineTransform.h"
#include <vector>
#include <iostream>

namespace itk
{
/** \class LandmarkBasedTransformInitializer
 * \brief LandmarkBasedTransformInitializer is a helper class intended to
 * The class computes the transform that aligns the fixed and moving images
 * given a set of landmarks. The class is templated over the Transform type.
 * The transform computed gives the best fit transform that maps the fixed
 * and moving images in a least squares sense. The indices are taken to
 * correspond, so point 1 in the first set will get mapped close to point
 * 1 in the second set, etc. An equal number of fixed and moving landmarks
 * need to be specified using SetFixedLandmarks() SetMovingLandmarks().
 * Any number of landmarks may be specified.
 * Call InitializeTransform() to initialize the transform.
 *
 * Currently, the  following transforms are supported by the class:
 *    VersorRigid3DTransform
 *    Rigid2DTransform
 *
 * The class is based in part on Hybrid/vtkLandmarkTransform originally
 * implemented in python by David G. Gobbi.
 *
 * The solution is based on
 * Berthold K. P. Horn (1987), "Closed-form solution of absolute orientation
 * using unit quaternions,"
 * http://people.csail.mit.edu/bkph/papers/Absolute_Orientation.pdf
 *
 * The Affine Transform initializer  is based on an algorithm by H
 * Spaeth, and is described in the Insight Journal Article
 * "Affine Transformation for Landmark Based Registration Initializer
 * in ITK" by Kim E.Y., Johnson H., Williams N.
 * available at  http://midasjournal.com/browse/publication/825
 *
 * \ingroup ITKTransform
 *
 * \wiki
 * \wikiexample{Registration/LandmarkBasedTransformInitializer,Rigidly register one image to another using manually specified landmarks}
 * \endwiki
 */
template< typename TTransform,
          typename TFixedImage,
          typename TMovingImage >
class LandmarkBasedTransformInitializer:
  public Object
{
public:
  /** Standard class typedefs. */
  typedef LandmarkBasedTransformInitializer Self;
  typedef Object                            Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LandmarkBasedTransformInitializer, Object);

  /** Type of the transform to initialize */
  typedef TTransform                      TransformType;
  typedef typename TransformType::Pointer TransformPointer;

  /** Dimension of parameters. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, TransformType::InputSpaceDimension);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, TransformType::OutputSpaceDimension);

  /** Set the transform to be initialized */
  itkSetObjectMacro(Transform,   TransformType);

  /** Image Types to use in the initialization of the transform */
  typedef TFixedImage  FixedImageType;
  typedef TMovingImage MovingImageType;


  typedef   typename FixedImageType::ConstPointer  FixedImagePointer;
  typedef   typename MovingImageType::ConstPointer MovingImagePointer;

  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, FixedImageType::ImageDimension);

  /** Convenience typedefs */
  typedef typename TransformType::InputPointType                  InputPointType;
  typedef typename TransformType::OutputVectorType                OutputVectorType;
  typedef Point< double, itkGetStaticConstMacro(ImageDimension) > LandmarkPointType;
  typedef std::vector< LandmarkPointType >                        LandmarkPointContainer;
  typedef typename LandmarkPointContainer::const_iterator         PointsContainerConstIterator;
  typedef typename TransformType::ParametersType                  ParametersType;
  typedef typename ParametersType::ValueType                      ParameterValueType;
  typedef std::vector< double >                                   LandmarkWeightType;
  typedef LandmarkWeightType::const_iterator                      LandmarkWeightConstIterator;

  /** Set the Fixed landmark point containers */
  void SetFixedLandmarks(const LandmarkPointContainer & fixedLandmarks)
  {
    this->m_FixedLandmarks = fixedLandmarks;
  }

  /** Set the Moving landmark point containers */
  void SetMovingLandmarks(const LandmarkPointContainer & movingLandmarks)
  {
    this->m_MovingLandmarks = movingLandmarks;
  }

  /** Set the landmark weight point containers
   *  Weight includes diagonal elements of weight matrix
   */
  void SetLandmarkWeight(LandmarkWeightType & landmarkWeight)
  {
    this->m_LandmarkWeight= landmarkWeight;
  }

  /**  Supported Transform typedefs */
  typedef VersorRigid3DTransform< ParameterValueType >                          VersorRigid3DTransformType;
  typedef Rigid2DTransform< ParameterValueType >                                Rigid2DTransformType;
  typedef AffineTransform< ParameterValueType, FixedImageType::ImageDimension > AffineTransformType;
  /** Initialize the transform from the landmarks */
  virtual void InitializeTransform();

protected:
  LandmarkBasedTransformInitializer();
  ~LandmarkBasedTransformInitializer(){}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  // Supported Transform types
  typedef enum {
    VersorRigid3Dtransform = 1,
    Rigid2Dtransfrom,
    Else
    } InputTransformType;

private:
  LandmarkBasedTransformInitializer(const Self &); //purposely not implemented
  void operator=(const Self &);                    //purposely not implemented


  /** fallback Initializer just sets transform to identity */
  template <typename TTransform2>
    void InternalInitializeTransform(TTransform *);
  /** Initializer for VersorRigid3D */
  void InternalInitializeTransform(VersorRigid3DTransformType *);
  /** Initializer for Rigid2DTransform */
  void InternalInitializeTransform(Rigid2DTransformType *);
  /** Initializer for AffineTransform */
  void InternalInitializeTransform(AffineTransformType *);

  FixedImagePointer  m_FixedImage;
  MovingImagePointer m_MovingImage;

  LandmarkPointContainer m_FixedLandmarks;
  LandmarkPointContainer m_MovingLandmarks;

  TransformPointer m_Transform;
  /** weights for affine landmarks */
  LandmarkWeightType m_LandmarkWeight;

}; //class LandmarkBasedTransformInitializer
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLandmarkBasedTransformInitializer.hxx"
#endif

#endif /* __itkLandmarkBasedTransformInitializer_h */
