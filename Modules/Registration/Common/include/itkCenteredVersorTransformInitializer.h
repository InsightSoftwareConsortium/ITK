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
#ifndef itkCenteredVersorTransformInitializer_h
#define itkCenteredVersorTransformInitializer_h

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
 * \ingroup ITKRegistrationCommon
 * \ingroup ITKTransform
 */
template< typename TFixedImage,
          typename TMovingImage >
class ITK_TEMPLATE_EXPORT CenteredVersorTransformInitializer:
  public CenteredTransformInitializer<
    VersorRigid3DTransform< double >,
    TFixedImage, TMovingImage >
{
public:
  /** Standard class typedefs. */
  typedef CenteredVersorTransformInitializer Self;
  typedef CenteredTransformInitializer<
    VersorRigid3DTransform< double >,
    TFixedImage, TMovingImage >          Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredVersorTransformInitializer, Object);

  /** Type of the transform to initialize */
  typedef typename Superclass::TransformType    TransformType;
  typedef typename Superclass::TransformPointer TransformPointer;

  /** Dimension of parameters. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int,
                      Superclass::InputSpaceDimension);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int,
                      Superclass::OutputSpaceDimension);

  /** Image Types to use in the initialization of the transform */
  typedef   typename Superclass::FixedImageType  FixedImageType;
  typedef   typename Superclass::MovingImageType MovingImageType;

  typedef   typename Superclass::FixedImagePointer  FixedImagePointer;
  typedef   typename Superclass::MovingImagePointer MovingImagePointer;

  /** Offset type. */
  typedef typename Superclass::OffsetType OffsetType;

  /** Point type. */
  typedef typename Superclass::InputPointType InputPointType;

  /** Vector type. */
  typedef typename Superclass::OutputVectorType OutputVectorType;

  /** Initialize the transform using data from the images */
  void InitializeTransform() ITK_OVERRIDE;

  /** Enable the use of the principal axes of each image to compute an
   * initial rotation that will align them. */
  itkSetMacro(ComputeRotation, bool);
  itkGetMacro(ComputeRotation, bool);
  itkBooleanMacro(ComputeRotation);

protected:
  CenteredVersorTransformInitializer();
  ~CenteredVersorTransformInitializer() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CenteredVersorTransformInitializer);

  bool m_ComputeRotation;
}; //class CenteredVersorTransformInitializer
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredVersorTransformInitializer.hxx"
#endif

#endif /* itkCenteredVersorTransformInitializer_h */
