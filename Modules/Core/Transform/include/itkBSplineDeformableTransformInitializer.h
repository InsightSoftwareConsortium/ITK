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
#ifndef itkBSplineDeformableTransformInitializer_h
#define itkBSplineDeformableTransformInitializer_h

#include "itkConfigure.h" //Needed to determine value of ITKV3_COMPATIBILITY
#ifdef ITKV3_COMPATIBILITY

#include "itkObject.h"
#include "itkObjectFactory.h"

#include <iostream>

namespace itk
{
/** \class BSplineDeformableTransformInitializer
 * \brief BSplineDeformableTransformInitializer is a helper class intended to
 * initialize the grid parameters of a BSplineDeformableTransform based on the
 * parameters of an image.
 *
 * In the context of image registration, the image to be used are reference will
 * be the fixed image. The BSpline grid will use the fixed image as a base for
 * computing the grid spacing, orientation and origin, among other things.
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Helper class for initializing the grid parameters of
 *  a BSpline deformable transform by using an image as reference"
 *
 *    http://www.insight-journal.org/browse/publication/216
 *    https://hdl.handle.net/1926/1338
 *
 * \ingroup ITKTransform
 */
template< typename TTransform, typename TImage >
class ITK_TEMPLATE_EXPORT BSplineDeformableTransformInitializer:public Object
{
public:
  /** Standard class typedefs. */
  typedef BSplineDeformableTransformInitializer Self;
  typedef Object                                Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDeformableTransformInitializer, Object);

  /** Type of the transform to initialize */
  typedef TTransform TransformType;

  /** Types defined from transform traits */
  typedef typename TransformType::Pointer        TransformPointer;
  typedef typename TransformType::RegionType     TransformRegionType;
  typedef typename TransformRegionType::SizeType TransformSizeType;

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int,
                      TransformType::InputSpaceDimension);

  /** Image Types to use in the initialization of the transform */
  typedef   TImage                           ImageType;
  typedef   typename ImageType::ConstPointer ImagePointer;

  /** Set the transform to be initialized */
  itkSetObjectMacro(Transform,   TransformType);

  /** Set the fixed image used in the registration process */
  itkSetConstObjectMacro(Image,  ImageType);

  /** Set the number of grid nodes that we want to place inside the image. This
   * method will override the settings of any previous call to
   * SetNumberOfGridNodesInsideTheImage().  */
  itkSetMacro(GridSizeInsideTheImage,  TransformSizeType);

  /** Set the number of grid nodes that we want to place inside the image. This
   * number of node is used along one dimension of the image.  Therefore, if
   * you pass the number 5 as argument of this method, in a 3D space, then the
   * total number of grid nodes inside the image will be \$ 5 x 5 x 5 \$ .
   * This method will override the settings of any previous call to
   * SetGridSizeInsideTheImage().  */
  void SetNumberOfGridNodesInsideTheImage(unsigned int numberOfNodes)
  {
    this->m_GridSizeInsideTheImage.Fill(numberOfNodes);
    this->Modified();
  }

  /** Initialize the transform using data from the images */
  virtual void InitializeTransform() const;

protected:
  BSplineDeformableTransformInitializer();
  ~BSplineDeformableTransformInitializer(){}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineDeformableTransformInitializer);

  TransformPointer m_Transform;

  ImagePointer m_Image;

  TransformSizeType m_GridSizeInsideTheImage;

  unsigned int m_NumberOfGridNodesInsideTheImage;
}; //class BSplineDeformableTransformInitializer
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineDeformableTransformInitializer.hxx"
#endif

#else  // def ITKV3_COMPATIBILITY
#error "itkBSplineDeformableTransformInitializer.h should only be included for ITKv3 compatibility. Build with ITKV3_COMPATIBILITY=ON to use this"
#endif // def ITKV3_COMPATIBILITY

#endif /* itkBSplineDeformableTransformInitializer_h */
