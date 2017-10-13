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
#ifndef itkBSplineTransformInitializer_h
#define itkBSplineTransformInitializer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class BSplineTransformInitializer
 * \brief BSplineTransformInitializer is a helper class intended to
 * initialize the control point grid such that it has a physically consistent
 * definition.
 * It sets the transform domain origin, physical dimensions and direction from
 * information obtained from the image. It also sets the mesh size if asked
 * to do so by calling SetTransformDomainMeshSize() before calling
 * InitializeTransform().
 *
 * \author Luis Ibanez
 * \author Nick Tustison
 *
 * \ingroup ITKTransform
 */
template<typename TTransform, typename TImage>
class ITK_TEMPLATE_EXPORT BSplineTransformInitializer
: public Object
{
public:

  /** Standard class typedefs. */
  typedef BSplineTransformInitializer Self;
  typedef Object                      Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineTransformInitializer, Object );

  /** Type of the transform to initialize. */
  typedef TTransform TransformType;

  /** Types defined from the input image traits. */
  typedef TImage                                   ImageType;
  typedef typename ImageType::ConstPointer         ImagePointer;
  typedef typename ImageType::IndexType            IndexType;
  typedef typename ImageType::PointType            ImagePointType;
  typedef typename ImagePointType::CoordRepType    ImagePointCoordRepType;

  /** Types defined from transform traits. */
  typedef typename TransformType::Pointer                 TransformPointer;
  typedef typename TransformType::RegionType              RegionType;
  typedef typename RegionType::SizeType                   SizeType;
  typedef typename TransformType::SpacingType             SpacingType;
  typedef typename TransformType::OriginType              OriginType;
  typedef typename TransformType::DirectionType           DirectionType;
  typedef typename TransformType::PhysicalDimensionsType  PhysicalDimensionsType;
  typedef typename TransformType::MeshSizeType            MeshSizeType;
  typedef typename SpacingType::ComponentType             SpacingComponentType;

  /** Dimension of parameters. */
  itkStaticConstMacro( SpaceDimension, unsigned int,
    TransformType::SpaceDimension );

  /** Set/Get the transform to be initialized. */
  itkGetConstObjectMacro( Transform, TransformType );
  itkSetObjectMacro( Transform, TransformType );

  /** Set/Get the image to initialize the domain. */
  itkGetConstObjectMacro( Image, ImageType );
  itkSetConstObjectMacro( Image, ImageType );

  /** Allow the user to set the mesh size of the transform via the initializer,
   * even though the initializer does not do anything with that information.
   * Default size is 1^ImageDimension. */
  itkGetConstMacro( TransformDomainMeshSize, MeshSizeType );
  void SetTransformDomainMeshSize( const MeshSizeType );

  /** Initialize the transform using the specified transformation domain. */
  virtual void InitializeTransform() const;

protected:
  BSplineTransformInitializer();
  ~BSplineTransformInitializer() ITK_OVERRIDE;

  virtual void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineTransformInitializer);

  ImagePointer                    m_Image;
  TransformPointer                m_Transform;

  MeshSizeType                    m_TransformDomainMeshSize;
  bool                            m_SetTransformDomainMeshSizeViaInitializer;

}; //class BSplineTransformInitializer
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineTransformInitializer.hxx"
#endif

#endif /* itkBSplineTransformInitializer_h */
