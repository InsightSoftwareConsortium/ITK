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

#ifndef itkLevelSetQuadEdgeMesh_h
#define itkLevelSetQuadEdgeMesh_h

#include "itkLevelSetBase.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 *  \class LevelSetQuadEdgeMesh
 *  \brief Base class for the surface mesh representation of a level-set function
 *
 *  \tparam TMesh Input mesh type of the level set function
 *  \ingroup ITKLevelSetsv4
 */
template< typename TMesh >
class ITK_TEMPLATE_EXPORT LevelSetQuadEdgeMesh :
    public LevelSetBase<
      typename TMesh::PointIdentifier,
      TMesh::PointDimension,
      typename TMesh::PixelType,
      TMesh >
{
public:
  typedef TMesh                   MeshType;
  typedef typename TMesh::Pointer MeshPointer;

  typedef LevelSetQuadEdgeMesh            Self;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;
  typedef LevelSetBase< typename MeshType::PointIdentifier,
    MeshType::PointDimension,
    typename MeshType::PixelType,
    MeshType                    >         Superclass;

  /** Method for creation through object factory */
  itkNewMacro ( Self );

  /** Run-time type information */
  itkTypeMacro ( LevelSetQuadEdgeMesh, LevelSetBase );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputType       OutputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  itkSetObjectMacro( Mesh, MeshType );
  itkGetModifiableObjectMacro(Mesh, MeshType );

  /** Returns the value of the level set function at a given location iP */
  virtual OutputType Evaluate( const InputType& iP ) const;

  /** Returns the image gradient of the level set function at a given location iP
   * \todo to be implemented */
  virtual GradientType EvaluateGradient( const InputType& iP ) const;

  /** Returns the image hessian of the level set function at a given location iP
   * \todo to be implemented */
  virtual HessianType EvaluateHessian( const InputType& iP ) const;


  /** Returns the value of the level set function at a given location iP
   * as part of the LevelSetDataType*/
  virtual void Evaluate( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the gradient of the level set function at a given location iP
   * as part of the LevelSetDataType
   * \todo to be implemented */
  virtual void EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the Hessian of the level set function at a given location iP
   * as part of the LevelSetDataType
   * \todo to be implemented */
  virtual void EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Initial the level set pointer */
  virtual void Initialize();

  /** Copy level set information from data object */
  virtual void CopyInformation(const DataObject *data);

  /** Graft data object as level set object */
  virtual void Graft( const DataObject* data );

protected:
  LevelSetQuadEdgeMesh();
  virtual ~LevelSetQuadEdgeMesh();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetQuadEdgeMesh);

  MeshPointer m_Mesh;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetQuadEdgeMesh.hxx"
#endif
#endif // itkLevelSetQuadEdgeMesh_h
