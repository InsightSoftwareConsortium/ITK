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
#ifndef __itkSpatialObjectTreeNode_h
#define __itkSpatialObjectTreeNode_h

#include "itkTreeNode.h"
#include "itkSpatialObject.h"

namespace itk
{
template< unsigned int TDimension >
class SpatialObject;

/** \class SpatialObjectTreeNode
 * \brief TODO
 * \ingroup ITKSpatialObjects
 */
template< unsigned int TDimension >
class SpatialObjectTreeNode:public TreeNode< SpatialObject< TDimension > * >
{
public:

  /** Standard typedefs */
  typedef SpatialObject< TDimension >                   SpatialObjectType;
  typedef TreeNode< SpatialObject< TDimension > * >     Superclass;
  typedef SpatialObjectTreeNode< TDimension >           Self;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;
  typedef ScalableAffineTransform< double, TDimension > TransformType;
  typedef typename TransformType::Pointer               TransformPointer;
  typedef const TransformType *                         TransformConstPointer;
  typedef typename Superclass::ChildrenListType         ChildrenListType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectTreeNode, TreeNode);

  /** Set the SpatialObject pointer */
  virtual void SetData(SpatialObjectType *data);

  /** Set/Get the NodeToParenNode transform */
  itkSetObjectMacro(NodeToParentNodeTransform, TransformType);
  itkGetConstReferenceObjectMacro(NodeToParentNodeTransform, TransformType);

  /** Set/Get the NodeToParenNode transform */
  itkSetObjectMacro(NodeToWorldTransform, TransformType);
  itkGetConstReferenceObjectMacro(NodeToWorldTransform, TransformType);

  /** Compute the NodeToWorld transform based on the parent */
  void ComputeNodeToWorldTransform();

  /** Return a list of children (the list should be deleted by the user */
#if !defined( CABLE_CONFIGURATION )
  virtual ChildrenListType * GetChildren(unsigned int depth = 0,
                                         char *name = NULL) const;

#endif

protected:

  /** Constructor */
  SpatialObjectTreeNode();
  virtual ~SpatialObjectTreeNode(){}
  void PrintSelf(std::ostream & os, Indent indent) const;

  TransformPointer m_NodeToParentNodeTransform;
  TransformPointer m_NodeToWorldTransform;

private:

  SpatialObjectTreeNode(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectTreeNode.hxx"
#endif

#endif
