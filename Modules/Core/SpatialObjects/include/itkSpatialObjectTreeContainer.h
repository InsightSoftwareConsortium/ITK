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
#ifndef itkSpatialObjectTreeContainer_h
#define itkSpatialObjectTreeContainer_h

#include "itkTreeContainer.h"
#include "itkSpatialObjectTreeNode.h"

namespace itk
{

// Forward reference because of circular dependencies
template< unsigned int TDimension >
class ITK_FORWARD_EXPORT SpatialObject;

/** \class SpatialObjectTreeContainer
 *  \brief Array class with size defined at construction time.
 *
 * This class derives from the vnl_vector<> class.
 * Its size is assigned at construction time (run time) and can
 * not be changed afterwards except by using assignment to another
 * Array.
 *
 * The class is templated over the type of the elements.
 *
 * Template parameters for class Array:
 *
 * - TValue = Element type stored at each location in the array.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKSpatialObjects
 */
template< unsigned int TDimension >
class ITK_TEMPLATE_EXPORT SpatialObjectTreeContainer:
  public TreeContainer< itk::SpatialObject< TDimension > * >
{
public:

  /** Standard typedefs */
  typedef SpatialObject< TDimension >              SpatialObjectType;
  typedef SpatialObjectType *                      SpatialObjectPointer;
  typedef TreeContainer< SpatialObjectPointer >    Superclass;
  typedef SpatialObjectTreeContainer< TDimension > Self;
  typedef SmartPointer< Self >                     Pointer;
  typedef SmartPointer< const Self >               ConstPointer;
  typedef SpatialObjectTreeNode< TDimension >      TreeNodeType;

  /** Iterators typedef */
  typedef typename Superclass::IteratorType IteratorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectTreeContainer, TreeContainer);

  /** Set the root */
  bool SetRoot(const SpatialObjectPointer element) ITK_OVERRIDE;

  bool SetRoot(typename Superclass::TreeNodeType *node) ITK_OVERRIDE
  { return Superclass::SetRoot(node); }

protected:

  SpatialObjectTreeContainer();
  virtual ~SpatialObjectTreeContainer() ITK_OVERRIDE;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectTreeContainer.hxx"
#endif

#endif
