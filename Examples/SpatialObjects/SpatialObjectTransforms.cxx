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

// Software Guide : BeginLatex
//
// \index{itk::SpatialObjectTransforms} This example describes the different
// transformations and the Object and World "spaces" associated with a spatial
// object.
//
// Object Space
//
// SpatialObjects have one primary coordinate space that is readily available
// to them, their ObjectSpace. This is the space in which the object was
// inherently defined. No transforms are applied to the points/values that
// get/set into this space. All children of an object are added into this space.
//
// ObjectToParentTransform
//
// SpatialObjects have only one transform that they directly control, their
// ObjectToParentTransform. This transform specifies how an object's space is
// transformed to fit into its parent's ObjectSpace. The
// ObjectToParentTransform is an affine transform, and it is confirmed to be
// invertible when assigned, or the assignment fails.
//
// WorldSpace
//
// WorldSpace is not directly controlled by any SpatialObject except the
// SpatialObject at the top level of the parent-child tree hierarchy of
// Spatial Objects. That is, any SpatialObject that does not have a parent
// exists in a WorldSpace that is defined by applying its
// ObjectToParentTransform to its ObjectSpace.
//
// Several member functions and variables are available to every SpatialObject
// so that they can readily access the WorldSpace in which they exist:
//
// * ComputeObjectToWorldTransform: Composes its ObjectToParentTransform
// with its parent's cached ObjectToObjectToWorldTransform, to determine the
// transform from the object's ObjectSpace to WorldSpace.   This transform is
// always invertible.   This call will cause all children objects to also
// update their cached ObjectToWorldTransform.   This function should be
// called on the top level object once all children object's
// ObjectToParentTransforms have been set.   This function should be called
// on children objects when their ObjectToParentTransforms have been changed.
// Due to the potentially high computational cost of this function, this
// function must be manually called by users.
//
// * GetObjectToWorldTransform: Returns the cached ObjectToWorldTransform.
// It is the user's responsibility to call ComputeObjectToWorldTransform when
// necessary, prior to calling GetObjectToWorldTransform, otherwise the
// returned transform may be "stale."
//
// * SetObjectToWorldTransform: This function updates the object's
// ObjectToParentTransform, using an inverse of the parent's cached
// ObjectToWorldTransform, so that the composition of those transforms
// equal the transform passed to this function.   If an object has no parent,
// its ObjectToParentTransform is equal to its ObjectToWorldTransform.
//
// Software Guide : EndLatex

#include "itkSpatialObject.h"

int main( int , char *[] )
{

// Software Guide : BeginLatex
//
// Like the first example, we create two spatial objects and give them the
// names \code{First Object} and \code{Second Object}, respectively.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  using SpatialObjectType = itk::SpatialObject<2>;
  using TransformType = SpatialObjectType::TransformType;

  SpatialObjectType::Pointer object1 = SpatialObjectType ::New();
  object1->GetProperty().SetName("First Object");

  SpatialObjectType::Pointer object2 = SpatialObjectType ::New();
  object2->GetProperty().SetName("Second Object");
  object1->AddChild(object2);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// First we define a scaling factor of 2 for the object2.
// This is done by setting the Scale of the ObjectToParentTransform.
//
// Note that this scaling would also apply to the children of object2,
// if it had children.  If you wish to scale an object, but not its children,
// then those children aren't actually "children", but they are siblings.  So,
// you should insert a GroupSpatialObject that holds the object and its
// children.  Then you can manipulate the object's transform/scaling
// independent of its siblings in that group, and if you wish to transform
// the object and its simblings, you apply that transform to the group.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  double scale[2];
  scale[0]=2;
  scale[1]=2;
  object2->GetModifiableObjectToParentTransform()->Scale(scale);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Next, we apply an offset on the \code{ObjectToParentTransform} to object1
// which will also cauase a translation of its child, object2.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  TransformType::OffsetType object1Offset;
  object1Offset[0] = 4;
  object1Offset[1] = 3;
  object1->GetModifiableObjectToParentTransform()->SetOffset(object1Offset);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// To realize the previous operations on the transformations, we should
// invoke the \code{ComputeObjectToWorldTransform()} that recomputes all
// dependent transformations.
//
// By calling this function on object1, it will also descend to its children,
// thereby also updating the objectToWorldTransform for object2.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  object1->ComputeObjectToWorldTransform();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We can now display the ObjectToWorldTransform for both objects.  One
// should notice that the only valid members of the Affine
// transformation are a Matrix and an Offset. For instance, when we invoke the
// \code{Scale()} method the internal Matrix is recomputed to
// reflect this change.
//
// The AffineTransform performs the following
// computation
//
//  \begin{equation}
//  X' = R \cdot \left( S \cdot X - C \right) + C + V
//  \end{equation}
//
// Where $R$ is the rotation matrix, $S$ is a scaling factor, $C$ is the center
// of rotation and $V$ is a translation vector or offset.
// Therefore the affine matrix $M$ and the affine offset $T$ are defined as:
//
// \begin{equation}
// M = R \cdot S
// \end{equation}
// \begin{equation}
// T = C + V - R \cdot C
// \end{equation}
//
// This means that \code{Scale()} and \code{GetOffset()}
// as well as the \code{GetMatrix()} might not be set to the
// expected value, especially if the transformation results from a
// composition with another transformation since the composition is done
// using the Matrix and the Offset of the affine transformation.
//
// Next, we show the two affine transformations corresponding to the two
// objects.
//
// First, their ObjectToParent transforms for object2:
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "object2 ObjectToParent Matrix: " << std::endl;
  std::cout << object2->GetObjectToParentTransform()->GetMatrix() << std::endl;
  std::cout << "object2 ObjectToParent Offset: ";
  std::cout << object2->GetObjectToParentTransform()->GetOffset() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Second, their ObjectToWorld transforms that are cached computations, derived
// from the parent-child hierarchy and the composition of the corresponding
// objectToParent transforms, for object2
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "object2 ObjectToWorld Matrix: " << std::endl;
  std::cout << object2->GetObjectToWorldTransform()->GetMatrix() << std::endl;
  std::cout << "object2 ObjectToWorld Offset: ";
  std::cout << object2->GetObjectToWorldTransform()->GetOffset() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We can also update an object's ObjectToParent transform by
// changing its ObjectToWorld transform and then calling
// \code{ComputeObjectToParentTransform()},
// which changes the ObjectToParent so as to achieve the cached
// objectToWorld transform.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  TransformType::OffsetType Object1ToWorldOffset;
  Object1ToWorldOffset[0] = 3;
  Object1ToWorldOffset[1] = 3;
  object2->GetModifiableObjectToWorldTransform()->SetOffset(Object1ToWorldOffset);
  object2->ComputeObjectToParentTransform();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Finally, we display the resulting affine transformations.   First,
// for the ObjectToParent transforms.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "object2 ObjectToParent Matrix: " << std::endl;
  std::cout << object2->GetObjectToParentTransform()->GetMatrix() << std::endl;
  std::cout << "object2 ObjectToParent Offset: ";
  std::cout << object2->GetObjectToParentTransform()->GetOffset() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Second, for the objectToWorld transforms.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "object2 ObjectToWorld Matrix: " << std::endl;
  std::cout << object2->GetObjectToWorldTransform()->GetMatrix() << std::endl;
  std::cout << "object2 ObjectToWorld Offset: ";
  std::cout << object2->GetObjectToWorldTransform()->GetOffset() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Also, as a child is disconnected from its parent, it should not move;
// so its objectToParent transform should be updated to match its
// objectToWorld transform.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  object1->RemoveChild( object2 );

  std::cout << "object2 ObjectToWorld Matrix: " << std::endl;
  std::cout << object2->GetObjectToWorldTransform()->GetMatrix() << std::endl;
  std::cout << "object2 ObjectToWorld Offset: ";
  std::cout << object2->GetObjectToWorldTransform()->GetOffset() << std::endl;
  std::cout << "object2 ObjectToParent Matrix: " << std::endl;
  std::cout << object2->GetObjectToParentTransform()->GetMatrix() << std::endl;
  std::cout << "object2 ObjectToParent Offset: ";
  std::cout << object2->GetObjectToParentTransform()->GetOffset() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The output of this second example looks like the following:
// \small
// \begin{verbatim}
//object2 IndexToObject Matrix:
//2 0
//0 2
//object2 IndexToObject Offset: 0  0
//object2 IndexToWorld Matrix:
//2 0
//0 2
//object2 IndexToWorld Offset: 4  3
//object1 IndexToWorld Matrix:
//1 0
//0 1
//object1 IndexToWorld Offset: 3  3
//object2 IndexToWorld Matrix:
//2 0
//0 2
//object2 IndexToWorld Offset: 7  6
// \end{verbatim}
// \normalsize
//
// Software Guide : EndLatex

  return EXIT_FAILURE;
}
