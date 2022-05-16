/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
// \index{itk::SpatialObjectTransform}
//
// This example describes the different
// transformations and the Object and World "spaces" associated with a spatial
// object.
//
// \begin{description}
// \item[Object Space]. SpatialObjects have one primary coordinate space
// that is readily available
// to them, their \code{ObjectSpace}. This is the space in which the object
// was inherently defined. No transforms are applied to the points/values that
// get/set into this space. All children of an object are added into this
// space.
//
// \item[ObjectToParentTransform]. SpatialObjects have only one transform
// that they directly control, their
// \code{ObjectToParentTransform}. This transform specifies how an object's
// \code{ObjectSpace} is
// transformed to fit into its parent's \code{ObjectSpace}. The
// \code{ObjectToParentTransform} is an affine transform, and it is confirmed
// to be invertible when assigned, or the assignment fails.
//
// \item[WorldSpace]. \code{WorldSpace} is not directly controlled by any
// SpatialObject except the
// SpatialObject at the top level of the parent-child tree hierarchy of
// Spatial Objects. That is, any SpatialObject that does not have a parent
// exists in a \code{WorldSpace} that is defined by applying its
// \code{ObjectToParentTransform} to its \code{ObjectSpace}.
//
// \end{description}
//
// Several member functions and variables are available to every SpatialObject
// so that they can readily access the WorldSpace in which they exist:
//
// \begin{description}
//
// \item[ProtectedComputeObjectToWorldTransform()]: This function is called
// whenever \code{Update()} is called.  It composes the object's
// \code{ObjectToParentTransform}
// with its parent's cached \code{ObjectToWorldTransform}, to determine the
// transform from the object's \code{ObjectSpace} to \code{WorldSpace}.
// This transform is
// always invertible.   This call will cause all children objects to also
// update their cached \code{ObjectToWorldTransform}.   This function should
// be called on the top level object (via \code{Update()}) once all children
// object's
// \code{ObjectToParentTransform}s have been set.   This function should
// be called
// on children objects when their \code{ObjectToParentTransform}s have been
// changed.
//
// \item[GetObjectToWorldTransform()]: Returns the cached
// \code{ObjectToWorldTransform}.
// It is the user's responsibility to call \code{Update()} (and thereby
// \code{ProtectedComputeObjectToWorldTransform()}) when
// necessary, prior to calling \code{GetObjectToWorldTransform()}, otherwise
// the returned transform may be "stale."
//
// \item[SetObjectToWorldTransform()]: This function updates the object's
// \code{ObjectToParentTransform}, using an inverse of the parent's cached
// \code{ObjectToWorldTransform}, so that the composition of those transforms
// equal the transform passed to this function.   If an object has no parent,
// its \code{ObjectToParentTransform} is equal to its
// \code{ObjectToWorldTransform}.
//
// \end{description}
//
// Software Guide : EndLatex

#include "itkSpatialObject.h"

int
main(int, char *[])
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

  auto object1 = SpatialObjectType::New();
  object1->GetProperty().SetName("First Object");

  auto object2 = SpatialObjectType::New();
  object2->GetProperty().SetName("Second Object");
  object1->AddChild(object2);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // First we define a scaling factor of 2 for the object2.
  // This is done by setting the Scale of the \code{ObjectToParentTransform}.
  //
  // Note that this scaling would also apply to the children of object2,
  // if it had children.  If you wish to scale an object, but not its
  // children, then those children aren't actually ``children'', but they are
  // siblings.  So, you should insert a \code{GroupSpatialObject} that holds
  // both the object and its siblings as children.  Then you can manipulate
  // the object's transform/scaling independent of its siblings in that group,
  // and if you wish to transform the object and its simblings, you apply that
  // transform to the group.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double scale[2];
  scale[0] = 2;
  scale[1] = 2;
  object2->GetModifiableObjectToParentTransform()->Scale(scale);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Next, we apply an offset on the \code{ObjectToParentTransform} to
  // \code{object1}
  // which will also cauase a translation of its child, \code{object2}.
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
  // invoke the \code{Update()} that recomputes all dependent transformations.
  //
  // By calling this function on \code{object1}, it will also descend to its
  // children, thereby also updating the \code{ObjectToWorldTransform} for
  // \code{object2}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  object1->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We can now display the \code{ObjectToWorldTransform} for both objects.
  // One should notice that the only valid members of the Affine
  // transformation are a Matrix and an Offset. For instance, when we invoke
  // the \code{Scale()} method the internal Matrix is recomputed to reflect
  // this change.
  //
  // The AffineTransform performs the following
  // computation
  //
  //  \begin{equation}
  //  X' = R \cdot \left( S \cdot X - C \right) + C + V
  //  \end{equation}
  //
  // Where $R$ is the rotation matrix, $S$ is a scaling factor, $C$ is the
  // center of rotation and $V$ is a translation vector or offset. Therefore
  // the affine matrix $M$ and the affine offset $T$ are defined as:
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
  // First, the \code{ObjectToParentTransform} for \code{object2}:
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "object2 ObjectToParent Matrix: " << std::endl;
  std::cout << object2->GetObjectToParentTransform()->GetMatrix()
            << std::endl;
  std::cout << "object2 ObjectToParent Offset: ";
  std::cout << object2->GetObjectToParentTransform()->GetOffset()
            << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Second, the \code{ObjectToWorldTransform} that is derived
  // from the parent-child hierarchy and the composition of the corresponding
  // \code{ObjectToParentTransform}s, computed by called to
  // \code{Update()}, and cached for efficient subsequent use, for
  // \code{object2}:
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
  // We can also update an object's \code{ObjectToParentTransform} by
  // changing its \code{ObjectToWorldTransform} and then calling
  // \code{ComputeObjectToParentTransform()},
  // which changes the \code{ObjectToParentTransform} so as to achieve the
  // cached \code{ObjectToWorldTransform}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OffsetType Object1ToWorldOffset;
  Object1ToWorldOffset[0] = 3;
  Object1ToWorldOffset[1] = 3;
  object1->GetModifiableObjectToWorldTransform()->SetOffset(
    Object1ToWorldOffset);
  object1->ComputeObjectToParentTransform();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Finally, we display the resulting affine transformations.   First,
  // for the \code{ObjectToParentTransform} for \code{object1}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "object1 ObjectToParent Matrix: " << std::endl;
  std::cout << object1->GetObjectToParentTransform()->GetMatrix()
            << std::endl;
  std::cout << "object1 ObjectToParent Offset: ";
  std::cout << object1->GetObjectToParentTransform()->GetOffset()
            << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Second, for the \code{ObjectToWorldTransform} for \code{object2}.
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
  // so its \code{ObjectToParentTransform} should be updated to match its
  // \code{ObjectToWorldTransform}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  object1->RemoveChild(object2);
  object2->Update();

  std::cout << "object2 ObjectToWorld Matrix: " << std::endl;
  std::cout << object2->GetObjectToWorldTransform()->GetMatrix() << std::endl;
  std::cout << "object2 ObjectToWorld Offset: ";
  std::cout << object2->GetObjectToWorldTransform()->GetOffset() << std::endl;
  std::cout << "object2 ObjectToParent Matrix: " << std::endl;
  std::cout << object2->GetObjectToParentTransform()->GetMatrix()
            << std::endl;
  std::cout << "object2 ObjectToParent Offset: ";
  std::cout << object2->GetObjectToParentTransform()->GetOffset()
            << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The output of this second example looks like the following:
  // \small
  // \begin{verbatim}
  // object2 ObjectToParent Matrix:
  // 2 0
  // 0 2
  // object2 ObjectToParent Offset: 0  0
  // object2 ObjectToWorld Matrix:
  // 2 0
  // 0 2
  // object2 ObjectToWorld Offset: 4  3
  //
  // object1 ObjectToParent Matrix:
  // 1 0
  // 0 1
  // object1 ObjecTParent Offset: 3  3
  // object2 ObjecToWorld Matrix:
  // 2 0
  // 0 2
  // object2 ObjectToWorld Offset: 7  6
  //
  // object2 ObjecToParent Matrix:
  // 2 0
  // 0 2
  // object2 ObjectToParent Offset: 7  6
  // object2 ObjecToWorld Matrix:
  // 2 0
  // 0 2
  // object2 ObjectToWorld Offset: 7  6
  // \end{verbatim}
  // \normalsize
  //
  // Software Guide : EndLatex

  return EXIT_FAILURE;
}
