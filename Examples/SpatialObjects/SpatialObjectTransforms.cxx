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
// transformations associated with a spatial object.
//
//
// \begin{figure} \center
// \includegraphics[width=0.9\textwidth]{SpatialObjectTransforms}
// \itkcaption[SpatialObject Transformations]{Set of transformations associated
// with a Spatial Object}
// \label{fig:SpatialObjectTransforms}
// \end{figure}
//
// Figure~\ref{fig:SpatialObjectTransforms} shows our set of transformations.
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
  typedef itk::SpatialObject<2>             SpatialObjectType;
  typedef SpatialObjectType::TransformType  TransformType;

  SpatialObjectType::Pointer object1 = SpatialObjectType ::New();
  object1->GetProperty()->SetName("First Object");

  SpatialObjectType::Pointer object2 = SpatialObjectType ::New();
  object2->GetProperty()->SetName("Second Object");
  object1->AddSpatialObject(object2);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Instances of \doxygen{SpatialObject} maintain three transformations
// internally that can be used to compute the position and orientation of
// data and objects.  These transformations are: an IndexToObjectTransform,
// an ObjectToParentTransform, and an ObjectToWorldTransform. As a
// convenience to the user, the global transformation IndexToWorldTransform
// and its inverse, WorldToIndexTransform, are also maintained by the
// class. Methods are provided by SpatialObject to access and manipulate
// these transforms.
//
// The two main transformations, IndexToObjectTransform and
// ObjectToParentTransform, are applied successively. ObjectToParentTransform
// is applied to children.
//
// The IndexToObjectTransform transforms points from the internal data
// coordinate system of the object (typically the indices of the image from
// which the object was defined) to ``physical" space (which accounts for the
// spacing, orientation, and offset of the indices).
//
// The ObjectToParentTransform transforms points from the object-specific
// ``physical" space to the ``physical" space of its parent object. As one can see from the
// figure ~\ref{fig:SpatialObjectTransforms}, the ObjectToParentTransform is composed of two
// transforms: ObjectToNodeTransform and NodeToParentNodeTransform. The ObjectToNodeTransform
// is not applied to the children, but the NodeToParentNodeTransform is. Therefore, if one
// sets the ObjectToParentTransform, the NodeToParentNodeTransform is actually set.
//
// The ObjectToWorldTransform maps points from the reference system of the SpatialObject
// into the global coordinate system. This is useful when the position of the object is known
// only in the global coordinate frame. Note that by setting this transform,
// the ObjectToParent transform is recomputed.
//
// These transformations use the
// \doxygen{FixedCenterOfRotationAffineTransform}. They are created in the
// constructor of the spatial \doxygen{SpatialObject}.
// Software Guide : EndLatex

// Software Guide : BeginLatex
//
// First we define an index scaling factor of 2 for the object2.
// This is done by setting the Scale of the IndexToObjectTransform.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  double scale[2];
  scale[0]=2;
  scale[1]=2;
  object2->GetIndexToObjectTransform()->SetScale(scale);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Next, we apply an offset on the \code{ObjectToParentTransform} of the child object.
// Therefore, object2 is now translated by a vector [4,3] regarding to its
// parent.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  TransformType::OffsetType Object2ToObject1Offset;
  Object2ToObject1Offset[0] = 4;
  Object2ToObject1Offset[1] = 3;
  object2->GetObjectToParentTransform()->SetOffset(Object2ToObject1Offset);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// To realize the previous operations on the transformations, we should
// invoke the \code{ComputeObjectToWorldTransform()} that recomputes all
// dependent transformations.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  object2->ComputeObjectToWorldTransform();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We can now display the ObjectToWorldTransform for both objects.  One
// should notice that the FixedCenterOfRotationAffineTransform derives from
// \doxygen{AffineTransform} and therefore the only valid members of the
// transformation are a Matrix and an Offset. For instance, when we invoke the
// \code{Scale()} method the internal Matrix is recomputed to
// reflect this change.
//
// The FixedCenterOfRotationAffineTransform performs the following
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
// This means that \code{GetScale()} and \code{GetOffset()}
// as well as the \code{GetMatrix()} might not be set to the
// expected value, especially if the transformation results from a
// composition with another transformation since the composition is done
// using the Matrix and the Offset of the affine transformation.
//
// Next, we show the two affine transformations corresponding to the two
// objects.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "object2 IndexToObject Matrix: " << std::endl;
  std::cout << object2->GetIndexToObjectTransform()->GetMatrix() << std::endl;
  std::cout << "object2 IndexToObject Offset: ";
  std::cout << object2->GetIndexToObjectTransform()->GetOffset() << std::endl;
  std::cout << "object2 IndexToWorld Matrix: " << std::endl;
  std::cout << object2->GetIndexToWorldTransform()->GetMatrix() << std::endl;
  std::cout << "object2 IndexToWorld Offset: ";
  std::cout << object2->GetIndexToWorldTransform()->GetOffset() << std::endl;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Then, we decide to translate the first object which is the parent of the
// second by a vector [3,3]. This is still done by setting the offset of the
// ObjectToParentTransform.  This can also be done by setting the
// ObjectToWorldTransform because the first object does not have any parent
// and therefore is attached to the world coordinate frame.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  TransformType::OffsetType Object1ToWorldOffset;
  Object1ToWorldOffset[0] = 3;
  Object1ToWorldOffset[1] = 3;
  object1->GetObjectToParentTransform()->SetOffset(Object1ToWorldOffset);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Next we invoke \code{ComputeObjectToWorldTransform()} on the modified
// object.  This will propagate the transformation through all its children.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  object1->ComputeObjectToWorldTransform();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// \begin{figure} \center
// \includegraphics[width=0.5\textwidth]{SpatialObjectExampleTransforms}
// \itkcaption[SpatialObject Transform Computations]{Physical positions of the
// two objects in the world frame (shapes are merely for illustration
// purposes).}
// \label{fig:SpatialObjectExampleTransforms}
// \end{figure}
//
// Figure~\ref{fig:SpatialObjectExampleTransforms} shows our set of transformations.
//
// Finally, we display the resulting affine transformations.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::cout << "object1 IndexToWorld Matrix: " << std::endl;
  std::cout << object1->GetIndexToWorldTransform()->GetMatrix() << std::endl;
  std::cout << "object1 IndexToWorld Offset: ";
  std::cout << object1->GetIndexToWorldTransform()->GetOffset() << std::endl;
  std::cout << "object2 IndexToWorld Matrix: " << std::endl;
  std::cout << object2->GetIndexToWorldTransform()->GetMatrix() << std::endl;
  std::cout << "object2 IndexToWorld Offset: ";
  std::cout << object2->GetIndexToWorldTransform()->GetOffset() << std::endl;
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
