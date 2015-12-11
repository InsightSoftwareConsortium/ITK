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
// \index{itk::Statistics::KdTree}
// \index{itk::Statistics::KdTree\-Generator}
// \index{itk::Statistics::Weighted\-Centroid\-KdTree\-Generator}
//
// The \subdoxygen{Statistics}{KdTree} implements a data structure that
// separates samples in a $k$-dimension space.  The \code{std::vector} class
// is used here as the container for the measurement vectors from a sample.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkMath.h"
#include "itkListSample.h"
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "itkEuclideanDistanceMetric.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // We define the measurement vector type and instantiate a
  // \subdoxygen{Statistics}{ListSample} object, and then put 1000
  // measurement vectors in the object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 2 > MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( 2 );

  MeasurementVectorType mv;
  for (unsigned int i = 0; i < 1000; ++i )
    {
    mv[0] = (float) i;
    mv[1] = (float) ((1000 - i) / 2 );
    sample->PushBack( mv );
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The following code snippet shows how to create two KdTree objects. The
  // first object \subdoxygen{Statistics}{KdTreeGenerator} has a minimal set
  // of information (partition dimension, partition value, and pointers to
  // the left and right child nodes). The second tree from the
  // \subdoxygen{Statistics}{WeightedCentroidKdTreeGenerator} has additional
  // information such as the number of children under each node, and the
  // vector sum of the measurement vectors belonging to children of a
  // particular node.  WeightedCentroidKdTreeGenerator and the resulting k-d
  // tree structure were implemented based on the description given in the
  // paper by Kanungo et al \cite{Kanungo2000}.
  //
  // The instantiation and input variables are exactly the same for both
  // tree generators. Using the \code{SetSample()} method we plug-in the
  // source sample. The bucket size input specifies the limit on the
  // maximum number of measurement vectors that can be stored in a
  // terminal (leaf) node. A bigger bucket size results in a smaller number of
  // nodes in a tree. It also affects the efficiency of search. With
  // many small leaf nodes, we might experience slower search
  // performance because of excessive boundary comparisons.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::KdTreeGenerator< SampleType > TreeGeneratorType;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  treeGenerator->SetSample( sample );
  treeGenerator->SetBucketSize( 16 );
  treeGenerator->Update();

  typedef itk::Statistics::WeightedCentroidKdTreeGenerator< SampleType >
    CentroidTreeGeneratorType;

  CentroidTreeGeneratorType::Pointer centroidTreeGenerator =
                                         CentroidTreeGeneratorType::New();

  centroidTreeGenerator->SetSample( sample );
  centroidTreeGenerator->SetBucketSize( 16 );
  centroidTreeGenerator->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // After the generation step, we can get the pointer to the kd-tree
  // from the generator by calling the \code{GetOutput()} method. To
  // traverse a kd-tree, we have to use the \code{GetRoot()} method. The
  // method will return the root node of the tree. Every node in a tree
  // can have its left and/or right child node. To get the child node,
  // we call the \code{Left()} or the \code{Right()} method of a node
  // (these methods do not belong to the kd-tree but to the nodes).
  //
  // We can get other information about a node by calling the methods
  // described below in addition to the child node pointers.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef TreeGeneratorType::KdTreeType TreeType;
  typedef TreeType::KdTreeNodeType      NodeType;

  TreeType::Pointer tree = treeGenerator->GetOutput();
  TreeType::Pointer centroidTree = centroidTreeGenerator->GetOutput();

  NodeType* root = tree->GetRoot();

  if ( root->IsTerminal() )
    {
    std::cout << "Root node is a terminal node." << std::endl;
    }
  else
    {
    std::cout << "Root node is not a terminal node." << std::endl;
    }

  unsigned int partitionDimension;
  float partitionValue;
  root->GetParameters( partitionDimension, partitionValue);
  std::cout << "Dimension chosen to split the space = "
            << partitionDimension << std::endl;
  std::cout << "Split point on the partition dimension = "
            << partitionValue << std::endl;

  std::cout << "Address of the left chile of the root node = "
            << root->Left() << std::endl;

  std::cout << "Address of the right chile of the root node = "
            << root->Right() << std::endl;

  root = centroidTree->GetRoot();
  std::cout << "Number of the measurement vectors under the root node"
            << " in the tree hierarchy = " << root->Size() << std::endl;

  NodeType::CentroidType centroid;
  root->GetWeightedCentroid( centroid );
  std::cout << "Sum of the measurement vectors under the root node = "
            << centroid << std::endl;

  std::cout << "Number of the measurement vectors under the left child"
            << " of the root node = " << root->Left()->Size() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // In the following code snippet, we query the three nearest neighbors of
  // the \code{queryPoint} on the two tree. The results and procedures are
  // exactly the same for both. First we define the point from which distances
  // will be measured.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeasurementVectorType queryPoint;
  queryPoint[0] = 10.0;
  queryPoint[1] = 7.0;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Then we instantiate the type of a distance metric, create an object of
  // this type and set the origin of coordinates for measuring distances.
  // The \code{GetMeasurementVectorSize()} method returns the length of
  // each measurement vector stored in the sample.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::EuclideanDistanceMetric< MeasurementVectorType >
    DistanceMetricType;
  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();

  DistanceMetricType::OriginType origin( 2 );
  for ( unsigned int i = 0; i < sample->GetMeasurementVectorSize(); ++i )
    {
    origin[i] = queryPoint[i];
    }
  distanceMetric->SetOrigin( origin );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We can now set the number of neighbors to be located and the point
  // coordinates to be used as a reference system.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int numberOfNeighbors = 3;
  TreeType::InstanceIdentifierVectorType neighbors;
  tree->Search( queryPoint, numberOfNeighbors, neighbors);

  std::cout <<
    "\n*** kd-tree knn search result using an Euclidean distance metric:"
    << std::endl
    << "query point = [" << queryPoint << "]" << std::endl
    << "k = " << numberOfNeighbors << std::endl;
  std::cout << "measurement vector : distance from querry point " << std::endl;
  std::vector<double> distances1 (numberOfNeighbors);
  for ( unsigned int i = 0; i < numberOfNeighbors; ++i )
     {
     distances1[i] =  distanceMetric->Evaluate(
       tree->GetMeasurementVector( neighbors[i] ));
     std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
               << "] : "
               << distances1[i]
               << std::endl;
               }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Instead of using an Euclidean distance metric, Tree itself can also return
  // the distance vector. Here we get the distance values from tree and compare
  // them with previous values.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::vector<double> distances2;
  tree->Search( queryPoint, numberOfNeighbors, neighbors, distances2 );

  std::cout << "\n*** kd-tree knn search result directly from tree:"
            << std::endl
            << "query point = [" << queryPoint << "]" << std::endl
            << "k = " << numberOfNeighbors << std::endl;
  std::cout << "measurement vector : distance from querry point " << std::endl;
  for ( unsigned int i = 0; i < numberOfNeighbors; ++i )
    {
    std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
              << "] : "
              << distances2[i]
              << std::endl;
    if ( itk::Math::NotAlmostEquals( distances2[i], distances1[i] ) )
      {
      std::cerr << "Mismatched distance values by tree." << std::endl;
      return EXIT_FAILURE;
      }
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // As previously indicated, the interface for finding nearest neighbors in
  // the centroid tree is very similar.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::vector<double> distances3;
  centroidTree->Search(
    queryPoint, numberOfNeighbors, neighbors, distances3 );

  centroidTree->Search( queryPoint, numberOfNeighbors, neighbors );
  std::cout << "\n*** Weighted centroid kd-tree knn search result:"
            << std::endl
            << "query point = [" << queryPoint << "]" << std::endl
            << "k = " << numberOfNeighbors << std::endl;
  std::cout << "measurement vector : distance_by_distMetric : distance_by_tree"
            << std::endl;
  std::vector<double> distances4 (numberOfNeighbors);
  for ( unsigned int i = 0; i < numberOfNeighbors; ++i )
    {
    distances4[i] = distanceMetric->Evaluate(
      centroidTree->GetMeasurementVector( neighbors[i]));
    std::cout << "[" << centroidTree->GetMeasurementVector( neighbors[i] )
              << "]       :       "
              << distances4[i]
              << "            :       "
              << distances3[i]
              << std::endl;
    if ( itk::Math::NotAlmostEquals( distances2[i], distances1[i] ) )
      {
      std::cerr << "Mismatched distance values by centroid tree." << std::endl;
      return EXIT_FAILURE;
      }
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // KdTree also supports searching points within a hyper-spherical
  // kernel. We specify the radius and call the \code{Search()} method.  In
  // the case of the KdTree, this is done with the following lines of code.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double radius = 437.0;

  tree->Search( queryPoint, radius, neighbors );

  std::cout << "\nSearching points within a hyper-spherical kernel:"
            << std::endl;
  std::cout << "*** kd-tree radius search result:" << std::endl
            << "query point = [" << queryPoint << "]" << std::endl
            << "search radius = " << radius << std::endl;
  std::cout << "measurement vector : distance" << std::endl;
  for ( unsigned int i = 0; i < neighbors.size(); ++i )
    {
    std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
              << "] : "
              << distanceMetric->Evaluate(
                  tree->GetMeasurementVector( neighbors[i]))
              << std::endl;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // In the case of the centroid KdTree, the \code{Search()} method is used as
  // illustrated by the following code.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  centroidTree->Search( queryPoint, radius, neighbors );
  std::cout << "\n*** Weighted centroid kd-tree radius search result:"
            << std::endl
            << "query point = [" << queryPoint << "]" << std::endl
            << "search radius = " << radius << std::endl;
  std::cout << "measurement vector : distance" << std::endl;
  for ( unsigned int i = 0; i < neighbors.size(); ++i )
    {
    std::cout << "[" << centroidTree->GetMeasurementVector( neighbors[i] )
              << "] : "
              << distanceMetric->Evaluate(
                  centroidTree->GetMeasurementVector( neighbors[i]))
              << std::endl;
    }
  // Software Guide : EndCodeSnippet
  return EXIT_SUCCESS;
}
