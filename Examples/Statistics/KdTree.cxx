/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    KdTree.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
// \index{itk::Statistics::KdTree|textbf}
// \index{itk::Statistics::KdTreeGenerator|textbf}
// \index{itk::Statistics::WeightedCenteroidKdTreeGenerator|textbf}
//
// kd-tree is a tree that separates a $k$-dimension space
// for thte \code{std::vector} class that will be the container for
// the measurement vectors from a sample.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkListSample.h"
#include "itkKdTree.h"
#include "itkKdTreeGenerator.h"
#include "itkWeightedCenteroidKdTreeGenerator.h"
// Software Guide : EndCodeSnippet

int main()
{

  // Software Guide : BeginLatex
  // We define the measurement vector type and instantiate a
  // \code{ListSample} object. Then put 1000 measurement vectors in the
  // object.  
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 2 > MeasurementVectorType ;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;

  MeasurementVectorType mv ;
  for (unsigned int i = 0 ; i < 1000 ; ++i )
    {
    mv[0] = (float) i ;
    mv[1] = (float) ((1000 - i) / 2 ) ;
    sample->PushBack( mv ) ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The following code snippet shows how to create two \code{KdTree}
  // objects. The first tree from the \code{treeGenerator} has smaller
  // set of information (partition dimension, partition value, and pointers
  // to the left and right child nodes). The second tree from the
  // \code{centeroidTreeGenerator} has additional information such as
  // the number of children under each node and the vector sum of the
  // measurement vectors belong to every child nodes belong to a node.
  //
  // The instantiation and input variables are exactly same for both
  // tree generators. Using the \code{SetSample} method we plug-in the
  // source sample. The bucket size input specifies the limit on the
  // maximum number of measurement vectors that can be stored in a
  // terminal (leaf) node. Bigger bucket size results in less number of
  // nodes in a tree. It also affects the efficiency of search. With
  // many small leaf nodes, we might experience slower search
  // performance because of excessive boundary comparisons.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::KdTreeGenerator< SampleType > TreeGeneratorType ;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New() ;

  treeGenerator->SetSample( sample ) ;
  treeGenerator->SetBucketSize( 16 ) ;
  treeGenerator->Update() ;

  typedef itk::Statistics::WeightedCenteroidKdTreeGenerator< SampleType > 
    CenteroidTreeGeneratorType ;
  CenteroidTreeGeneratorType::Pointer centeroidTreeGenerator = 
    CenteroidTreeGeneratorType::New() ;

  centeroidTreeGenerator->SetSample( sample ) ;
  centeroidTreeGenerator->SetBucketSize( 16 ) ;
  centeroidTreeGenerator->Update() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // After the generation step, we can get the pointer to the kd-tree
  // from the generator by calling the \code{GetOutput()} method. To
  // traverse a kd-tree, we have use the \code{GetRoot()} method. The
  // method will return the root node of the tree. Every node in a tree
  // can have its left and/or right child node. To get the child node,
  // we call the \code{Left()} or the \code{Right()} method of a node
  // (this methods are not part of the kd-tree).
  //
  // We can get other information about a node calling the method listed
  // below in addition to the child node pointers.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef TreeGeneratorType::KdTreeType TreeType ;
  typedef TreeType::NearestNeighbors NeighborsType ;
  typedef TreeType::KdTreeNodeType NodeType ;

  TreeType::Pointer tree = treeGenerator->GetOutput() ;
  TreeType::Pointer centeroidTree = centeroidTreeGenerator->GetOutput() ;

  NodeType* root = tree->GetRoot() ;

  if ( root->IsTerminal() )
    {
    std::cout << "Root node is a terminal node." << std::endl ;
    }
  else
    {
    std::cout << "Root node is not a terminal node." << std::endl ;
    }

  unsigned int partitionDimension ;
  float partitionValue ;
  root->GetParameters( partitionDimension, partitionValue) ;
  std::cout << "Dimension chosen to split the space = " 
            << partitionDimension << std::endl ;
  std::cout << "Split point on the partition dimension = "
            << partitionValue << std::endl ;

  std::cout << "Address of the left chile of the root node = "
            << root->Left() << std::endl ;
  
  std::cout << "Address of the right chile of the root node = "
            << root->Right() << std::endl ;

  root = centeroidTree->GetRoot() ;
  std::cout << "Number of the measurement vectors under the root node"
            << " in the tree hierarchy = " << root->Size() << std::endl ;

  NodeType::CenteroidType centeroid ;
  root->GetWeightedCenteroid( centeroid ) ;
  std::cout << "Sum of the measurement vectors under the root node = "
            << centeroid << std::endl ;

  std::cout << "Number of the measurement vectors under the left child"
            << " of the root node = " << root->Left()->Size() << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // In the following code snippet, we query the three nearest neighbors
  // of the \code{queryPoint} on the two tree. The results and
  // procedures are exacly same for both.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeasurementVectorType queryPoint ;
  queryPoint[0] = 10.0 ;
  queryPoint[1] = 7.0 ;

  unsigned int numberOfNeighbors = 3 ;
  tree->Search( queryPoint, numberOfNeighbors ) ; 
  
  NeighborsType result = tree->GetSearchResult() ;

  std::cout << "kd-tree knn search result: " << std::endl ;
  for ( unsigned int i = 0 ; i < numberOfNeighbors ; ++i )
    {
    std::cout << tree->GetMeasurementVector( result.GetNeighbor( i ) )
              << std::endl ;
    }

  centeroidTree->Search( queryPoint, numberOfNeighbors ) ; 
  result = centeroidTree->GetSearchResult() ;

  std::cout << "weighted centeroid kd-tree knn search result: " << std::endl ;
  for ( unsigned int i = 0 ; i < numberOfNeighbors ; ++i )
    {
    std::cout << centeroidTree->GetMeasurementVector( result.GetNeighbor( i ) )
              << std::endl ;
    }
  // Software Guide : EndCodeSnippet

  return 0 ;
}
