/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedialNodePairCorrespondenceProcess.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMedialNodePairCorrespondenceProcess_txx
#define __itkMedialNodePairCorrespondenceProcess_txx

#include "itkMedialNodePairCorrespondenceProcess.h"

#include <fstream>

namespace itk
{

/**
 * Constructor
 */
template< typename TSourceImage >
MedialNodePairCorrespondenceProcess< TSourceImage >
::MedialNodePairCorrespondenceProcess()
{
  itkDebugMacro(<< "itkMedialNodePairCorrespondenceProcess::itkMedialNodePairCorrespondenceProcess() called")

  // Setting the output.
  DataStructurePointerType output;
  output  = static_cast<MedialNodePairCorrespondenceProcess::DataStructureType*>(this->MakeOutput(0).GetPointer()); 
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());

  // Initialize necessary objects and variables.
  m_BinaryMetric = BinaryMetricType::New();

  m_NumberOfNodePairs = 0;
  m_NumberOfNodeBasePairs = 0;

  // Output testing file creation flags.  Set to off by default.
  m_CreateOutputFile = false;
  m_OutputPNG = false;
}

/**
 * Make Output.
 */
template< typename TSourceImage >
typename MedialNodePairCorrespondenceProcess< TSourceImage >::DataObjectPointer
MedialNodePairCorrespondenceProcess< TSourceImage >
::MakeOutput(unsigned int)
{
  return static_cast<DataObject*>(DataStructureType::New().GetPointer());
}

/**
 * Get Output.
 */
template< typename TSourceImage >
typename MedialNodePairCorrespondenceProcess< TSourceImage >::DataStructureType*
MedialNodePairCorrespondenceProcess< TSourceImage >
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return static_cast< DataStructureType * >
                     (this->ProcessObject::GetOutput(0));
}

/**
 * Set the first core atom image input.
 */
template< typename TSourceImage >
void
MedialNodePairCorrespondenceProcess< TSourceImage >
::SetCoreAtomImageA(const CoreAtomImageType * CoreAtomImageA ) 
{
  std::cerr << "MedialNodePairCorrespondenceProcess: Setting first input" << std::endl;
  // Process object is not const-correct so the const casting is required.
  SetNthInput(0,  const_cast<TSourceImage *>( CoreAtomImageA ) );
}

/**
 * Set the second core atom image input.
 */
template< typename TSourceImage >
void
MedialNodePairCorrespondenceProcess< TSourceImage >
::SetCoreAtomImageB(const CoreAtomImageType * CoreAtomImageB ) 
{
  std::cerr << "MedialNodePairCorrespondenceProcess: Setting second input" << std::endl;
  // Process object is not const-correct so the const casting is required.
  SetNthInput(1, const_cast<TSourceImage *>( CoreAtomImageB ) );
}

/**
 * Set the first distance matrix input.
 */
template< typename TSourceImage >
void
MedialNodePairCorrespondenceProcess< TSourceImage >
::SetDistanceMatrixA(const DistanceMatrixType * DistanceMatrixA ) 
{
  std::cerr << "MedialNodePairCorrespondenceProcess: Setting third input" << std::endl;
  // Process object is not const-correct so the const casting is required.
  SetNthInput(2, const_cast<DistanceMatrixType *>( DistanceMatrixA ) );
}

/**
 * Set the second distance matrix input.
 */
template< typename TSourceImage >
void
MedialNodePairCorrespondenceProcess< TSourceImage >
::SetDistanceMatrixB(const DistanceMatrixType * DistanceMatrixB ) 
{
  std::cerr << "MedialNodePairCorrespondenceProcess: Setting fourth input" << std::endl;
  // Process object is not const-correct so the const casting is required.
  SetNthInput(3, const_cast<DistanceMatrixType *>( DistanceMatrixB ) );
}

/**
 * Set the correspondence matrix input.
 */
template< typename TSourceImage >
void
MedialNodePairCorrespondenceProcess< TSourceImage >
::SetCorrespondenceMatrix(const CorrespondenceMatrixType * CorrespondenceMatrix ) 
{
  std::cerr << "MedialNodePairCorrespondenceProcess: Setting fifth input" << std::endl;
  // Process object is not const-correct so the const casting is required.
  SetNthInput(4, const_cast<CorrespondenceMatrixType *>( CorrespondenceMatrix ) );
}

/**
 * Get the first core atom image input.
 */
template< typename TSourceImage >
TSourceImage *
MedialNodePairCorrespondenceProcess< TSourceImage >
::GetCoreAtomImageA() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<TSourceImage *>(GetNthInput(0));
}

/**
 * Get the second core atom image input.
 */
template< typename TSourceImage >
TSourceImage *
MedialNodePairCorrespondenceProcess< TSourceImage >
::GetCoreAtomImageB() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<TSourceImage *>(GetNthInput(1));
}

/**
 * Get the first distance matrix input.
 */
template< typename TSourceImage >
typename MedialNodePairCorrespondenceProcess< TSourceImage >::DistanceMatrixType *
MedialNodePairCorrespondenceProcess< TSourceImage >
::GetDistanceMatrixA() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<DistanceMatrixType *>(GetNthInput(2));
}

/**
 * Get the second distance matrix input.
 */
template< typename TSourceImage >
typename MedialNodePairCorrespondenceProcess< TSourceImage >::DistanceMatrixType *
MedialNodePairCorrespondenceProcess< TSourceImage >
::GetDistanceMatrixB() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<DistanceMatrixType *>(GetNthInput(3));
}

/**
 * Get the correspondence matrix.
 */
template< typename TSourceImage >
typename MedialNodePairCorrespondenceProcess< TSourceImage >::CorrespondenceMatrixType *
MedialNodePairCorrespondenceProcess< TSourceImage >
::GetCorrespondenceMatrix() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<CorrespondenceMatrixType *>(GetNthInput(4));
}

/**
 * Analyze the inputs and produce the pair correspondence data structure.
 */
template< typename TSourceImage >
void
MedialNodePairCorrespondenceProcess< TSourceImage >
::GenerateData()
{
  itkDebugMacro(<< "itkMedialNodePairCorrespondenceProcess::GenerateData() called");

  // Pointers to the input, output objects.
  m_CoreAtomImageA = dynamic_cast<CoreAtomImageType*>(ProcessObject::GetInput(0));
  m_CoreAtomImageB = dynamic_cast<CoreAtomImageType*>(ProcessObject::GetInput(1));
  m_DistanceMatrixA = dynamic_cast<DistanceMatrixType*>(ProcessObject::GetInput(2));
  m_DistanceMatrixB = dynamic_cast<DistanceMatrixType*>(ProcessObject::GetInput(3));
  m_CorrespondenceMatrix = dynamic_cast<CorrespondenceMatrixType*>(ProcessObject::GetInput(4));
  m_DataStructure = dynamic_cast<DataStructureType*>(ProcessObject::GetOutput(0));

  m_Rows = m_CoreAtomImageA->GetMedialNodeCount();
  m_Columns = m_CoreAtomImageB->GetMedialNodeCount();

  typedef BloxCoreAtomPixel<NDimensions> PixelType;
  typedef std::vector<PixelType*> NodePointerListType;

  NodePointerListType NodePointerList1;
  NodePointerListType NodePointerList2;

  NodePointerList1 = *(m_CoreAtomImageA->GetNodePointerList());
  NodePointerList2 = *(m_CoreAtomImageB->GetNodePointerList());

  double TemporaryDistance;
  double result1, result2;
  bool BasePairAdded = false;

  // Initializing objects to write-out a PNG image of correspondance matrix.
  typedef unsigned char CorrespondencePixelType;  
  typedef Image<CorrespondencePixelType, 2> CorrespondenceImageType;

  CorrespondenceImageType::IndexType pixelIndex;
  CorrespondenceImageType::SizeType size;
  size[0]=m_Rows;  
  size[1]=m_Columns;
  
  CorrespondenceImageType::RegionType region;
  CorrespondenceImageType::IndexType  index;
  index.Fill(0);
  region.SetIndex( index );
  region.SetSize(size);

  CorrespondenceImageType::Pointer CorrespondenceImage;
  CorrespondenceImage = CorrespondenceImageType::New();

  CorrespondenceImage->SetRegions( region );
  CorrespondenceImage->Allocate();
  CorrespondenceImage->FillBuffer(0);

  typedef ImageFileWriter<CorrespondenceImageType> FileWriterType;
  FileWriterType::Pointer imageWriter;
  imageWriter = FileWriterType::New();

  PNGImageIO::Pointer io;
  io = PNGImageIO::New();

  // Output file for debugging/testing.
  std::ofstream PairCreationFile;
  PairCreationFile.open("pair_creation_log.txt",std::ios::out); 

  for(int i = 0;i<m_Rows;++i)//iterate through rows in dist matrix A
    {  
    // Create a new NodePairList.
    typename DataStructureType::SecondaryNodeListType * PairListPointer = new typename DataStructureType::SecondaryNodeListType();

    for(int j = 0;j<m_Rows;++j)// Iterate through rows in dist matrix A.
      {
      if(j > i) // Make sure we dont check for the same pair twice.
        {
        if(m_CreateOutputFile)
          {
          PairCreationFile << "Testing base pair: (" << i << "," << j << ")" << std::endl;
          }

        //reset pairAdded to false, as we have just started on a new base pair
        BasePairAdded = false;

        //create a new CorresponsingPairList
        typename DataStructureType::CorrespondingListType * CorrespondingPairListPointer = new typename DataStructureType::CorrespondingListType();

        //get distance between two nodes in question from DMatrixA
        TemporaryDistance = m_DistanceMatrixA->get(i,j);

        if(m_CreateOutputFile)
          {
          PairCreationFile << "  Distance for base pair: (" << i << "," << j << ") is: " << TemporaryDistance << std::endl;
          }

        // Iterate through DMatrixB to find distances close enough to TemporaryDistance.
        for(int a = 0;a<m_Columns;++a)//iterate through rows in dist matrix B
          {
          for(int b = 0;b<m_Columns;++b)//iterate through columns in dist matrix B
            {
            if(m_CreateOutputFile)
              {
              PairCreationFile << "\n  Testing pair: (" << a << "," << b << ") for correspondence" << std::endl;
              PairCreationFile << "    Passed: ";
              PairCreationFile << "    Distance for corr pair: (" << a << "," << b << ") is: " << m_DistanceMatrixB->get(a,b) << std::endl;
              }

            // If distance of pair in matrix B is close enough to pair in matrix A.
            if( ( m_DistanceMatrixB->get(a,b) > (TemporaryDistance-0.1) ) && ( m_DistanceMatrixB->get(a,b) < (TemporaryDistance+0.1) ) )
              {
              // Check correspondence matrix for matching C values.
              if( m_CorrespondenceMatrix->get(i,a) >= 0.9 && m_CorrespondenceMatrix->get(j,b) >= 0.9 )
                {
                if(m_CreateOutputFile)
                  {
                  PairCreationFile << " -> Unary 1";
                  }
                    
                // In this case, we are asserting that j~a and i~b.

                //Test (i,j)~(a,b).

                // Here we run the signature metric on the pairs, as a final test of correspondence.
                m_BinaryMetric->SetMedialNodes(NodePointerList1[i], 
                                               NodePointerList1[j], 
                                               NodePointerList2[a], 
                                               NodePointerList2[b] );

                // Initialize and run binary metric to get value.
                m_BinaryMetric->Initialize();
                result1 = m_BinaryMetric->GetResult();

                //Test (j,i)~(b,a).

                //Here we run the signature metric on the pairs, as a final test of correspondence
                m_BinaryMetric->SetMedialNodes(NodePointerList1[j], 
                                               NodePointerList1[i], 
                                               NodePointerList2[b], 
                                               NodePointerList2[a] );

                // Initialize and run binary metric to get value.
                m_BinaryMetric->Initialize();
                result2 = m_BinaryMetric->GetResult();

                //If this is true, pair i-j matches pair a-b.
                if(result1 >= 0.95 && result2 >= 0.95)
                  {
                  if(m_CreateOutputFile)
                    {
                    PairCreationFile << " -> Binary 1" << std::endl;
                    }

                  if(m_OutputPNG)
                    {
                    pixelIndex[0] = i;
                    pixelIndex[1] = a;
                    CorrespondenceImage->SetPixel(pixelIndex, 100*m_CorrespondenceMatrix->get(i,a) );

                    pixelIndex[0] = j;
                    pixelIndex[1] = b;
                    CorrespondenceImage->SetPixel(pixelIndex, 100*m_CorrespondenceMatrix->get(j,b) );
                    }

                  // If it isnt already there, add i-j to pair correspondence structure 
                  // with pair a-b as a matching pair.  if it is, just add pair a-b
                  // to its list of matching pairs. 
                  if(BasePairAdded == false)
                    {
                    if(m_CreateOutputFile)
                      {
                      PairCreationFile << "    Adding base pair " << "(" << i << "," << j << ")" << std::endl;
                      }

                    m_NumberOfNodeBasePairs++;

                    // Set the index of the base node in the pair.
                    PairListPointer->SetIndex(i);

                    // Set the index of the second node in the pair.
                    CorrespondingPairListPointer->SetIndex(0,j);

                    //make a new node for pair a-b, the corresponding pair
                    NodeType NewNode;
                    //index i corresponds to index a, so a is base_pair_node_index
                    NewNode.SetNodeIndex(0,a);
                    NewNode.SetNodeIndex(1,b);
                    NewNode.SetCorrespondenceValue(0,result1);
                    NewNode.SetCorrespondenceValue(1,result2);

                    // Add a node to the corresponding pair list, and add the 
                    // corresponding pair list to the pair list.
                    CorrespondingPairListPointer->push_back(NewNode);

                    if(m_CreateOutputFile)
                      {
                      PairCreationFile << "    Added corr pair " << "(" << a << "," << b << ")" << std::endl;
                      }

                    // Set pairAdded to true, as it has just been added.
                    BasePairAdded = true;
                    }
                  else
                    {
                    // Add new corresponding pair to existing index in pair list
                    // of current pair.

                    // Make a new Node for pair a-b.
                    NodeType NewNode;
                    NewNode.SetNodeIndex(0,a);
                    NewNode.SetNodeIndex(1,b);
                    NewNode.SetCorrespondenceValue(0,result1);
                    NewNode.SetCorrespondenceValue(1,result2);
                      
                    // Add a node to the corresponding pair list.
                    CorrespondingPairListPointer->push_back(NewNode);

                    if(m_CreateOutputFile)
                      {
                      PairCreationFile << "    Added corr pair " << "(" << a << "," << b << ")" << std::endl;
                      }
                    }
                  }
                }
              else if( m_CorrespondenceMatrix->get(i,b) >= 0.9 && m_CorrespondenceMatrix->get(j,a) >= 0.9 )
                {

                if(m_CreateOutputFile)
                  {
                  PairCreationFile << " -> Unary 2" << std::endl;
                  }

                // In this case, we are asserting that i~b and j~a.

                //(i,j)~(b,a)
                
                // Here we run the signature metric on the pairs, as a final test of correspondence.
                m_BinaryMetric->SetMedialNodes(NodePointerList1[i], 
                                               NodePointerList1[j], 
                                               NodePointerList2[b], 
                                               NodePointerList2[a] );

                // Initialize and run metric to get value
                m_BinaryMetric->Initialize();
                result1 = m_BinaryMetric->GetResult();

                //(j,i)~(a,b)

                // Here we run the signature metric on the pairs, as a final test of correspondence.
                m_BinaryMetric->SetMedialNodes(NodePointerList1[j], 
                                               NodePointerList1[i], 
                                               NodePointerList2[a], 
                                               NodePointerList2[b] );
                  
                // Initialize and run metric to get value
                m_BinaryMetric->Initialize();
                result2 = m_BinaryMetric->GetResult();

                if(m_OutputPNG)
                  {
                  pixelIndex[0] = i;
                  pixelIndex[1] = b;
                  CorrespondenceImage->SetPixel(pixelIndex, 100*m_CorrespondenceMatrix->get(i,b) );

                  pixelIndex[0] = j;
                  pixelIndex[1] = a;
                  CorrespondenceImage->SetPixel(pixelIndex, 100*m_CorrespondenceMatrix->get(j,a) );
                  }

                // If this is true, pair i-j matches pair a-b.
                if(result1 >= 0.95 && result2 >= 0.95)
                  {
                  if(m_CreateOutputFile)
                    {
                    PairCreationFile << " -> Binary 2" << std::endl;
                    }

                  // If it isnt already there, add i-j to pair correspondence structure 
                  // with pair a-b as a matching pair.  if it is, just add pair a-b
                  // to its list of matching pairs.  
                  if(BasePairAdded == false)
                    {
                    if(m_CreateOutputFile)
                      {
                      PairCreationFile << "    Adding base pair " << "(" << i << "," << j << ")" << std::endl;
                      }

                    m_NumberOfNodeBasePairs++;

                    // Set the index of the base node in the pair
                    PairListPointer->SetIndex(i);
                    // Set the index of the second node in the pair
                    CorrespondingPairListPointer->SetIndex(0,j);

                    // Make a new Node for pair b-a
                    NodeType NewNode;
                    // Index i corresponds to index b, so b is base_pair_node_index
                    NewNode.SetNodeIndex(0,b);
                    NewNode.SetNodeIndex(1,a);
                    NewNode.SetCorrespondenceValue(0,result1);
                    NewNode.SetCorrespondenceValue(1,result2);
                      
                    // Add a node to the corresponding pair list, and add the 
                    // corresponding pair list to the pair list.
                    CorrespondingPairListPointer->push_back(NewNode);

                    if(m_CreateOutputFile)
                      {
                      PairCreationFile << "    Added corr pair " << "(" << b << "," << a << ")" << std::endl;
                      }
                    BasePairAdded = true;//set pairAdded to true, as it has just been added
                    }
                  else
                    {
                    // Add new corresponding pair to existing index in pair list
                    // of current pair.

                    // Make a new Node for pair b-a.
                    NodeType NewNode;
                    NewNode.SetNodeIndex(0,b);
                    NewNode.SetNodeIndex(1,a);
                    NewNode.SetCorrespondenceValue(0,result1);
                    NewNode.SetCorrespondenceValue(1,result2);
                     
                    // Add a node to the corresponding pair list.
                    CorrespondingPairListPointer->push_back(NewNode);

                    if(m_CreateOutputFile)
                      {
                      PairCreationFile << "    Added corr pair " << "(" << b << "," << a << ")" << std::endl;
                      }
                    }
                  }
                }
              }              
            }// End dist matrix B for loop
          }// End dist matrix A for loop
        // Add a node (corresponding pair list) to the node pair list

        // Only add the CorrespondingPairListPointer if it has something in it.
        if(CorrespondingPairListPointer->GetSize() != 0)
        {
          PairListPointer->push_back(*CorrespondingPairListPointer);
        }
        m_NumberOfNodePairs++;
        }// End if(i<j)
      }// End for j

      // Add pair list to node list.
      if(PairListPointer->GetSize() != 0)
      {
        m_DataStructure->m_NodeList->push_back(*PairListPointer);
      }
    }// End for i
  if(m_OutputPNG)
    {
    // Write voting image out.
    std::cerr << "^_^ WRITE OUT CORRESPONDANCE IMAGE TO CorrespondenceImage.png" << std::endl;
    imageWriter->SetInput(CorrespondenceImage);
    imageWriter->SetFileName("CorrespondenceImage.png");
    imageWriter->SetImageIO(io);
    imageWriter->Write();
    }
  itkDebugMacro(<< "MedialNodePairCorrespondenceProcess::NumberOfNodePairs: " << m_NumberOfNodePairs << "\n");
  itkDebugMacro(<< "MedialNodePairCorrespondenceProcess::NumberOfNodeBasePairs: " << m_NumberOfNodeBasePairs << "\n");
  itkDebugMacro(<< "Finished MedialNodePairCorrespondenceProcess\n");
}

/**
 * Print Self
  */
template< typename TSourceImage >
void
MedialNodePairCorrespondenceProcess< TSourceImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace

#endif
