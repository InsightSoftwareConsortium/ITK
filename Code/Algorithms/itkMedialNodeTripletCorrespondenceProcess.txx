/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedialNodeTripletCorrespondenceProcess.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMedialNodeTripletCorrespondenceProcess_txx
#define __itkMedialNodeTripletCorrespondenceProcess_txx

#include "itkMedialNodeTripletCorrespondenceProcess.h"

#include <fstream>

namespace itk
{

/** 
 * Constructor.
 */
template< typename TSourceImage >
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::MedialNodeTripletCorrespondenceProcess()
{
  itkDebugMacro(<< "itkMedialNodeTripletCorrespondenceProcess::itkMedialNodeTripletCorrespondenceProcess() called");

  m_NumberOfNodeBaseTriplets = 0;
  m_NumberOfTriplets = 0;

  m_CreateOutputFile = false;

  //Setting the output
  OutputDataStructurePointerType output;
  output = static_cast<MedialNodeTripletCorrespondenceProcess::OutputDataStructureType*>(this->MakeOutput(0).GetPointer()); 
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}

/**
 * 
 */
template< typename TSourceImage >
typename MedialNodeTripletCorrespondenceProcess< TSourceImage >::DataObjectPointer
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::MakeOutput(unsigned int)
{
  return static_cast<DataObject*>(OutputDataStructureType::New().GetPointer());
}

/**
 *
 */
template< typename TSourceImage >
typename MedialNodeTripletCorrespondenceProcess< TSourceImage >::OutputDataStructureType*
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return static_cast< OutputDataStructureType * >
                     (this->ProcessObject::GetOutput(0));
}

/** 
 * Set the itkCorrespondenceDataStructure associated with node pairs between the two images as an input.
 */
template< typename TSourceImage >
void
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::SetPairDataStructure(const InputDataStructureType * InputDataStructure )
{
  itkDebugMacro(<< "MedialNodeTripletCorrespondenceProcess: Setting first input");
  // Process object is not const-correct so the const casting is required.
  SetNthInput(0,  const_cast<InputDataStructureType *>( InputDataStructure ) );
}

/** 
 * Set the distance matrix for image A.
 */
template< typename TSourceImage >
void
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::SetDistanceMatrixA( const DistanceMatrixType * DistanceMatrixA ) 
{
  itkDebugMacro(<< "MedialNodeTripletCorrespondenceProcess: Setting first distance matrix");
  // Process object is not const-correct so the const casting is required.
  SetNthInput(1,  const_cast<DistanceMatrixType *>( DistanceMatrixA ) );
}

/** 
 * Set the distance matrix for image B.
 */
template< typename TSourceImage >
void
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::SetDistanceMatrixB( const DistanceMatrixType * DistanceMatrixB ) 
{
  itkDebugMacro(<< "MedialNodeTripletCorrespondenceProcess: Setting second distance matrix");
  // Process object is not const-correct so the const casting is required.
  SetNthInput(2,  const_cast<DistanceMatrixType *>( DistanceMatrixB ) );
}

/** 
 * Set the first core atom image input.
 */
template< typename TSourceImage >
void
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::SetCoreAtomImageA(const CoreAtomImageType * CoreAtomImageA ) 
{
  itkDebugMacro(<< "MedialNodeTripletCorrespondenceProcess: Setting first core atom image");
  // Process object is not const-correct so the const casting is required.
  SetNthInput(3,  const_cast<TSourceImage *>( CoreAtomImageA ) );
}

/** 
 * Set the second core atom image input.
 */
template< typename TSourceImage >
void
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::SetCoreAtomImageB(const CoreAtomImageType * CoreAtomImageB ) 
{
  itkDebugMacro(<< "MedialNodeTripletCorrespondenceProcess: Setting second core atom image");
  // Process object is not const-correct so the const casting is required.
  SetNthInput(4, const_cast<TSourceImage *>( CoreAtomImageB ) );
}

/** 
 * Get the first core atom image input.
 */
template< typename TSourceImage >
TSourceImage *
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::GetCoreAtomImageA() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<TSourceImage *>(GetNthInput(3));
}

/** 
 * Get the second core atom image input.
 */
template< typename TSourceImage >
TSourceImage *
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::GetCoreAtomImageB() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<TSourceImage *>(GetNthInput(4));
}

/** 
 * Get the distance matrix for core atom image A.
 */
template< typename TSourceImage >
typename MedialNodeTripletCorrespondenceProcess< TSourceImage >::DistanceMatrixType *
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::GetDistanceMatrixA() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<DistanceMatrixType *>(GetNthInput(1));
}

/** 
 * Get the distance matrix for core atom image B.
 */
template< typename TSourceImage >
typename MedialNodeTripletCorrespondenceProcess< TSourceImage >::DistanceMatrixType *
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::GetDistanceMatrixB() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<DistanceMatrixType *>(GetNthInput(2));
}

/** 
 * Analyze the pair data structure and create the triplet data structure.
 */
template< typename TSourceImage >
void
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::GenerateData()
{
  itkDebugMacro(<< "itkMedialNodeTripletCorrespondenceProcess::GenerateData() called");

  // Optional output file for debugging purposes.
  std::ofstream OutputFile;
  if(m_CreateOutputFile)
    OutputFile.open("triplet_creation_log.txt",std::ios::out);

  // Pointers to the input, output objects.
  m_InputDataStructure = dynamic_cast<InputDataStructureType*>(ProcessObject::GetInput(0));
  m_DistanceMatrixA = dynamic_cast<DistanceMatrixType*>(ProcessObject::GetInput(1));
  m_DistanceMatrixB = dynamic_cast<DistanceMatrixType*>(ProcessObject::GetInput(2));
  m_CoreAtomImageA = dynamic_cast<CoreAtomImageType*>(ProcessObject::GetInput(3));
  m_CoreAtomImageB = dynamic_cast<CoreAtomImageType*>(ProcessObject::GetInput(4));
  m_OutputDataStructure = dynamic_cast<OutputDataStructureType*>(ProcessObject::GetOutput(0));

  int counter = 0;
  int counter2 = 0;
  int counter3 = 0;

  double TemporaryDistanceA;
  double TemporaryDistanceB;

  // Get node pointer lists.
  typedef BloxCoreAtomPixel<NDimensions> PixelType;
  typedef std::vector<PixelType*> NodePointerListType;

  NodePointerListType NodePointerListA;
  NodePointerListType NodePointerListB;

  NodePointerListA = *(m_CoreAtomImageA->GetNodePointerList());
  NodePointerListB = *(m_CoreAtomImageB->GetNodePointerList());

  typename InputDataStructureType::NodeListType::iterator NodeListIterator = m_InputDataStructure->m_NodeList->begin();

  typename InputDataStructureType::SecondaryNodeListType::iterator SecondaryListIterator1;
  typename InputDataStructureType::SecondaryNodeListType::iterator SecondaryListIterator2;
  typename InputDataStructureType::CorrespondingListType::iterator CorrespondingListIterator1;
  typename InputDataStructureType::CorrespondingListType::iterator CorrespondingListIterator2;

  bool BaseTripAdded = false;

  while( NodeListIterator != m_InputDataStructure->m_NodeList->end() )//iterate through NodeList in pair structure
    {
    counter2 = 0;

    // Create a new CorresponsingPairList.
    typename OutputDataStructureType::SecondaryNodeListType * TripletListPointer = new typename OutputDataStructureType::SecondaryNodeListType();

    for(SecondaryListIterator1 = NodeListIterator->begin();SecondaryListIterator1 != NodeListIterator->end();SecondaryListIterator1++)//iterate through SecondaryNodeLists in pair structure
      {
      SecondaryListIterator2 = NodeListIterator->begin();
      counter3 = 0;

      while( SecondaryListIterator2 != NodeListIterator->end() && counter3 < counter2)//concurrent iteration through SecondaryNodeList
        {                                             //with different iterator
        if(m_CreateOutputFile)
          {
          OutputFile << "\n\n**NEW BASE TRIPLET**" << "\nTesting Triplet: (" << NodeListIterator->GetIndex() << "," 
                     << SecondaryListIterator1->GetIndex(0) << "," << SecondaryListIterator2->GetIndex(0) << ") : ";
          }

        //If statement to make sure no two indices in the BASE triplet are the same
        if(NodeListIterator->GetIndex() != SecondaryListIterator1->GetIndex(0) && 
           SecondaryListIterator1->GetIndex(0) != SecondaryListIterator2->GetIndex(0) &&
           NodeListIterator->GetIndex() != SecondaryListIterator2->GetIndex(0))
          {

          if(m_CreateOutputFile)
            OutputFile << " no duplicate indices :)" << std::endl;

          // Starting a new base triplet, so:
          BaseTripAdded = false;

          // our triplet is represented by:
          // it->GetIndex(0);
          // SecondaryListIterator1->GetIndex(0);
          // SecondaryListIterator2->GetIndex(0);

          typename OutputDataStructureType::CorrespondingListType * CorrespondingTripletListPointer = new typename OutputDataStructureType::CorrespondingListType();
          typename OutputDataStructureType::CorrespondingListType::iterator CorrespondingTripletListIterator;

          bool FoundCorrespondingTriplet = false;

          // Iterate through the two corresponding pair lists simultaneously
          // finding every possible pair, and testing for triplet correspondence.
          for(CorrespondingListIterator1 = SecondaryListIterator1->begin();CorrespondingListIterator1 != SecondaryListIterator1->end();CorrespondingListIterator1++)//iterate through CorrPairList of SecondaryListIterator1
            {
            for(CorrespondingListIterator2 = SecondaryListIterator2->begin();CorrespondingListIterator2 != SecondaryListIterator2->end();CorrespondingListIterator2++)//iterate through CorrPairList of SecondaryListIterator2
              {
              // Check NodeIndex[0] (base node correspondence) in the corresponding pairs to make sure they are the
              // same node in the corresponding image.
              if(CorrespondingListIterator1->GetNodeIndex(0) == CorrespondingListIterator2->GetNodeIndex(0))
                {
                // Check to see if the corresponding triplet has a duplicate node.
                if(CorrespondingListIterator1->GetNodeIndex(0) != CorrespondingListIterator1->GetNodeIndex(1) && 
                   CorrespondingListIterator1->GetNodeIndex(0) != CorrespondingListIterator2->GetNodeIndex(1) && 
                   CorrespondingListIterator1->GetNodeIndex(1) != CorrespondingListIterator2->GetNodeIndex(1))
                  {
                  bool DuplicateCorrespondingTriplet = false;

                  if(BaseTripAdded == true)
                    {
                    // Need an iterator for CorrespondingTripletListPointer.
                    CorrespondingTripletListIterator = CorrespondingTripletListPointer->begin();

                    while( CorrespondingTripletListIterator != CorrespondingTripletListPointer->end() && DuplicateCorrespondingTriplet == false)
                      {
                      if( (CorrespondingListIterator1->GetNodeIndex(0) == CorrespondingTripletListIterator->GetNodeIndex(0) ||
                           CorrespondingListIterator1->GetNodeIndex(1) == CorrespondingTripletListIterator->GetNodeIndex(0) ||
                           CorrespondingListIterator2->GetNodeIndex(1) == CorrespondingTripletListIterator->GetNodeIndex(0)) && 
                          (CorrespondingListIterator1->GetNodeIndex(0) == CorrespondingTripletListIterator->GetNodeIndex(1) ||
                           CorrespondingListIterator1->GetNodeIndex(1) == CorrespondingTripletListIterator->GetNodeIndex(1) ||
                           CorrespondingListIterator2->GetNodeIndex(1) == CorrespondingTripletListIterator->GetNodeIndex(1)) && 
                          (CorrespondingListIterator1->GetNodeIndex(0) == CorrespondingTripletListIterator->GetNodeIndex(2) ||
                           CorrespondingListIterator1->GetNodeIndex(1) == CorrespondingTripletListIterator->GetNodeIndex(2) ||
                           CorrespondingListIterator2->GetNodeIndex(1) == CorrespondingTripletListIterator->GetNodeIndex(2)) )
                        {
                        DuplicateCorrespondingTriplet = true;
                        }
                      CorrespondingTripletListIterator++;
                      }
                    }

                  if(m_CreateOutputFile)
                    {
                    OutputFile << "    Checking Triplet: (" << CorrespondingListIterator1->GetNodeIndex(0) << "," 
                               << CorrespondingListIterator1->GetNodeIndex(1) << "," << CorrespondingListIterator2->GetNodeIndex(1) 
                               << ")  for correspondence" << std::endl;
                    }

                  if(DuplicateCorrespondingTriplet == false)
                    {
                    // Get distances between secondary nodes in triplets.
                    TemporaryDistanceA = m_DistanceMatrixA->get(SecondaryListIterator1->GetIndex(0), SecondaryListIterator2->GetIndex(0));
                    TemporaryDistanceB = m_DistanceMatrixB->get(CorrespondingListIterator1->GetNodeIndex(1),CorrespondingListIterator2->GetNodeIndex(1));

                    int numTriplets = 0;
                    if( TemporaryDistanceA <= (TemporaryDistanceB+0.1) && TemporaryDistanceA >= (TemporaryDistanceB-0.1) )
                      {
                      // If this comes out true, we have found a corresponding triplet.

                      if(m_CreateOutputFile)
                        OutputFile << "      Distance test passed...this is a corresponding triplet" << std::endl;

                      if(BaseTripAdded == false)
                        {
                        // Add base triplet.
                        ++m_NumberOfNodeBaseTriplets;
                      
                        // Set the index of the base node in the triplet.
                        TripletListPointer->SetIndex(NodeListIterator->GetIndex() );
                        // Set the index of the second node in the triplet.
                        CorrespondingTripletListPointer->SetIndex(0, SecondaryListIterator1->GetIndex(0) );
                        // Set the index of the third node in the triplet.
                        CorrespondingTripletListPointer->SetIndex(1, SecondaryListIterator2->GetIndex(0) );

                        // Make a new Node for corresponding triplet.
                        OutputNodeType NewNode;

                        NewNode.SetNodeIndex(0,CorrespondingListIterator1->GetNodeIndex(0));
                        NewNode.SetNodeIndex(1,CorrespondingListIterator1->GetNodeIndex(1));
                        NewNode.SetNodeIndex(2,CorrespondingListIterator2->GetNodeIndex(1));

                        if(m_CreateOutputFile)
                          {
                          OutputFile << "      Created node for corresponding triplet: (" << NewNode.GetNodeIndex(0)
                                     << "," << NewNode.GetNodeIndex(1) << "," << NewNode.GetNodeIndex(2)
                                     << ")" << std::endl;
                          }

                        CorrespondingTripletListPointer->push_back(NewNode);

                        if(m_CreateOutputFile)
                          OutputFile << "      Added new node to CorrespondingTripletListPointer" << std::endl;

                        FoundCorrespondingTriplet = true;

                        BaseTripAdded = true;
                        }
                      else
                        {
                        if(m_CreateOutputFile)
                          {
                          OutputFile << "      Base triplet " << "(" << NodeListIterator->GetIndex()
                                     << "," << SecondaryListIterator1->GetIndex(0) << "," << SecondaryListIterator2->GetIndex(0) 
                                     << ")" << " is already in structure" << std::endl;
                          }

                        OutputNodeType NewNode;
                    
                        NewNode.SetNodeIndex(0,CorrespondingListIterator1->GetNodeIndex(0));
                        NewNode.SetNodeIndex(1,CorrespondingListIterator1->GetNodeIndex(1));
                        NewNode.SetNodeIndex(2,CorrespondingListIterator2->GetNodeIndex(1));

                        if(m_CreateOutputFile)
                          {
                          OutputFile << "      Created node for corresponding triplet: (" << NewNode.GetNodeIndex(0)
                                     << "," << NewNode.GetNodeIndex(1) << "," << NewNode.GetNodeIndex(2)
                                     << ")" << std::endl;
                          }

                        CorrespondingTripletListPointer->push_back(NewNode);
                        FoundCorrespondingTriplet = true;

                        if(m_CreateOutputFile)
                          OutputFile << "      Added new node to CorrespondingTripletListPointer" << std::endl;
                        }
                      numTriplets++;
                      }
                    }
                  else
                    {
                    if(m_CreateOutputFile)
                      OutputFile << "      DUPLICATE" << std::endl;
                    }
                  }
                }
              }// End for CorrespondingListIterator2.
            }// End for CorrespondingListIterator1.
          // Add a new CorrespondingTripletList to the structure.
          if(FoundCorrespondingTriplet == true)
            {
            TripletListPointer->push_back(*CorrespondingTripletListPointer);
            }  
          }  
        counter++;
        SecondaryListIterator2++;
        counter3++;
        }
      counter2++;
      }
    NodeListIterator++;

    // Add a new index of the NodeList.
    if(!TripletListPointer->empty())
      {
      m_OutputDataStructure->m_NodeList->push_back(*TripletListPointer);
       
      if(m_CreateOutputFile)
        OutputFile << "\n    Added new list to m_NodeList" << std::endl;
      }
    else 
      {
      if(m_CreateOutputFile)
        OutputFile << "TripletListPointer is EMPTY, NOT adding new list to m_NodeList" << std::endl;
      }
    }// End while loop for NodeListIterator.

  itkDebugMacro(<< "End Initialize Triplets: " << m_NumberOfTriplets << " found!" << std::endl);
  itkDebugMacro(<< "Number of Base Triplets: " << m_NumberOfNodeBaseTriplets << " found!" << std::endl);

  if(m_CreateOutputFile)
    OutputFile.close();

  itkDebugMacro(<< "Finished MedialNodeTripletCorrespondenceProcess\n");
}

/**
 * Print Self
 */
template< typename TSourceImage >
void
MedialNodeTripletCorrespondenceProcess< TSourceImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace

#endif
