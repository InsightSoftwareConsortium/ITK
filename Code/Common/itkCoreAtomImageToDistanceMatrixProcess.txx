/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCoreAtomImageToDistanceMatrixProcess.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCoreAtomImageToDistanceMatrixProcess_txx
#define __itkCoreAtomImageToDistanceMatrixProcess_txx

#include "itkCoreAtomImageToDistanceMatrixProcess.h"

namespace itk
{

/**
* Default Constructor.  Initializes outputs for the process object.
*/
template< typename TSourceImage >
CoreAtomImageToDistanceMatrixProcess< TSourceImage >
::CoreAtomImageToDistanceMatrixProcess()
{
  itkDebugMacro(<< "itkCoreAtomImageToDistanceMatrixProcess::itkCoreAtomImageToDistanceMatrixProcess() called");

  // Setting the output.
  DistanceMatrixPointer output;
  output = static_cast<CoreAtomImageToDistanceMatrixProcess::DistanceMatrixType*>(this->MakeOutput(0).GetPointer()); 
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}

/**
 * Returns a pointer to the output cast as a Data Object.
 */
template< typename TSourceImage >
typename CoreAtomImageToDistanceMatrixProcess< TSourceImage >::DataObjectPointer
CoreAtomImageToDistanceMatrixProcess< TSourceImage >
::MakeOutput(unsigned int)
{
  return static_cast<DataObject*>(DistanceMatrixType::New().GetPointer());
}

/**
 * Standard GetOutput
 */
template< typename TSourceImage >
typename CoreAtomImageToDistanceMatrixProcess< TSourceImage >::DistanceMatrixType*
CoreAtomImageToDistanceMatrixProcess< TSourceImage >
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  return static_cast< DistanceMatrixType * >
                     (this->ProcessObject::GetOutput(0));
}

/**
 * Sets the core atom image input.
 */
template< typename TSourceImage >
void
CoreAtomImageToDistanceMatrixProcess< TSourceImage > 
::SetInput1(const TSourceImage * image1 ) 
{
  itkDebugMacro(<< "itkCoreAtomImageToDistanceMatrixProcess: Setting core atom image");
  // Process object is not const-correct so the const casting is required.
  SetNthInput(0,  const_cast<TSourceImage *>( image1 ) );
}

/**
 * Get the core atom image
 */
template< typename TSourceImage >
TSourceImage *
CoreAtomImageToDistanceMatrixProcess< TSourceImage >
::GetInput1() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<TSourceImage *>(GetNthInput(0));
}

/**
 * 
 */
template< typename TSourceImage >
void
CoreAtomImageToDistanceMatrixProcess< TSourceImage >
::GenerateData()
{
  itkDebugMacro(<< "itkCoreAtomImageToDistanceMatrixProcess::GenerateData() called");

  // Pointers to the core atom images, output matrix object.
  m_CoreAtomImage = dynamic_cast<CoreAtomImageType*>(ProcessObject::GetInput(0));
  m_DistanceMatrix = dynamic_cast<DistanceMatrixType*>(ProcessObject::GetOutput(0));

  // Get the number of medial nodes in the core atom image.
  m_NumberOfNodes = m_CoreAtomImage->GetMedialNodeCount();

  // Resize the distance matrix
  if(m_DistanceMatrix->resize(m_NumberOfNodes,m_NumberOfNodes))
    {
    itkDebugMacro(<< "m_DistanceMatrix resized successfully");
    }
  else
    {
    itkDebugMacro(<< "m_DistanceMatrix resize failed");
    assert(0);
    }
  itkDebugMacro(<< "CoreAtomImageToDistanceMatrixProcess::GenerateData(): Matrix Size: " << m_NumberOfNodes << " x " << m_NumberOfNodes);

  // Create iterator that will walk the blox core atom image.
  typedef itk::ImageRegionIterator<CoreAtomImageType> BloxIterator;

  BloxIterator bloxIt = BloxIterator(m_CoreAtomImage,
                                      m_CoreAtomImage->GetRequestedRegion() );

  BloxIterator bloxIt2 = BloxIterator(m_CoreAtomImage,
                                      m_CoreAtomImage->GetRequestedRegion() );

  // Pointer for accessing pixel.
  MedialNodeType* pPixel1;
  MedialNodeType* pPixel2;

  // Local variables.
  PositionType DistanceVector;
  PositionType Location1;
  PositionType Location2;
  double distance;
  int counter1 = 0;
  int counter2 = 0;

  // Iterate through nodes in the core atom image.
  for ( bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    pPixel1 = &bloxIt.Value();

    if( pPixel1->empty() )
      continue;

    for ( bloxIt2.GoToBegin(); !bloxIt2.IsAtEnd(); ++bloxIt2)
      {
      pPixel2 = &bloxIt2.Value();

      if( pPixel2->empty() )
        continue;

      // Get distance between pPixel1 and pPixel2.
      Location1 = pPixel1->GetVotedLocation();
      Location2 = pPixel2->GetVotedLocation();

      DistanceVector[0] = Location1[0] - Location2[0];
      DistanceVector[1] = Location1[1] - Location2[1];
      DistanceVector[2] = Location1[2] - Location2[2];

      distance = sqrt( pow((double)DistanceVector[0],2.0) + pow((double)DistanceVector[1],2.0) + pow((double)DistanceVector[2],2.0) );

      m_DistanceMatrix->put(counter1,counter2,distance);
  
      counter2++;
      }
    counter2 = 0;
    counter1++;
    }
  itkDebugMacro(<< "Finished CoreAtomImageToDistanceMatrixProcess\n");
}

/**
 * Print Self
 */
template< typename TSourceImage >
void
CoreAtomImageToDistanceMatrixProcess< TSourceImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace

#endif
