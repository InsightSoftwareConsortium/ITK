/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImportImageFilter.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
ImportImageFilter<TPixel, VImageDimension>
::ImportImageFilter()
{
  unsigned int idx;
  
  for (idx = 0; idx < VImageDimension; ++idx)
    {
    m_Spacing[idx] = 1.0;
    m_Origin[idx] = 0.0;
    }
}

/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
ImportImageFilter<TPixel, VImageDimension>
::~ImportImageFilter()
{
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  //  os << indent << "Shrink Factor: " << m_ShrinkFactor << std::endl;
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::SetImportPointer(TPixel *ptr, unsigned long num, bool LetSourceManageMemory)
{
  OutputImagePointer outputPtr = this->GetOutput();

  // check to see if this will cause the filter to be modified
  if (ptr != outputPtr->GetPixelContainer()->GetImportPointer())
    {
    outputPtr->GetPixelContainer()
      ->SetImportPointer( ptr, num, LetSourceManageMemory );
    this->Modified();
    }
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
TPixel *
ImportImageFilter<TPixel, VImageDimension>
::GetImportPointer()
{
  OutputImagePointer outputPtr = this->GetOutput();

  return outputPtr->GetImportPointer();
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::EnlargeOutputRequestedRegion(DataObject *output) 
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // get pointer to the output
  OutputImagePointer outputPtr = this->GetOutput();

  // set the requested region to the largest possible region (in this case
  // the amount of data that we have)
  outputPtr->SetRequestedRegion( outputPtr->GetLargestPossibleRegion() );
}


/** 
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointer to the output
  OutputImagePointer outputPtr = this->GetOutput();

  // we need to compute the output spacing, the output origin, the
  // output image size, and the output image start index
  outputPtr->SetSpacing( m_Spacing );
  outputPtr->SetOrigin( m_Origin );
  outputPtr->SetLargestPossibleRegion( m_Region );
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::GenerateData()
{
  // Normally, GenerateData() allocates memory.  However, the application
  // provides the memory for this filter via the SetImportPointer() method.
  // Therefore, this filter does not call outputPtr->Allocate().
  
  // get pointer to the output
  OutputImagePointer outputPtr = this->GetOutput();

  // the output buffer size is set to the size specified by the user via the
  // SetRegion() method.
  outputPtr->SetBufferedRegion( outputPtr->GetLargestPossibleRegion() );
}


} // end namespace itk
