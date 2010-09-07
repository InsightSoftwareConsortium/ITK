/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorErodeDilateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorErodeDilateImageFilter_txx
#define __itkAnchorErodeDilateImageFilter_txx

#include "itkAnchorErodeDilateImageFilter.h"

//#include "itkNeighborhoodAlgorithm.h"

#include "itkAnchorUtilities.h"
namespace itk
{
template< class TImage, class TKernel, class TFunction1, class TFunction2 >
AnchorErodeDilateImageFilter< TImage, TKernel, TFunction1, TFunction2 >
::AnchorErodeDilateImageFilter()
{
  m_KernelSet = false;
}

template< class TImage, class TKernel, class TFunction1, class TFunction2 >
void
AnchorErodeDilateImageFilter< TImage, TKernel, TFunction1, TFunction2 >
::SetBoundary(const InputImagePixelType value)
{
  m_Boundary = value;
}

template< class TImage, class TKernel, class TFunction1, class TFunction2 >
void
AnchorErodeDilateImageFilter< TImage, TKernel, TFunction1, TFunction2 >
::ThreadedGenerateData(const InputImageRegionType & outputRegionForThread,
                       int threadId)
{
  // check that we are using a decomposable kernel
  if ( !m_Kernel.GetDecomposable() )
    {
    itkExceptionMacro("Anchor morphology only works with decomposable structuring elements");
    return;
    }
  if ( !m_KernelSet )
    {
    itkExceptionMacro("No kernel set - quitting");
    return;
    }
  // TFunction1 will be < for erosions
  // TFunction2 will be <=

  AnchorLineType AnchorLine;

  // the initial version will adopt the methodology of loading a line
  // at a time into a buffer vector, carrying out the opening or
  // closing, and then copy the result to the output. Hopefully this
  // will improve cache performance when working along non raster
  // directions.

  ProgressReporter progress(this, threadId, m_Kernel.GetLines().size() + 1);

  InputImageConstPointer input = this->GetInput();

  InputImageRegionType IReg = outputRegionForThread;
  IReg.PadByRadius( m_Kernel.GetRadius() );
  IReg.Crop( this->GetInput()->GetRequestedRegion() );

  // allocate an internal buffer
  typename InputImageType::Pointer internalbuffer = InputImageType::New();
  internalbuffer->SetRegions(IReg);
  internalbuffer->Allocate();
  InputImagePointer output = internalbuffer;

  // get the region size
  InputImageRegionType OReg = outputRegionForThread;
  // maximum buffer length is sum of dimensions
  unsigned int bufflength = 0;
  for ( unsigned i = 0; i < TImage::ImageDimension; i++ )
    {
    bufflength += IReg.GetSize()[i];
    }

  // compat
  bufflength += 2;

  InputImagePixelType *buffer = new InputImagePixelType[bufflength];
  InputImagePixelType *inbuffer = new InputImagePixelType[bufflength];

  // iterate over all the structuring elements
  typename KernelType::DecompType decomposition = m_Kernel.GetLines();
  BresType BresLine;

  for ( unsigned i = 0; i < decomposition.size(); i++ )
    {
    typename KernelType::LType ThisLine = decomposition[i];
    typename BresType::OffsetArray TheseOffsets = BresLine.BuildLine(ThisLine, bufflength);

    typedef typename KernelType::LType KernelLType;

    unsigned int SELength = GetLinePixels< KernelLType >(ThisLine);

    // want lines to be odd
    if ( !( SELength % 2 ) )
      {
      ++SELength;
      }

    InputImageRegionType BigFace = MakeEnlargedFace< InputImageType, KernelLType >(input, IReg, ThisLine);

    AnchorLine.SetSize(SELength);

    DoAnchorFace< TImage, BresType, AnchorLineType, KernelLType >(
      input,
      output,
      m_Boundary,
      ThisLine,
      AnchorLine,
      TheseOffsets,
      inbuffer,
      buffer,
      IReg,
      BigFace
      );
    // after the first pass the input will be taken from the output
    input = internalbuffer;
    progress.CompletedPixel();
    }

  // copy internal buffer to output
  typedef ImageRegionIterator< InputImageType > IterType;
  IterType oit(this->GetOutput(), OReg);
  IterType iit(internalbuffer, OReg);
  for ( oit.GoToBegin(), iit.GoToBegin(); !oit.IsAtEnd(); ++oit, ++iit )
    {
    oit.Set( iit.Get() );
    }
  progress.CompletedPixel();
  delete[] buffer;
  delete[] inbuffer;
}

template< class TImage, class TKernel, class TFunction1, class TFunction2 >
void
AnchorErodeDilateImageFilter< TImage, TKernel, TFunction1, TFunction2 >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Boundary: " << m_Boundary << std::endl;
}

template< class TImage, class TKernel, class TFunction1, class TFunction2 >
void
AnchorErodeDilateImageFilter< TImage, TKernel, TFunction1, TFunction2 >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( m_Kernel.GetRadius() );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    std::ostringstream          msg;
    msg << static_cast< const char * >( this->GetNameOfClass() )
        << "::GenerateInputRequestedRegion()";
    e.SetLocation( msg.str().c_str() );
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}
} // end namespace itk

#endif
