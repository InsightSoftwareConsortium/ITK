/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCoreAtomImageToUnaryCorrespondenceMatrixProcess.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCoreAtomImageToUnaryCorrespondenceMatrixProcess_txx
#define __itkCoreAtomImageToUnaryCorrespondenceMatrixProcess_txx

#include "itkCoreAtomImageToUnaryCorrespondenceMatrixProcess.h"

namespace itk
{

/**
 * Constructor.
 */
template< typename TSourceImage >
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::CoreAtomImageToUnaryCorrespondenceMatrixProcess()
{
  itkDebugMacro(<< "itkCoreAtomImageToUnaryCorrespondenceMatrixProcess::itkCoreAtomImageToUnaryCorrespondenceMatrixProcess() called");

  // Setting the output.
  CorrespondenceMatrixPointer output;
  output = static_cast<CoreAtomImageToUnaryCorrespondenceMatrixProcess::CorrespondenceMatrixType*>(this->MakeOutput(0).GetPointer());
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());

  // Initialize unary metric object.
  //m_Metric = UnaryMetricType::New();

  // Initialize flag to off by default.
  m_OutputPNG = false;
}

/**
 * Returns a pointer to the output cast as a Data Object.
 */
template< typename TSourceImage >
typename CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >::DataObjectPointer
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::MakeOutput(unsigned int)
{
  return static_cast<DataObject*>(CorrespondenceMatrixType::New().GetPointer());
}

/**
 * GetOutput.
 */
template< typename TSourceImage >
typename CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >::CorrespondenceMatrixType*
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return static_cast< CorrespondenceMatrixType * >
                     (this->ProcessObject::GetOutput(0));
}

/**
 * 
 */
template< typename TSourceImage >
void
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::SetInput1(const TSourceImage * image1 ) 
{
  itkDebugMacro(<< "CoreAtomImageToUnaryCorrespondenceMatrixProcess: Setting first core atom image input")

  // Process object is not const-correct so the const casting is required.
  SetNthInput(1,  const_cast<TSourceImage *>( image1 ) );
}

/**
 * 
 */
template< typename TSourceImage >
void
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::SetInput2(const TSourceImage * image2 ) 
{
  itkDebugMacro(<< "CoreAtomImageToUnaryCorrespondenceMatrixProcess: Setting second core atom image input")
  // Process object is not const-correct so the const casting is required.
  SetNthInput(0, const_cast<TSourceImage *>( image2 ) );
}

/**
 * Get first core atom image input.
 */
template< typename TSourceImage >
TSourceImage *
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::GetInput1() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<TSourceImage *>(GetNthInput(0));
}

/**
 * Get second core atom image input.
 */
template< typename TSourceImage >
TSourceImage *
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::GetInput2() 
{
  // Process object is not const-correct so the const casting is required.
  return const_cast<TSourceImage *>(GetNthInput(1));
}

/**
 * Run the unary metric on the two images and create the unary correspondence matrix.
 */
template< typename TSourceImage >
void
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::GenerateData()
{
  itkDebugMacro(<< "itkCoreAtomImageToUnaryCorrespondenceMatrixProcess::GenerateData() called");

  m_Metric = UnaryMetricType::New();

  // Pointers to the input core atom images, output matrix object.
  m_CoreAtomImageA = dynamic_cast<CoreAtomImageType*>(ProcessObject::GetInput(0));
  m_CoreAtomImageB = dynamic_cast<CoreAtomImageType*>(ProcessObject::GetInput(1));
  m_CorrespondenceMatrix = dynamic_cast<CorrespondenceMatrixType*>(ProcessObject::GetOutput(0));

  m_Rows = m_CoreAtomImageA->GetMedialNodeCount();
  m_Columns = m_CoreAtomImageB->GetMedialNodeCount();

  if(m_CorrespondenceMatrix->resize(m_Rows,m_Columns))
    {
    itkDebugMacro(<< "m_CorrespondenceMatrix resized successfully");
    }
  else
    {
    itkDebugMacro(<< "m_CorrespondenceMatrix resize failed");
    }

  // Here we iterate through all of the core atoms and get their metric values.
    
  // Create iterators that will walk the blox core atom images.
  typedef itk::ImageRegionIterator<CoreAtomImageType> BloxIterator;

  BloxIterator bloxItA = BloxIterator(m_CoreAtomImageA,
                                      m_CoreAtomImageA->GetRequestedRegion() );

  BloxIterator bloxItB = BloxIterator(m_CoreAtomImageB,
                                      m_CoreAtomImageB->GetRequestedRegion() );

  int counterA = 0;
  int counterB = 0;
  double MetricValue;

  // Initialize necessary components to write-out a PNG image of correspondance matrix.  
  // The image only gets created is the flag is set.
    typedef unsigned char CorrespondencePixelType;  
    typedef Image<CorrespondencePixelType, 2>  CorrespondenceImageType;

    CorrespondenceImageType::IndexType pixelIndex;
    CorrespondenceImageType::SizeType size;
    size[0] = m_Rows;  
    size[1] = m_Columns;
  
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

  // Iterate through nodes in m_CoreAtomImageA (rows).
  for ( bloxItA.GoToBegin(); !bloxItA.IsAtEnd(); ++bloxItA)
    {
    MedialNodeType* pPixelA = &bloxItA.Value();

    if( pPixelA->empty() )
      continue;

    // Iterate through nodes in m_CoreAtomImageB (columns)
    for ( bloxItB.GoToBegin(); !bloxItB.IsAtEnd(); ++bloxItB) //iterate through nodes in m_MedialWindowA
      {
      MedialNodeType* pPixelB = &bloxItB.Value();

      if( pPixelB->empty() )
        continue;

      m_Metric->SetMedialNodes(pPixelA, pPixelB);
      m_Metric->Initialize();
      MetricValue = m_Metric->GetResult();
      //MetricValue = 1;

      // Use m_CorrespondenceMatrix->put(row, column, value) to set matrix.
      m_CorrespondenceMatrix->put(counterA, counterB, MetricValue);

      pixelIndex[0] = counterA;
      pixelIndex[1] = counterB;

      if(m_OutputPNG)
        {
        CorrespondenceImage->SetPixel(pixelIndex, 
                  static_cast<CorrespondencePixelType>(100*m_CorrespondenceMatrix->get(counterA,counterB)) );
        }

      counterB++;
      }
    counterB = 0;
    counterA++;
    }

  // Write voting PNG image.
  if(m_OutputPNG)
    {
    std::cerr << "WROTE CORRESPONDANCE IMAGE TO CorrespondenceImage1.png" << std::endl;
    imageWriter->SetInput(CorrespondenceImage);
    imageWriter->SetFileName("CorrespondenceImage1.png");
    imageWriter->SetImageIO(io);
    imageWriter->Write();
    }
  itkDebugMacro(<< "Finished CoreAtomImageToUnaryCorrespondenceMatrixProcess\n");
}

/**
 * Print Self
 */
template< typename TSourceImage >
void
CoreAtomImageToUnaryCorrespondenceMatrixProcess< TSourceImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace

#endif
