/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteMetaImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkWriteMetaImage.h"
#include "itkObjectFactory.h"
#include <fstream>

namespace itk
{

/**
 *
 */
template <class TInputImage>
WriteMetaImage<TInputImage>
::WriteMetaImage()
{
  m_FileName = "";
}





/**
 *
 */
template <class TInputImage>
void 
WriteMetaImage<TInputImage>
::Execute(void)
{
  this->WriteData();  
}





/**
 *
 */
template <class TInputImage>
void 
WriteMetaImage<TInputImage>
::WriteData(void)
{
  
  const unsigned int BitsPerPixel = 0; // use default value for MET_UCHAR
  float * PixelSize = 0;               // instruct to ignore pixel size

  const unsigned int dimension = TInputImage::ImageDimension;

  typename TInputImage::Pointer m_InputImage( GetInput() );

  int dimSize[ dimension ];
  const unsigned long *size    = m_InputImage->GetBufferSize();
  const float         *spacing = m_InputImage->GetSpacing();
  
  const unsigned long bufferSize = 
                  m_InputImage->GetOffsetTable()[dimension];

  for(unsigned int i=0; i<dimension; i++) 
  {
    dimSize[i] = size[i];
  }

  typedef typename TInputImage::PixelType PixelType;
  
  PixelType *yetAnotherBuffer = new PixelType[ bufferSize ];

  typedef itk::ImageRegionSimpleIterator<typename TInputImage::PixelType,
                  TInputImage::ImageDimension> IteratorType;
  
  IteratorType it(m_InputImage,
                  m_InputImage->GetBufferStartIndex(),
                  m_InputImage->GetBufferSize() );

  
  PixelType * destination = yetAnotherBuffer;
  it.Begin();
  while( !it.IsAtEnd() ) 
  {
    *destination++ = *it; 
    ++it;
  }

  MetaImage saver(  dimension,
                    dimSize,
                    MET_SHORT,
                    spacing,
                    BitsPerPixel,
                    MET_SYSTEM_BYTE_ORDER_MSB,
                    yetAnotherBuffer);


  saver.Save( this->m_FileName.c_str(),0,1);

  delete [] yetAnotherBuffer;

  if(saver.Error())  {
    std::cerr << "Unable to open file:" << m_FileName << std::endl;
    return;
  }

}






/**
 *
 */
template <class TInputImage>
void 
WriteMetaImage<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  WriteImage<TInputImage>::PrintSelf(os,indent);
  os << indent << "Meta Image file format writer";
}


} // end namespace itk
