/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReadVTKImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkReadVTKImage.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TOutputImage>
ReadVTKImage<TOutputImage>
::ReadVTKImage()
{
  m_FileName = "";
}


/**
 *
 */
template <class TOutputImage>
void 
ReadVTKImage<TOutputImage>
::GenerateData()
{
}


/**
 *
 */
template <class TOutputImage>
void 
ReadVTKImage<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageSource<TOutputImage>::PrintSelf(os,indent);

}

} // end namespace itk
