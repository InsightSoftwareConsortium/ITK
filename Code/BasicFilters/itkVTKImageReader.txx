/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkVTKImageReader.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TOutputImage>
VTKImageReader<TOutputImage>
::VTKImageReader()
{
  m_FileName = "";
}


/**
 *
 */
template <class TOutputImage>
void 
VTKImageReader<TOutputImage>
::GenerateData()
{
}


/**
 *
 */
template <class TOutputImage>
void 
VTKImageReader<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

}

} // end namespace itk
