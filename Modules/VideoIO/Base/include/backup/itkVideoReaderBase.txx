/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoReaderBase.yxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined( _MSC_VER )
#pragma warning ( disable : 4786 )
#endif

#include "itkImageIOBase.h"
#include "itkOpenCVReader.h"
#include "itkVXLReader.h"

namespace itk
{

template< typename TImage >
VideoReaderBase< TImage >::VideoReaderBase()
{
}

template< typename TImage >
void VideoReaderBase< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

}; //namespace itk end