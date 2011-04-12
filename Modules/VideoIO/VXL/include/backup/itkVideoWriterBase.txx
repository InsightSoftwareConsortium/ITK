/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoWriterBase.cxx
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

#include "itkVideoWriterBase.h"
#include "itkOpenCVWriter.h"
#include "itkVXLWriter.h"

namespace itk
{

template< typename TImage >
VideoWriterBase< TImage >::VideoWriterBase()
{
}

template< typename TImage >
void VideoWriterBase< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

}; //namespace itk end