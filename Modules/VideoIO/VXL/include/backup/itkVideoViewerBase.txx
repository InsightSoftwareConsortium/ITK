/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoViewerBase.txx
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

#include "itkVideoViewerBase.h"
#include "itkOpenCVViewer.h"
#include "itkVXLViewer.h"

namespace itk
{

template< typename TImage >
VideoViewerBase< TImage >::VideoViewerBase()
{
}

template< typename TImage >
void VideoViewerBase< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

}; //namespace itk end