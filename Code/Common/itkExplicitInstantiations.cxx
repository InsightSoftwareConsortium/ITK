/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExplicitInstantiations.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#define ITK_NOEXPLICIT_INSTANTIATION
#define ITK_EXPLICIT
//NOTE: If this file is compiled, then this flag must not have been already defined.
//The ITK_NOEXPLICIT_INSTANTIATION flag turns off the ITK_EXPLICIT instantiation in the itkImage.h so that these files are build once.
//This should not be necessary with he ITK_EXPLICIT tag
#include "itkImageBase.h"
#include "itkImage.h"
#include "itkImageSource.h"
#include "itkPoint.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
  ITK_EXPLICIT template class ImageBase<2>;
  ITK_EXPLICIT template class ImageBase<3>;

  ITK_EXPLICIT template class Image<float         ,2>;
  ITK_EXPLICIT template class Image<double        ,2>;
  ITK_EXPLICIT template class Image<unsigned char ,2>;
  ITK_EXPLICIT template class Image<unsigned short,2>;
  ITK_EXPLICIT template class Image<unsigned int  ,2>;
  ITK_EXPLICIT template class Image<signed char   ,2>;
  ITK_EXPLICIT template class Image<signed short  ,2>;
  ITK_EXPLICIT template class Image<signed int    ,2>;
  ITK_EXPLICIT template class Image<float         ,3>;
  ITK_EXPLICIT template class Image<double        ,3>;
  ITK_EXPLICIT template class Image<unsigned char ,3>;
  ITK_EXPLICIT template class Image<unsigned short,3>;
  ITK_EXPLICIT template class Image<unsigned int  ,3>;
  ITK_EXPLICIT template class Image<signed char   ,3>;
  ITK_EXPLICIT template class Image<signed short  ,3>;
  ITK_EXPLICIT template class Image<signed int    ,3>;

  ITK_EXPLICIT template class ImageSource<Image<float         ,2> >;
  ITK_EXPLICIT template class ImageSource<Image<double        ,2> >;
  ITK_EXPLICIT template class ImageSource<Image<unsigned char ,2> >;
  ITK_EXPLICIT template class ImageSource<Image<unsigned short,2> >;
  ITK_EXPLICIT template class ImageSource<Image<unsigned int  ,2> >;
  ITK_EXPLICIT template class ImageSource<Image<signed char   ,2> >;
  ITK_EXPLICIT template class ImageSource<Image<signed short  ,2> >;
  ITK_EXPLICIT template class ImageSource<Image<signed int    ,2> >;
  ITK_EXPLICIT template class ImageSource<Image<float         ,3> >;
  ITK_EXPLICIT template class ImageSource<Image<double        ,3> >;
  ITK_EXPLICIT template class ImageSource<Image<unsigned char ,3> >;
  ITK_EXPLICIT template class ImageSource<Image<unsigned short,3> >;
  ITK_EXPLICIT template class ImageSource<Image<unsigned int  ,3> >;
  ITK_EXPLICIT template class ImageSource<Image<signed char   ,3> >;
  ITK_EXPLICIT template class ImageSource<Image<signed short  ,3> >;
  ITK_EXPLICIT template class ImageSource<Image<signed int    ,3> >;

  ITK_EXPLICIT template class Point<float         ,2>;
  ITK_EXPLICIT template class Point<double        ,2>;
  ITK_EXPLICIT template class Point<float         ,3>;
  ITK_EXPLICIT template class Point<double        ,3>;

} // end namespace itk

