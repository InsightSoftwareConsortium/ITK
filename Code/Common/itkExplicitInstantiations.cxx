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
#ifdef ITK_EXPLICIT_INSTANTIATION  //This is a strange construct, only compile this file if ITK_EXPLICIT_INSTANTIATION is defined
#undef ITK_EXPLICIT_INSTANTIATION  //and this file requires that the headers are included with ITK_EXPLICIT_INSTANTIATION undefined!!
//NOTE: If this file is compiled, then this flag must not have been already defined.
//The ITK_EXPLICIT_INSTANTIATION flag turns off the  instantiation in the itkImage.h so that these classes are built once.  With out undef'ing ITK_EXPLICIT_INSTANTIATION in this file, the compiler will refuse to instatiate the objects, expeting that to be done elsewhere.


//The SGI compiler requires #pragma commands to infect the whole system to get htis to work.

//This seems to work really well on the gcc compilers.

//The example for doing this was taken from the VC6.0 documentation, so I assume that it would work there also.

#include "itkSize.h"
#include "itkIndex.h"
#include "itkImageBase.h"
#include "itkImage.h"
#include "itkImageSource.h"
#include "itkImageToImageFilter.h"
#include "itkPoint.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
   //Compilations from itkSize.h
   template class Size<1>;
   template class Size<2>;
   template class Size<3>;
   template class Size<4>;
   template class Size<5>;
   template std::ostream & operator<<(std::ostream &os, const Size<1> &size);
   template std::ostream & operator<<(std::ostream &os, const Size<2> &size);
   template std::ostream & operator<<(std::ostream &os, const Size<3> &size);
   template std::ostream & operator<<(std::ostream &os, const Size<4> &size);
   template std::ostream & operator<<(std::ostream &os, const Size<5> &size);

   //Compilations from itkIndex.h
   template class Index<1>;
   template class Index<2>;
   template class Index<3>;
   template class Index<4>;
   template class Index<5>;
   template std::ostream & operator<<(std::ostream &os, const Index<1> &ind);
   template std::ostream & operator<<(std::ostream &os, const Index<2> &ind);
   template std::ostream & operator<<(std::ostream &os, const Index<3> &ind);
   template std::ostream & operator<<(std::ostream &os, const Index<4> &ind);
   template std::ostream & operator<<(std::ostream &os, const Index<5> &ind);

   //Compilations from itkImageBase.h
   template class ImageBase<2>;
   template class ImageBase<3>;
   template class ImageBase<4>;
   template class ImageBase<5>;

   //Compilations from itkImage.h
   template class Image<float         ,2>;
   template class Image<double        ,2>;
   template class Image<unsigned char ,2>;
   template class Image<unsigned short,2>;
   template class Image<unsigned int  ,2>;
   template class Image<signed char   ,2>;
   template class Image<signed short  ,2>;
   template class Image<signed int    ,2>;
   template class Image<float         ,3>;
   template class Image<double        ,3>;
   template class Image<unsigned char ,3>;
   template class Image<unsigned short,3>;
   template class Image<unsigned int  ,3>;
   template class Image<signed char   ,3>;
   template class Image<signed short  ,3>;
   template class Image<signed int    ,3>;

   //Compilations from itkImageSource.h
   template class ImageSource<Image<float         ,2> >;
   template class ImageSource<Image<double        ,2> >;
   template class ImageSource<Image<unsigned char ,2> >;
   template class ImageSource<Image<unsigned short,2> >;
   template class ImageSource<Image<unsigned int  ,2> >;
   template class ImageSource<Image<signed char   ,2> >;
   template class ImageSource<Image<signed short  ,2> >;
   template class ImageSource<Image<signed int    ,2> >;
   template class ImageSource<Image<float         ,3> >;
   template class ImageSource<Image<double        ,3> >;
   template class ImageSource<Image<unsigned char ,3> >;
   template class ImageSource<Image<unsigned short,3> >;
   template class ImageSource<Image<unsigned int  ,3> >;
   template class ImageSource<Image<signed char   ,3> >;
   template class ImageSource<Image<signed short  ,3> >;
   template class ImageSource<Image<signed int    ,3> >;

   //Compilations from itkImageToImageFilter.h
   template class ImageToImageFilter<Image<float         ,2>, Image<float         ,2> >;
   template class ImageToImageFilter<Image<double        ,2>, Image<double        ,2> >;
   template class ImageToImageFilter<Image<unsigned char ,2>, Image<unsigned char ,2> >;
   template class ImageToImageFilter<Image<unsigned short,2>, Image<unsigned short,2> >;
   template class ImageToImageFilter<Image<signed char   ,2>, Image<signed char   ,2> >;
   template class ImageToImageFilter<Image<signed short  ,2>, Image<signed short  ,2> >;
   template class ImageToImageFilter<Image<signed int    ,2>, Image<signed int    ,2> >;

   template class ImageToImageFilter<Image<double        ,2>, Image<float         ,2> >;
   template class ImageToImageFilter<Image<unsigned char ,2>, Image<float         ,2> >;
   template class ImageToImageFilter<Image<unsigned short,2>, Image<float         ,2> >;
   template class ImageToImageFilter<Image<signed char   ,2>, Image<float         ,2> >;
   template class ImageToImageFilter<Image<signed short  ,2>, Image<float         ,2> >;

   template class ImageToImageFilter<Image<float         ,2>, Image<double        ,2> >;
   template class ImageToImageFilter<Image<float         ,2>, Image<unsigned char ,2> >;
   template class ImageToImageFilter<Image<float         ,2>, Image<unsigned short,2> >;
   template class ImageToImageFilter<Image<float         ,2>, Image<signed char   ,2> >;
   template class ImageToImageFilter<Image<float         ,2>, Image<signed short  ,2> >;

   template class ImageToImageFilter<Image<float         ,3>, Image<float         ,3> >;
   template class ImageToImageFilter<Image<double        ,3>, Image<double        ,3> >;
   template class ImageToImageFilter<Image<unsigned char ,3>, Image<unsigned char ,3> >;
   template class ImageToImageFilter<Image<unsigned short,3>, Image<unsigned short,3> >;
   template class ImageToImageFilter<Image<signed char   ,3>, Image<signed char   ,3> >;
   template class ImageToImageFilter<Image<signed short  ,3>, Image<signed short  ,3> >;
   template class ImageToImageFilter<Image<signed int    ,3>, Image<signed int    ,3> >;

   template class ImageToImageFilter<Image<double        ,3>, Image<float         ,3> >;
   template class ImageToImageFilter<Image<unsigned char ,3>, Image<float         ,3> >;
   template class ImageToImageFilter<Image<unsigned short,3>, Image<float         ,3> >;
   template class ImageToImageFilter<Image<signed char   ,3>, Image<float         ,3> >;
   template class ImageToImageFilter<Image<signed short  ,3>, Image<float         ,3> >;

   template class ImageToImageFilter<Image<float         ,3>, Image<double        ,3> >;
   template class ImageToImageFilter<Image<float         ,3>, Image<unsigned char ,3> >;
   template class ImageToImageFilter<Image<float         ,3>, Image<unsigned short,3> >;
   template class ImageToImageFilter<Image<float         ,3>, Image<signed char   ,3> >;
   template class ImageToImageFilter<Image<float         ,3>, Image<signed short  ,3> >;

   //Compilations from itkPoint.h
   template class Point<float         ,2>;
   template class Point<double        ,2>;
   template class Point<float         ,3>;
   template class Point<double        ,3>;

   //Compilations from xxxx
} // end namespace itk

#endif
