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

}

#include "itkIndex.h"
namespace itk
{
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
}

#include "itkImageBase.h"
namespace itk
{
    //Compilations from itkImageBase.h
    template class ImageBase<2>;
    template class ImageBase<3>;
    template class ImageBase<4>;
    template class ImageBase<5>;
}

#include "itkImage.h"
namespace itk
{
    //Compilations from itkImage.h
    template class Image<float         ,2>;
    template class Image<double        ,2>;
    template class Image<unsigned char ,2>;
    template class Image<unsigned short int,2>;
    template class Image<unsigned int  ,2>;
    template class Image<signed char   ,2>;
    template class Image<signed short int  ,2>;
    template class Image<signed int    ,2>;
    template class Image<float         ,3>;
    template class Image<double        ,3>;
    template class Image<unsigned char ,3>;
    template class Image<unsigned short int,3>;
    template class Image<unsigned int  ,3>;
    template class Image<signed char   ,3>;
    template class Image<signed short int  ,3>;
    template class Image<signed int    ,3>;
}

#include "itkImageSource.h"
namespace itk
{
    //Compilations from itkImageSource.h
    template class ImageSource<Image<float         ,2> >;
    template class ImageSource<Image<double        ,2> >;
    template class ImageSource<Image<unsigned char ,2> >;
    template class ImageSource<Image<unsigned short int,2> >;
    template class ImageSource<Image<unsigned int  ,2> >;
    template class ImageSource<Image<signed char   ,2> >;
    template class ImageSource<Image<signed short int  ,2> >;
    template class ImageSource<Image<signed int    ,2> >;
    template class ImageSource<Image<float         ,3> >;
    template class ImageSource<Image<double        ,3> >;
    template class ImageSource<Image<unsigned char ,3> >;
    template class ImageSource<Image<unsigned short int,3> >;
    template class ImageSource<Image<unsigned int  ,3> >;
    template class ImageSource<Image<signed char   ,3> >;
    template class ImageSource<Image<signed short int  ,3> >;
    template class ImageSource<Image<signed int    ,3> >;
}

#include "itkImageToImageFilter.h"
namespace itk
{
    //Compilations from itkImageToImageFilter.h
    template class ImageToImageFilter<Image<float         ,2>, Image<float         ,2> >;
    template class ImageToImageFilter<Image<double        ,2>, Image<double        ,2> >;
    template class ImageToImageFilter<Image<unsigned char ,2>, Image<unsigned char ,2> >;
    template class ImageToImageFilter<Image<unsigned short int,2>, Image<unsigned short int,2> >;
    template class ImageToImageFilter<Image<signed char   ,2>, Image<signed char   ,2> >;
    template class ImageToImageFilter<Image<signed short int  ,2>, Image<signed short int  ,2> >;
    template class ImageToImageFilter<Image<signed int    ,2>, Image<signed int    ,2> >;

    template class ImageToImageFilter<Image<double        ,2>, Image<float         ,2> >;
    template class ImageToImageFilter<Image<unsigned char ,2>, Image<float         ,2> >;
    template class ImageToImageFilter<Image<unsigned short int,2>, Image<float         ,2> >;
    template class ImageToImageFilter<Image<signed char   ,2>, Image<float         ,2> >;
    template class ImageToImageFilter<Image<signed short int  ,2>, Image<float         ,2> >;

    template class ImageToImageFilter<Image<float         ,2>, Image<double        ,2> >;
    template class ImageToImageFilter<Image<float         ,2>, Image<unsigned char ,2> >;
    template class ImageToImageFilter<Image<float         ,2>, Image<unsigned short int,2> >;
    template class ImageToImageFilter<Image<float         ,2>, Image<signed char   ,2> >;
    template class ImageToImageFilter<Image<float         ,2>, Image<signed short int  ,2> >;

    template class ImageToImageFilter<Image<float         ,3>, Image<float         ,3> >;
    template class ImageToImageFilter<Image<double        ,3>, Image<double        ,3> >;
    template class ImageToImageFilter<Image<unsigned char ,3>, Image<unsigned char ,3> >;
    template class ImageToImageFilter<Image<unsigned short int,3>, Image<unsigned short int,3> >;
    template class ImageToImageFilter<Image<signed char   ,3>, Image<signed char   ,3> >;
    template class ImageToImageFilter<Image<signed short int  ,3>, Image<signed short int  ,3> >;
    template class ImageToImageFilter<Image<signed int    ,3>, Image<signed int    ,3> >;

    template class ImageToImageFilter<Image<double        ,3>, Image<float         ,3> >;
    template class ImageToImageFilter<Image<unsigned char ,3>, Image<float         ,3> >;
    template class ImageToImageFilter<Image<unsigned short int,3>, Image<float         ,3> >;
    template class ImageToImageFilter<Image<signed char   ,3>, Image<float         ,3> >;
    template class ImageToImageFilter<Image<signed short int  ,3>, Image<float         ,3> >;

    template class ImageToImageFilter<Image<float         ,3>, Image<double        ,3> >;
    template class ImageToImageFilter<Image<float         ,3>, Image<unsigned char ,3> >;
    template class ImageToImageFilter<Image<float         ,3>, Image<unsigned short int,3> >;
    template class ImageToImageFilter<Image<float         ,3>, Image<signed char   ,3> >;
    template class ImageToImageFilter<Image<float         ,3>, Image<signed short int  ,3> >;
}

#include "itkPoint.h"
namespace itk
{
    //Compilations from itkPoint.h
    template class Point<float         ,2>;
    template class Point<double        ,2>;
    template class Point<float         ,3>;
    template class Point<double        ,3>;
}

#include "itkPhasedArray3DSpecialCoordinatesImage.h"
namespace itk
{
    template class PhasedArray3DSpecialCoordinatesImage<float         >;
    template class PhasedArray3DSpecialCoordinatesImage<double        >;
    template class PhasedArray3DSpecialCoordinatesImage<unsigned char >;
    template class PhasedArray3DSpecialCoordinatesImage<unsigned short>;
    template class PhasedArray3DSpecialCoordinatesImage<unsigned int  >;
    template class PhasedArray3DSpecialCoordinatesImage<signed char   >;
    template class PhasedArray3DSpecialCoordinatesImage<signed short  >;
    template class PhasedArray3DSpecialCoordinatesImage<signed int    >;
}

#include "itkSpecialCoordinatesImage.h"
namespace itk
{
    template class SpecialCoordinatesImage<float         ,2>;
    template class SpecialCoordinatesImage<double        ,2>;
    template class SpecialCoordinatesImage<unsigned char ,2>;
    template class SpecialCoordinatesImage<unsigned short,2>;
    template class SpecialCoordinatesImage<unsigned int  ,2>;
    template class SpecialCoordinatesImage<signed char   ,2>;
    template class SpecialCoordinatesImage<signed short  ,2>;
    template class SpecialCoordinatesImage<signed int    ,2>;
    template class SpecialCoordinatesImage<float         ,3>;
    template class SpecialCoordinatesImage<double        ,3>;
    template class SpecialCoordinatesImage<unsigned char ,3>;
    template class SpecialCoordinatesImage<unsigned short,3>;
    template class SpecialCoordinatesImage<unsigned int  ,3>;
    template class SpecialCoordinatesImage<signed char   ,3>;
    template class SpecialCoordinatesImage<signed short  ,3>;
    template class SpecialCoordinatesImage<signed int    ,3>;
}

//#include "itkConstNeighborhoodIterator.h"
//Compilations from xxxx

#endif
