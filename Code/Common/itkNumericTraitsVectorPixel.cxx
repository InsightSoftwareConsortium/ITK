/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVectorPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsVectorPixel.h"

namespace itk
{

const Vector<unsigned char, 2>  NumericTraits<Vector<unsigned char, 2> >::Zero = Vector<unsigned char, 2>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 2>  NumericTraits<Vector<unsigned char, 2> >::One = Vector<unsigned char, 2>( NumericTraits<unsigned char>::One );
const Vector<signed char, 2>  NumericTraits<Vector<signed char, 2> >::Zero = Vector<signed char, 2>( NumericTraits<signed char>::Zero );
const Vector<signed char, 2>  NumericTraits<Vector<signed char, 2> >::One = Vector<signed char, 2>( NumericTraits<signed char>::One );
const Vector<char, 2>  NumericTraits<Vector<char, 2> >::Zero = Vector<char, 2>( NumericTraits<char>::Zero );
const Vector<char, 2>  NumericTraits<Vector<char, 2> >::One = Vector<char, 2>( NumericTraits<char>::One );
const Vector<short, 2>  NumericTraits<Vector<short, 2> >::Zero = Vector<short, 2>( NumericTraits<short>::Zero );
const Vector<short, 2>  NumericTraits<Vector<short, 2> >::One = Vector<short, 2>( NumericTraits<short>::One );
const Vector<unsigned short, 2>  NumericTraits<Vector<unsigned short, 2> >::Zero = Vector<unsigned short, 2>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 2>  NumericTraits<Vector<unsigned short, 2> >::One = Vector<unsigned short, 2>( NumericTraits<unsigned short>::One );
const Vector<int, 2>  NumericTraits<Vector<int, 2> >::Zero = Vector<int, 2>( NumericTraits<int>::Zero );
const Vector<int, 2>  NumericTraits<Vector<int, 2> >::One = Vector<int, 2>( NumericTraits<int>::One );
const Vector<unsigned int, 2>  NumericTraits<Vector<unsigned int, 2> >::Zero = Vector<unsigned int, 2>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 2>  NumericTraits<Vector<unsigned int, 2> >::One = Vector<unsigned int, 2>( NumericTraits<unsigned int>::One );
const Vector<long, 2>  NumericTraits<Vector<long, 2> >::Zero = Vector<long, 2>( NumericTraits<long>::Zero );
const Vector<long, 2>  NumericTraits<Vector<long, 2> >::One = Vector<long, 2>( NumericTraits<long>::One );
const Vector<unsigned long, 2>  NumericTraits<Vector<unsigned long, 2> >::Zero = Vector<unsigned long, 2>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 2>  NumericTraits<Vector<unsigned long, 2> >::One = Vector<unsigned long, 2>( NumericTraits<unsigned long>::One );
const Vector<float, 2>  NumericTraits<Vector<float, 2> >::Zero = Vector<float, 2>( NumericTraits<float >::Zero );
const Vector<float, 2>  NumericTraits<Vector<float, 2> >::One = Vector<float, 2>( NumericTraits<float>::One );
const Vector<double, 2>  NumericTraits<Vector<double, 2> >::Zero = Vector<double , 2>( NumericTraits<double>::Zero );
const Vector<double, 2>  NumericTraits<Vector<double, 2> >::One = Vector<double, 2>( NumericTraits<double>::One );
const Vector<long double, 2>  NumericTraits<Vector<long double, 2> >::Zero = Vector<long double, 2>( NumericTraits<long double>::Zero );
const Vector<long double, 2>  NumericTraits<Vector<long double, 2> >::One = Vector<long double, 2>( NumericTraits<long double>::One );

const Vector<unsigned char, 3>  NumericTraits<Vector<unsigned char, 3> >::Zero = Vector<unsigned char, 3>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 3>  NumericTraits<Vector<unsigned char, 3> >::One = Vector<unsigned char, 3>( NumericTraits<unsigned char>::One );
const Vector<signed char, 3>  NumericTraits<Vector<signed char, 3> >::Zero = Vector<signed char, 3>( NumericTraits<signed char>::Zero );
const Vector<signed char, 3>  NumericTraits<Vector<signed char, 3> >::One = Vector<signed char, 3>( NumericTraits<signed char>::One );
const Vector<char, 3>  NumericTraits<Vector<char, 3> >::Zero = Vector<char, 3>( NumericTraits<char>::Zero );
const Vector<char, 3>  NumericTraits<Vector<char, 3> >::One = Vector<char, 3>( NumericTraits<char>::One );
const Vector<short, 3>  NumericTraits<Vector<short, 3> >::Zero = Vector<short, 3>( NumericTraits<short>::Zero );
const Vector<short, 3>  NumericTraits<Vector<short, 3> >::One = Vector<short, 3>( NumericTraits<short>::One );
const Vector<unsigned short, 3>  NumericTraits<Vector<unsigned short, 3> >::Zero = Vector<unsigned short, 3>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 3>  NumericTraits<Vector<unsigned short, 3> >::One = Vector<unsigned short, 3>( NumericTraits<unsigned short>::One );
const Vector<int, 3>  NumericTraits<Vector<int, 3> >::Zero = Vector<int, 3>( NumericTraits<int>::Zero );
const Vector<int, 3>  NumericTraits<Vector<int, 3> >::One = Vector<int, 3>( NumericTraits<int>::One );
const Vector<unsigned int, 3>  NumericTraits<Vector<unsigned int, 3> >::Zero = Vector<unsigned int, 3>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 3>  NumericTraits<Vector<unsigned int, 3> >::One = Vector<unsigned int, 3>( NumericTraits<unsigned int>::One );
const Vector<long, 3>  NumericTraits<Vector<long, 3> >::Zero = Vector<long, 3>( NumericTraits<long>::Zero );
const Vector<long, 3>  NumericTraits<Vector<long, 3> >::One = Vector<long, 3>( NumericTraits<long>::One );
const Vector<unsigned long, 3>  NumericTraits<Vector<unsigned long, 3> >::Zero = Vector<unsigned long, 3>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 3>  NumericTraits<Vector<unsigned long, 3> >::One = Vector<unsigned long, 3>( NumericTraits<unsigned long>::One );
const Vector<float, 3>  NumericTraits<Vector<float, 3> >::Zero = Vector<float, 3>( NumericTraits<float >::Zero );
const Vector<float, 3>  NumericTraits<Vector<float, 3> >::One = Vector<float, 3>( NumericTraits<float>::One );
const Vector<double, 3>  NumericTraits<Vector<double, 3> >::Zero = Vector<double , 3>( NumericTraits<double>::Zero );
const Vector<double, 3>  NumericTraits<Vector<double, 3> >::One = Vector<double, 3>( NumericTraits<double>::One );
const Vector<long double, 3>  NumericTraits<Vector<long double, 3> >::Zero = Vector<long double, 3>( NumericTraits<long double>::Zero );
const Vector<long double, 3>  NumericTraits<Vector<long double, 3> >::One = Vector<long double, 3>( NumericTraits<long double>::One );

const Vector<unsigned char, 4>  NumericTraits<Vector<unsigned char, 4> >::Zero = Vector<unsigned char, 4>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 4>  NumericTraits<Vector<unsigned char, 4> >::One = Vector<unsigned char, 4>( NumericTraits<unsigned char>::One );
const Vector<signed char, 4>  NumericTraits<Vector<signed char, 4> >::Zero = Vector<signed char, 4>( NumericTraits<signed char>::Zero );
const Vector<signed char, 4>  NumericTraits<Vector<signed char, 4> >::One = Vector<signed char, 4>( NumericTraits<signed char>::One );
const Vector<char, 4>  NumericTraits<Vector<char, 4> >::Zero = Vector<char, 4>( NumericTraits<char>::Zero );
const Vector<char, 4>  NumericTraits<Vector<char, 4> >::One = Vector<char, 4>( NumericTraits<char>::One );
const Vector<short, 4>  NumericTraits<Vector<short, 4> >::Zero = Vector<short, 4>( NumericTraits<short>::Zero );
const Vector<short, 4>  NumericTraits<Vector<short, 4> >::One = Vector<short, 4>( NumericTraits<short>::One );
const Vector<unsigned short, 4>  NumericTraits<Vector<unsigned short, 4> >::Zero = Vector<unsigned short, 4>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 4>  NumericTraits<Vector<unsigned short, 4> >::One = Vector<unsigned short, 4>( NumericTraits<unsigned short>::One );
const Vector<int, 4>  NumericTraits<Vector<int, 4> >::Zero = Vector<int, 4>( NumericTraits<int>::Zero );
const Vector<int, 4>  NumericTraits<Vector<int, 4> >::One = Vector<int, 4>( NumericTraits<int>::One );
const Vector<unsigned int, 4>  NumericTraits<Vector<unsigned int, 4> >::Zero = Vector<unsigned int, 4>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 4>  NumericTraits<Vector<unsigned int, 4> >::One = Vector<unsigned int, 4>( NumericTraits<unsigned int>::One );
const Vector<long, 4>  NumericTraits<Vector<long, 4> >::Zero = Vector<long, 4>( NumericTraits<long>::Zero );
const Vector<long, 4>  NumericTraits<Vector<long, 4> >::One = Vector<long, 4>( NumericTraits<long>::One );
const Vector<unsigned long, 4>  NumericTraits<Vector<unsigned long, 4> >::Zero = Vector<unsigned long, 4>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 4>  NumericTraits<Vector<unsigned long, 4> >::One = Vector<unsigned long, 4>( NumericTraits<unsigned long>::One );
const Vector<float, 4>  NumericTraits<Vector<float, 4> >::Zero = Vector<float, 4>( NumericTraits<float >::Zero );
const Vector<float, 4>  NumericTraits<Vector<float, 4> >::One = Vector<float, 4>( NumericTraits<float>::One );
const Vector<double, 4>  NumericTraits<Vector<double, 4> >::Zero = Vector<double , 4>( NumericTraits<double>::Zero );
const Vector<double, 4>  NumericTraits<Vector<double, 4> >::One = Vector<double, 4>( NumericTraits<double>::One );
const Vector<long double, 4>  NumericTraits<Vector<long double, 4> >::Zero = Vector<long double, 4>( NumericTraits<long double>::Zero );
const Vector<long double, 4>  NumericTraits<Vector<long double, 4> >::One = Vector<long double, 4>( NumericTraits<long double>::One );

const Vector<unsigned char, 5>  NumericTraits<Vector<unsigned char, 5> >::Zero = Vector<unsigned char, 5>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 5>  NumericTraits<Vector<unsigned char, 5> >::One = Vector<unsigned char, 5>( NumericTraits<unsigned char>::One );
const Vector<signed char, 5>  NumericTraits<Vector<signed char, 5> >::Zero = Vector<signed char, 5>( NumericTraits<signed char>::Zero );
const Vector<signed char, 5>  NumericTraits<Vector<signed char, 5> >::One = Vector<signed char, 5>( NumericTraits<signed char>::One );
const Vector<char, 5>  NumericTraits<Vector<char, 5> >::Zero = Vector<char, 5>( NumericTraits<char>::Zero );
const Vector<char, 5>  NumericTraits<Vector<char, 5> >::One = Vector<char, 5>( NumericTraits<char>::One );
const Vector<short, 5>  NumericTraits<Vector<short, 5> >::Zero = Vector<short, 5>( NumericTraits<short>::Zero );
const Vector<short, 5>  NumericTraits<Vector<short, 5> >::One = Vector<short, 5>( NumericTraits<short>::One );
const Vector<unsigned short, 5>  NumericTraits<Vector<unsigned short, 5> >::Zero = Vector<unsigned short, 5>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 5>  NumericTraits<Vector<unsigned short, 5> >::One = Vector<unsigned short, 5>( NumericTraits<unsigned short>::One );
const Vector<int, 5>  NumericTraits<Vector<int, 5> >::Zero = Vector<int, 5>( NumericTraits<int>::Zero );
const Vector<int, 5>  NumericTraits<Vector<int, 5> >::One = Vector<int, 5>( NumericTraits<int>::One );
const Vector<unsigned int, 5>  NumericTraits<Vector<unsigned int, 5> >::Zero = Vector<unsigned int, 5>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 5>  NumericTraits<Vector<unsigned int, 5> >::One = Vector<unsigned int, 5>( NumericTraits<unsigned int>::One );
const Vector<long, 5>  NumericTraits<Vector<long, 5> >::Zero = Vector<long, 5>( NumericTraits<long>::Zero );
const Vector<long, 5>  NumericTraits<Vector<long, 5> >::One = Vector<long, 5>( NumericTraits<long>::One );
const Vector<unsigned long, 5>  NumericTraits<Vector<unsigned long, 5> >::Zero = Vector<unsigned long, 5>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 5>  NumericTraits<Vector<unsigned long, 5> >::One = Vector<unsigned long, 5>( NumericTraits<unsigned long>::One );
const Vector<float, 5>  NumericTraits<Vector<float, 5> >::Zero = Vector<float, 5>( NumericTraits<float >::Zero );
const Vector<float, 5>  NumericTraits<Vector<float, 5> >::One = Vector<float, 5>( NumericTraits<float>::One );
const Vector<double, 5>  NumericTraits<Vector<double, 5> >::Zero = Vector<double , 5>( NumericTraits<double>::Zero );
const Vector<double, 5>  NumericTraits<Vector<double, 5> >::One = Vector<double, 5>( NumericTraits<double>::One );
const Vector<long double, 5>  NumericTraits<Vector<long double, 5> >::Zero = Vector<long double, 5>( NumericTraits<long double>::Zero );
const Vector<long double, 5>  NumericTraits<Vector<long double, 5> >::One = Vector<long double, 5>( NumericTraits<long double>::One );

const Vector<unsigned char, 6>  NumericTraits<Vector<unsigned char, 6> >::Zero = Vector<unsigned char, 6>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 6>  NumericTraits<Vector<unsigned char, 6> >::One = Vector<unsigned char, 6>( NumericTraits<unsigned char>::One );
const Vector<signed char, 6>  NumericTraits<Vector<signed char, 6> >::Zero = Vector<signed char, 6>( NumericTraits<signed char>::Zero );
const Vector<signed char, 6>  NumericTraits<Vector<signed char, 6> >::One = Vector<signed char, 6>( NumericTraits<signed char>::One );
const Vector<char, 6>  NumericTraits<Vector<char, 6> >::Zero = Vector<char, 6>( NumericTraits<char>::Zero );
const Vector<char, 6>  NumericTraits<Vector<char, 6> >::One = Vector<char, 6>( NumericTraits<char>::One );
const Vector<short, 6>  NumericTraits<Vector<short, 6> >::Zero = Vector<short, 6>( NumericTraits<short>::Zero );
const Vector<short, 6>  NumericTraits<Vector<short, 6> >::One = Vector<short, 6>( NumericTraits<short>::One );
const Vector<unsigned short, 6>  NumericTraits<Vector<unsigned short, 6> >::Zero = Vector<unsigned short, 6>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 6>  NumericTraits<Vector<unsigned short, 6> >::One = Vector<unsigned short, 6>( NumericTraits<unsigned short>::One );
const Vector<int, 6>  NumericTraits<Vector<int, 6> >::Zero = Vector<int, 6>( NumericTraits<int>::Zero );
const Vector<int, 6>  NumericTraits<Vector<int, 6> >::One = Vector<int, 6>( NumericTraits<int>::One );
const Vector<unsigned int, 6>  NumericTraits<Vector<unsigned int, 6> >::Zero = Vector<unsigned int, 6>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 6>  NumericTraits<Vector<unsigned int, 6> >::One = Vector<unsigned int, 6>( NumericTraits<unsigned int>::One );
const Vector<long, 6>  NumericTraits<Vector<long, 6> >::Zero = Vector<long, 6>( NumericTraits<long>::Zero );
const Vector<long, 6>  NumericTraits<Vector<long, 6> >::One = Vector<long, 6>( NumericTraits<long>::One );
const Vector<unsigned long, 6>  NumericTraits<Vector<unsigned long, 6> >::Zero = Vector<unsigned long, 6>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 6>  NumericTraits<Vector<unsigned long, 6> >::One = Vector<unsigned long, 6>( NumericTraits<unsigned long>::One );
const Vector<float, 6>  NumericTraits<Vector<float, 6> >::Zero = Vector<float, 6>( NumericTraits<float >::Zero );
const Vector<float, 6>  NumericTraits<Vector<float, 6> >::One = Vector<float, 6>( NumericTraits<float>::One );
const Vector<double, 6>  NumericTraits<Vector<double, 6> >::Zero = Vector<double , 6>( NumericTraits<double>::Zero );
const Vector<double, 6>  NumericTraits<Vector<double, 6> >::One = Vector<double, 6>( NumericTraits<double>::One );
const Vector<long double, 6>  NumericTraits<Vector<long double, 6> >::Zero = Vector<long double, 6>( NumericTraits<long double>::Zero );
const Vector<long double, 6>  NumericTraits<Vector<long double, 6> >::One = Vector<long double, 6>( NumericTraits<long double>::One );

const Vector<unsigned char, 7>  NumericTraits<Vector<unsigned char, 7> >::Zero = Vector<unsigned char, 7>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 7>  NumericTraits<Vector<unsigned char, 7> >::One = Vector<unsigned char, 7>( NumericTraits<unsigned char>::One );
const Vector<signed char, 7>  NumericTraits<Vector<signed char, 7> >::Zero = Vector<signed char, 7>( NumericTraits<signed char>::Zero );
const Vector<signed char, 7>  NumericTraits<Vector<signed char, 7> >::One = Vector<signed char, 7>( NumericTraits<signed char>::One );
const Vector<char, 7>  NumericTraits<Vector<char, 7> >::Zero = Vector<char, 7>( NumericTraits<char>::Zero );
const Vector<char, 7>  NumericTraits<Vector<char, 7> >::One = Vector<char, 7>( NumericTraits<char>::One );
const Vector<short, 7>  NumericTraits<Vector<short, 7> >::Zero = Vector<short, 7>( NumericTraits<short>::Zero );
const Vector<short, 7>  NumericTraits<Vector<short, 7> >::One = Vector<short, 7>( NumericTraits<short>::One );
const Vector<unsigned short, 7>  NumericTraits<Vector<unsigned short, 7> >::Zero = Vector<unsigned short, 7>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 7>  NumericTraits<Vector<unsigned short, 7> >::One = Vector<unsigned short, 7>( NumericTraits<unsigned short>::One );
const Vector<int, 7>  NumericTraits<Vector<int, 7> >::Zero = Vector<int, 7>( NumericTraits<int>::Zero );
const Vector<int, 7>  NumericTraits<Vector<int, 7> >::One = Vector<int, 7>( NumericTraits<int>::One );
const Vector<unsigned int, 7>  NumericTraits<Vector<unsigned int, 7> >::Zero = Vector<unsigned int, 7>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 7>  NumericTraits<Vector<unsigned int, 7> >::One = Vector<unsigned int, 7>( NumericTraits<unsigned int>::One );
const Vector<long, 7>  NumericTraits<Vector<long, 7> >::Zero = Vector<long, 7>( NumericTraits<long>::Zero );
const Vector<long, 7>  NumericTraits<Vector<long, 7> >::One = Vector<long, 7>( NumericTraits<long>::One );
const Vector<unsigned long, 7>  NumericTraits<Vector<unsigned long, 7> >::Zero = Vector<unsigned long, 7>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 7>  NumericTraits<Vector<unsigned long, 7> >::One = Vector<unsigned long, 7>( NumericTraits<unsigned long>::One );
const Vector<float, 7>  NumericTraits<Vector<float, 7> >::Zero = Vector<float, 7>( NumericTraits<float >::Zero );
const Vector<float, 7>  NumericTraits<Vector<float, 7> >::One = Vector<float, 7>( NumericTraits<float>::One );
const Vector<double, 7>  NumericTraits<Vector<double, 7> >::Zero = Vector<double , 7>( NumericTraits<double>::Zero );
const Vector<double, 7>  NumericTraits<Vector<double, 7> >::One = Vector<double, 7>( NumericTraits<double>::One );
const Vector<long double, 7>  NumericTraits<Vector<long double, 7> >::Zero = Vector<long double, 7>( NumericTraits<long double>::Zero );
const Vector<long double, 7>  NumericTraits<Vector<long double, 7> >::One = Vector<long double, 7>( NumericTraits<long double>::One );

const Vector<unsigned char, 8>  NumericTraits<Vector<unsigned char, 8> >::Zero = Vector<unsigned char, 8>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 8>  NumericTraits<Vector<unsigned char, 8> >::One = Vector<unsigned char, 8>( NumericTraits<unsigned char>::One );
const Vector<signed char, 8>  NumericTraits<Vector<signed char, 8> >::Zero = Vector<signed char, 8>( NumericTraits<signed char>::Zero );
const Vector<signed char, 8>  NumericTraits<Vector<signed char, 8> >::One = Vector<signed char, 8>( NumericTraits<signed char>::One );
const Vector<char, 8>  NumericTraits<Vector<char, 8> >::Zero = Vector<char, 8>( NumericTraits<char>::Zero );
const Vector<char, 8>  NumericTraits<Vector<char, 8> >::One = Vector<char, 8>( NumericTraits<char>::One );
const Vector<short, 8>  NumericTraits<Vector<short, 8> >::Zero = Vector<short, 8>( NumericTraits<short>::Zero );
const Vector<short, 8>  NumericTraits<Vector<short, 8> >::One = Vector<short, 8>( NumericTraits<short>::One );
const Vector<unsigned short, 8>  NumericTraits<Vector<unsigned short, 8> >::Zero = Vector<unsigned short, 8>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 8>  NumericTraits<Vector<unsigned short, 8> >::One = Vector<unsigned short, 8>( NumericTraits<unsigned short>::One );
const Vector<int, 8>  NumericTraits<Vector<int, 8> >::Zero = Vector<int, 8>( NumericTraits<int>::Zero );
const Vector<int, 8>  NumericTraits<Vector<int, 8> >::One = Vector<int, 8>( NumericTraits<int>::One );
const Vector<unsigned int, 8>  NumericTraits<Vector<unsigned int, 8> >::Zero = Vector<unsigned int, 8>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 8>  NumericTraits<Vector<unsigned int, 8> >::One = Vector<unsigned int, 8>( NumericTraits<unsigned int>::One );
const Vector<long, 8>  NumericTraits<Vector<long, 8> >::Zero = Vector<long, 8>( NumericTraits<long>::Zero );
const Vector<long, 8>  NumericTraits<Vector<long, 8> >::One = Vector<long, 8>( NumericTraits<long>::One );
const Vector<unsigned long, 8>  NumericTraits<Vector<unsigned long, 8> >::Zero = Vector<unsigned long, 8>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 8>  NumericTraits<Vector<unsigned long, 8> >::One = Vector<unsigned long, 8>( NumericTraits<unsigned long>::One );
const Vector<float, 8>  NumericTraits<Vector<float, 8> >::Zero = Vector<float, 8>( NumericTraits<float >::Zero );
const Vector<float, 8>  NumericTraits<Vector<float, 8> >::One = Vector<float, 8>( NumericTraits<float>::One );
const Vector<double, 8>  NumericTraits<Vector<double, 8> >::Zero = Vector<double , 8>( NumericTraits<double>::Zero );
const Vector<double, 8>  NumericTraits<Vector<double, 8> >::One = Vector<double, 8>( NumericTraits<double>::One );
const Vector<long double, 8>  NumericTraits<Vector<long double, 8> >::Zero = Vector<long double, 8>( NumericTraits<long double>::Zero );
const Vector<long double, 8>  NumericTraits<Vector<long double, 8> >::One = Vector<long double, 8>( NumericTraits<long double>::One );

const Vector<unsigned char, 9>  NumericTraits<Vector<unsigned char, 9> >::Zero = Vector<unsigned char, 9>( NumericTraits<unsigned char>::Zero );
const Vector<unsigned char, 9>  NumericTraits<Vector<unsigned char, 9> >::One = Vector<unsigned char, 9>( NumericTraits<unsigned char>::One );
const Vector<signed char, 9>  NumericTraits<Vector<signed char, 9> >::Zero = Vector<signed char, 9>( NumericTraits<signed char>::Zero );
const Vector<signed char, 9>  NumericTraits<Vector<signed char, 9> >::One = Vector<signed char, 9>( NumericTraits<signed char>::One );
const Vector<char, 9>  NumericTraits<Vector<char, 9> >::Zero = Vector<char, 9>( NumericTraits<char>::Zero );
const Vector<char, 9>  NumericTraits<Vector<char, 9> >::One = Vector<char, 9>( NumericTraits<char>::One );
const Vector<short, 9>  NumericTraits<Vector<short, 9> >::Zero = Vector<short, 9>( NumericTraits<short>::Zero );
const Vector<short, 9>  NumericTraits<Vector<short, 9> >::One = Vector<short, 9>( NumericTraits<short>::One );
const Vector<unsigned short, 9>  NumericTraits<Vector<unsigned short, 9> >::Zero = Vector<unsigned short, 9>( NumericTraits<unsigned short>::Zero );
const Vector<unsigned short, 9>  NumericTraits<Vector<unsigned short, 9> >::One = Vector<unsigned short, 9>( NumericTraits<unsigned short>::One );
const Vector<int, 9>  NumericTraits<Vector<int, 9> >::Zero = Vector<int, 9>( NumericTraits<int>::Zero );
const Vector<int, 9>  NumericTraits<Vector<int, 9> >::One = Vector<int, 9>( NumericTraits<int>::One );
const Vector<unsigned int, 9>  NumericTraits<Vector<unsigned int, 9> >::Zero = Vector<unsigned int, 9>( NumericTraits<unsigned int>::Zero );
const Vector<unsigned int, 9>  NumericTraits<Vector<unsigned int, 9> >::One = Vector<unsigned int, 9>( NumericTraits<unsigned int>::One );
const Vector<long, 9>  NumericTraits<Vector<long, 9> >::Zero = Vector<long, 9>( NumericTraits<long>::Zero );
const Vector<long, 9>  NumericTraits<Vector<long, 9> >::One = Vector<long, 9>( NumericTraits<long>::One );
const Vector<unsigned long, 9>  NumericTraits<Vector<unsigned long, 9> >::Zero = Vector<unsigned long, 9>( NumericTraits<unsigned long>::Zero );
const Vector<unsigned long, 9>  NumericTraits<Vector<unsigned long, 9> >::One = Vector<unsigned long, 9>( NumericTraits<unsigned long>::One );
const Vector<float, 9>  NumericTraits<Vector<float, 9> >::Zero = Vector<float, 9>( NumericTraits<float >::Zero );
const Vector<float, 9>  NumericTraits<Vector<float, 9> >::One = Vector<float, 9>( NumericTraits<float>::One );
const Vector<double, 9>  NumericTraits<Vector<double, 9> >::Zero = Vector<double , 9>( NumericTraits<double>::Zero );
const Vector<double, 9>  NumericTraits<Vector<double, 9> >::One = Vector<double, 9>( NumericTraits<double>::One );
const Vector<long double, 9>  NumericTraits<Vector<long double, 9> >::Zero = Vector<long double, 9>( NumericTraits<long double>::Zero );
const Vector<long double, 9>  NumericTraits<Vector<long double, 9> >::One = Vector<long double, 9>( NumericTraits<long double>::One );

} // end namespace itk
