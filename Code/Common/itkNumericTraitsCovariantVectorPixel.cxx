/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsCovariantVectorPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsCovariantVectorPixel.h"

namespace itk
{

const CovariantVector<unsigned char, 2>  NumericTraits<CovariantVector<unsigned char, 2> >::Zero = CovariantVector<unsigned char, 2>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 2>  NumericTraits<CovariantVector<unsigned char, 2> >::One = CovariantVector<unsigned char, 2>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 2>  NumericTraits<CovariantVector<signed char, 2> >::Zero = CovariantVector<signed char, 2>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 2>  NumericTraits<CovariantVector<signed char, 2> >::One = CovariantVector<signed char, 2>( NumericTraits<signed char>::One );
const CovariantVector<char, 2>  NumericTraits<CovariantVector<char, 2> >::Zero = CovariantVector<char, 2>( NumericTraits<char>::Zero );
const CovariantVector<char, 2>  NumericTraits<CovariantVector<char, 2> >::One = CovariantVector<char, 2>( NumericTraits<char>::One );
const CovariantVector<short, 2>  NumericTraits<CovariantVector<short, 2> >::Zero = CovariantVector<short, 2>( NumericTraits<short>::Zero );
const CovariantVector<short, 2>  NumericTraits<CovariantVector<short, 2> >::One = CovariantVector<short, 2>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 2>  NumericTraits<CovariantVector<unsigned short, 2> >::Zero = CovariantVector<unsigned short, 2>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 2>  NumericTraits<CovariantVector<unsigned short, 2> >::One = CovariantVector<unsigned short, 2>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 2>  NumericTraits<CovariantVector<int, 2> >::Zero = CovariantVector<int, 2>( NumericTraits<int>::Zero );
const CovariantVector<int, 2>  NumericTraits<CovariantVector<int, 2> >::One = CovariantVector<int, 2>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 2>  NumericTraits<CovariantVector<unsigned int, 2> >::Zero = CovariantVector<unsigned int, 2>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 2>  NumericTraits<CovariantVector<unsigned int, 2> >::One = CovariantVector<unsigned int, 2>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 2>  NumericTraits<CovariantVector<long, 2> >::Zero = CovariantVector<long, 2>( NumericTraits<long>::Zero );
const CovariantVector<long, 2>  NumericTraits<CovariantVector<long, 2> >::One = CovariantVector<long, 2>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 2>  NumericTraits<CovariantVector<unsigned long, 2> >::Zero = CovariantVector<unsigned long, 2>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 2>  NumericTraits<CovariantVector<unsigned long, 2> >::One = CovariantVector<unsigned long, 2>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 2>  NumericTraits<CovariantVector<float, 2> >::Zero = CovariantVector<float, 2>( NumericTraits<float >::Zero );
const CovariantVector<float, 2>  NumericTraits<CovariantVector<float, 2> >::One = CovariantVector<float, 2>( NumericTraits<float>::One );
const CovariantVector<double, 2>  NumericTraits<CovariantVector<double, 2> >::Zero = CovariantVector<double , 2>( NumericTraits<double>::Zero );
const CovariantVector<double, 2>  NumericTraits<CovariantVector<double, 2> >::One = CovariantVector<double, 2>( NumericTraits<double>::One );
const CovariantVector<long double, 2>  NumericTraits<CovariantVector<long double, 2> >::Zero = CovariantVector<long double, 2>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 2>  NumericTraits<CovariantVector<long double, 2> >::One = CovariantVector<long double, 2>( NumericTraits<long double>::One );

const CovariantVector<unsigned char, 3>  NumericTraits<CovariantVector<unsigned char, 3> >::Zero = CovariantVector<unsigned char, 3>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 3>  NumericTraits<CovariantVector<unsigned char, 3> >::One = CovariantVector<unsigned char, 3>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 3>  NumericTraits<CovariantVector<signed char, 3> >::Zero = CovariantVector<signed char, 3>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 3>  NumericTraits<CovariantVector<signed char, 3> >::One = CovariantVector<signed char, 3>( NumericTraits<signed char>::One );
const CovariantVector<char, 3>  NumericTraits<CovariantVector<char, 3> >::Zero = CovariantVector<char, 3>( NumericTraits<char>::Zero );
const CovariantVector<char, 3>  NumericTraits<CovariantVector<char, 3> >::One = CovariantVector<char, 3>( NumericTraits<char>::One );
const CovariantVector<short, 3>  NumericTraits<CovariantVector<short, 3> >::Zero = CovariantVector<short, 3>( NumericTraits<short>::Zero );
const CovariantVector<short, 3>  NumericTraits<CovariantVector<short, 3> >::One = CovariantVector<short, 3>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 3>  NumericTraits<CovariantVector<unsigned short, 3> >::Zero = CovariantVector<unsigned short, 3>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 3>  NumericTraits<CovariantVector<unsigned short, 3> >::One = CovariantVector<unsigned short, 3>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 3>  NumericTraits<CovariantVector<int, 3> >::Zero = CovariantVector<int, 3>( NumericTraits<int>::Zero );
const CovariantVector<int, 3>  NumericTraits<CovariantVector<int, 3> >::One = CovariantVector<int, 3>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 3>  NumericTraits<CovariantVector<unsigned int, 3> >::Zero = CovariantVector<unsigned int, 3>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 3>  NumericTraits<CovariantVector<unsigned int, 3> >::One = CovariantVector<unsigned int, 3>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 3>  NumericTraits<CovariantVector<long, 3> >::Zero = CovariantVector<long, 3>( NumericTraits<long>::Zero );
const CovariantVector<long, 3>  NumericTraits<CovariantVector<long, 3> >::One = CovariantVector<long, 3>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 3>  NumericTraits<CovariantVector<unsigned long, 3> >::Zero = CovariantVector<unsigned long, 3>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 3>  NumericTraits<CovariantVector<unsigned long, 3> >::One = CovariantVector<unsigned long, 3>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 3>  NumericTraits<CovariantVector<float, 3> >::Zero = CovariantVector<float, 3>( NumericTraits<float >::Zero );
const CovariantVector<float, 3>  NumericTraits<CovariantVector<float, 3> >::One = CovariantVector<float, 3>( NumericTraits<float>::One );
const CovariantVector<double, 3>  NumericTraits<CovariantVector<double, 3> >::Zero = CovariantVector<double , 3>( NumericTraits<double>::Zero );
const CovariantVector<double, 3>  NumericTraits<CovariantVector<double, 3> >::One = CovariantVector<double, 3>( NumericTraits<double>::One );
const CovariantVector<long double, 3>  NumericTraits<CovariantVector<long double, 3> >::Zero = CovariantVector<long double, 3>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 3>  NumericTraits<CovariantVector<long double, 3> >::One = CovariantVector<long double, 3>( NumericTraits<long double>::One );

const CovariantVector<unsigned char, 4>  NumericTraits<CovariantVector<unsigned char, 4> >::Zero = CovariantVector<unsigned char, 4>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 4>  NumericTraits<CovariantVector<unsigned char, 4> >::One = CovariantVector<unsigned char, 4>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 4>  NumericTraits<CovariantVector<signed char, 4> >::Zero = CovariantVector<signed char, 4>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 4>  NumericTraits<CovariantVector<signed char, 4> >::One = CovariantVector<signed char, 4>( NumericTraits<signed char>::One );
const CovariantVector<char, 4>  NumericTraits<CovariantVector<char, 4> >::Zero = CovariantVector<char, 4>( NumericTraits<char>::Zero );
const CovariantVector<char, 4>  NumericTraits<CovariantVector<char, 4> >::One = CovariantVector<char, 4>( NumericTraits<char>::One );
const CovariantVector<short, 4>  NumericTraits<CovariantVector<short, 4> >::Zero = CovariantVector<short, 4>( NumericTraits<short>::Zero );
const CovariantVector<short, 4>  NumericTraits<CovariantVector<short, 4> >::One = CovariantVector<short, 4>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 4>  NumericTraits<CovariantVector<unsigned short, 4> >::Zero = CovariantVector<unsigned short, 4>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 4>  NumericTraits<CovariantVector<unsigned short, 4> >::One = CovariantVector<unsigned short, 4>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 4>  NumericTraits<CovariantVector<int, 4> >::Zero = CovariantVector<int, 4>( NumericTraits<int>::Zero );
const CovariantVector<int, 4>  NumericTraits<CovariantVector<int, 4> >::One = CovariantVector<int, 4>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 4>  NumericTraits<CovariantVector<unsigned int, 4> >::Zero = CovariantVector<unsigned int, 4>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 4>  NumericTraits<CovariantVector<unsigned int, 4> >::One = CovariantVector<unsigned int, 4>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 4>  NumericTraits<CovariantVector<long, 4> >::Zero = CovariantVector<long, 4>( NumericTraits<long>::Zero );
const CovariantVector<long, 4>  NumericTraits<CovariantVector<long, 4> >::One = CovariantVector<long, 4>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 4>  NumericTraits<CovariantVector<unsigned long, 4> >::Zero = CovariantVector<unsigned long, 4>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 4>  NumericTraits<CovariantVector<unsigned long, 4> >::One = CovariantVector<unsigned long, 4>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 4>  NumericTraits<CovariantVector<float, 4> >::Zero = CovariantVector<float, 4>( NumericTraits<float >::Zero );
const CovariantVector<float, 4>  NumericTraits<CovariantVector<float, 4> >::One = CovariantVector<float, 4>( NumericTraits<float>::One );
const CovariantVector<double, 4>  NumericTraits<CovariantVector<double, 4> >::Zero = CovariantVector<double , 4>( NumericTraits<double>::Zero );
const CovariantVector<double, 4>  NumericTraits<CovariantVector<double, 4> >::One = CovariantVector<double, 4>( NumericTraits<double>::One );
const CovariantVector<long double, 4>  NumericTraits<CovariantVector<long double, 4> >::Zero = CovariantVector<long double, 4>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 4>  NumericTraits<CovariantVector<long double, 4> >::One = CovariantVector<long double, 4>( NumericTraits<long double>::One );

const CovariantVector<unsigned char, 5>  NumericTraits<CovariantVector<unsigned char, 5> >::Zero = CovariantVector<unsigned char, 5>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 5>  NumericTraits<CovariantVector<unsigned char, 5> >::One = CovariantVector<unsigned char, 5>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 5>  NumericTraits<CovariantVector<signed char, 5> >::Zero = CovariantVector<signed char, 5>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 5>  NumericTraits<CovariantVector<signed char, 5> >::One = CovariantVector<signed char, 5>( NumericTraits<signed char>::One );
const CovariantVector<char, 5>  NumericTraits<CovariantVector<char, 5> >::Zero = CovariantVector<char, 5>( NumericTraits<char>::Zero );
const CovariantVector<char, 5>  NumericTraits<CovariantVector<char, 5> >::One = CovariantVector<char, 5>( NumericTraits<char>::One );
const CovariantVector<short, 5>  NumericTraits<CovariantVector<short, 5> >::Zero = CovariantVector<short, 5>( NumericTraits<short>::Zero );
const CovariantVector<short, 5>  NumericTraits<CovariantVector<short, 5> >::One = CovariantVector<short, 5>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 5>  NumericTraits<CovariantVector<unsigned short, 5> >::Zero = CovariantVector<unsigned short, 5>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 5>  NumericTraits<CovariantVector<unsigned short, 5> >::One = CovariantVector<unsigned short, 5>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 5>  NumericTraits<CovariantVector<int, 5> >::Zero = CovariantVector<int, 5>( NumericTraits<int>::Zero );
const CovariantVector<int, 5>  NumericTraits<CovariantVector<int, 5> >::One = CovariantVector<int, 5>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 5>  NumericTraits<CovariantVector<unsigned int, 5> >::Zero = CovariantVector<unsigned int, 5>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 5>  NumericTraits<CovariantVector<unsigned int, 5> >::One = CovariantVector<unsigned int, 5>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 5>  NumericTraits<CovariantVector<long, 5> >::Zero = CovariantVector<long, 5>( NumericTraits<long>::Zero );
const CovariantVector<long, 5>  NumericTraits<CovariantVector<long, 5> >::One = CovariantVector<long, 5>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 5>  NumericTraits<CovariantVector<unsigned long, 5> >::Zero = CovariantVector<unsigned long, 5>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 5>  NumericTraits<CovariantVector<unsigned long, 5> >::One = CovariantVector<unsigned long, 5>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 5>  NumericTraits<CovariantVector<float, 5> >::Zero = CovariantVector<float, 5>( NumericTraits<float >::Zero );
const CovariantVector<float, 5>  NumericTraits<CovariantVector<float, 5> >::One = CovariantVector<float, 5>( NumericTraits<float>::One );
const CovariantVector<double, 5>  NumericTraits<CovariantVector<double, 5> >::Zero = CovariantVector<double , 5>( NumericTraits<double>::Zero );
const CovariantVector<double, 5>  NumericTraits<CovariantVector<double, 5> >::One = CovariantVector<double, 5>( NumericTraits<double>::One );
const CovariantVector<long double, 5>  NumericTraits<CovariantVector<long double, 5> >::Zero = CovariantVector<long double, 5>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 5>  NumericTraits<CovariantVector<long double, 5> >::One = CovariantVector<long double, 5>( NumericTraits<long double>::One );

const CovariantVector<unsigned char, 6>  NumericTraits<CovariantVector<unsigned char, 6> >::Zero = CovariantVector<unsigned char, 6>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 6>  NumericTraits<CovariantVector<unsigned char, 6> >::One = CovariantVector<unsigned char, 6>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 6>  NumericTraits<CovariantVector<signed char, 6> >::Zero = CovariantVector<signed char, 6>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 6>  NumericTraits<CovariantVector<signed char, 6> >::One = CovariantVector<signed char, 6>( NumericTraits<signed char>::One );
const CovariantVector<char, 6>  NumericTraits<CovariantVector<char, 6> >::Zero = CovariantVector<char, 6>( NumericTraits<char>::Zero );
const CovariantVector<char, 6>  NumericTraits<CovariantVector<char, 6> >::One = CovariantVector<char, 6>( NumericTraits<char>::One );
const CovariantVector<short, 6>  NumericTraits<CovariantVector<short, 6> >::Zero = CovariantVector<short, 6>( NumericTraits<short>::Zero );
const CovariantVector<short, 6>  NumericTraits<CovariantVector<short, 6> >::One = CovariantVector<short, 6>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 6>  NumericTraits<CovariantVector<unsigned short, 6> >::Zero = CovariantVector<unsigned short, 6>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 6>  NumericTraits<CovariantVector<unsigned short, 6> >::One = CovariantVector<unsigned short, 6>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 6>  NumericTraits<CovariantVector<int, 6> >::Zero = CovariantVector<int, 6>( NumericTraits<int>::Zero );
const CovariantVector<int, 6>  NumericTraits<CovariantVector<int, 6> >::One = CovariantVector<int, 6>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 6>  NumericTraits<CovariantVector<unsigned int, 6> >::Zero = CovariantVector<unsigned int, 6>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 6>  NumericTraits<CovariantVector<unsigned int, 6> >::One = CovariantVector<unsigned int, 6>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 6>  NumericTraits<CovariantVector<long, 6> >::Zero = CovariantVector<long, 6>( NumericTraits<long>::Zero );
const CovariantVector<long, 6>  NumericTraits<CovariantVector<long, 6> >::One = CovariantVector<long, 6>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 6>  NumericTraits<CovariantVector<unsigned long, 6> >::Zero = CovariantVector<unsigned long, 6>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 6>  NumericTraits<CovariantVector<unsigned long, 6> >::One = CovariantVector<unsigned long, 6>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 6>  NumericTraits<CovariantVector<float, 6> >::Zero = CovariantVector<float, 6>( NumericTraits<float >::Zero );
const CovariantVector<float, 6>  NumericTraits<CovariantVector<float, 6> >::One = CovariantVector<float, 6>( NumericTraits<float>::One );
const CovariantVector<double, 6>  NumericTraits<CovariantVector<double, 6> >::Zero = CovariantVector<double , 6>( NumericTraits<double>::Zero );
const CovariantVector<double, 6>  NumericTraits<CovariantVector<double, 6> >::One = CovariantVector<double, 6>( NumericTraits<double>::One );
const CovariantVector<long double, 6>  NumericTraits<CovariantVector<long double, 6> >::Zero = CovariantVector<long double, 6>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 6>  NumericTraits<CovariantVector<long double, 6> >::One = CovariantVector<long double, 6>( NumericTraits<long double>::One );

const CovariantVector<unsigned char, 7>  NumericTraits<CovariantVector<unsigned char, 7> >::Zero = CovariantVector<unsigned char, 7>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 7>  NumericTraits<CovariantVector<unsigned char, 7> >::One = CovariantVector<unsigned char, 7>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 7>  NumericTraits<CovariantVector<signed char, 7> >::Zero = CovariantVector<signed char, 7>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 7>  NumericTraits<CovariantVector<signed char, 7> >::One = CovariantVector<signed char, 7>( NumericTraits<signed char>::One );
const CovariantVector<char, 7>  NumericTraits<CovariantVector<char, 7> >::Zero = CovariantVector<char, 7>( NumericTraits<char>::Zero );
const CovariantVector<char, 7>  NumericTraits<CovariantVector<char, 7> >::One = CovariantVector<char, 7>( NumericTraits<char>::One );
const CovariantVector<short, 7>  NumericTraits<CovariantVector<short, 7> >::Zero = CovariantVector<short, 7>( NumericTraits<short>::Zero );
const CovariantVector<short, 7>  NumericTraits<CovariantVector<short, 7> >::One = CovariantVector<short, 7>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 7>  NumericTraits<CovariantVector<unsigned short, 7> >::Zero = CovariantVector<unsigned short, 7>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 7>  NumericTraits<CovariantVector<unsigned short, 7> >::One = CovariantVector<unsigned short, 7>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 7>  NumericTraits<CovariantVector<int, 7> >::Zero = CovariantVector<int, 7>( NumericTraits<int>::Zero );
const CovariantVector<int, 7>  NumericTraits<CovariantVector<int, 7> >::One = CovariantVector<int, 7>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 7>  NumericTraits<CovariantVector<unsigned int, 7> >::Zero = CovariantVector<unsigned int, 7>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 7>  NumericTraits<CovariantVector<unsigned int, 7> >::One = CovariantVector<unsigned int, 7>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 7>  NumericTraits<CovariantVector<long, 7> >::Zero = CovariantVector<long, 7>( NumericTraits<long>::Zero );
const CovariantVector<long, 7>  NumericTraits<CovariantVector<long, 7> >::One = CovariantVector<long, 7>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 7>  NumericTraits<CovariantVector<unsigned long, 7> >::Zero = CovariantVector<unsigned long, 7>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 7>  NumericTraits<CovariantVector<unsigned long, 7> >::One = CovariantVector<unsigned long, 7>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 7>  NumericTraits<CovariantVector<float, 7> >::Zero = CovariantVector<float, 7>( NumericTraits<float >::Zero );
const CovariantVector<float, 7>  NumericTraits<CovariantVector<float, 7> >::One = CovariantVector<float, 7>( NumericTraits<float>::One );
const CovariantVector<double, 7>  NumericTraits<CovariantVector<double, 7> >::Zero = CovariantVector<double , 7>( NumericTraits<double>::Zero );
const CovariantVector<double, 7>  NumericTraits<CovariantVector<double, 7> >::One = CovariantVector<double, 7>( NumericTraits<double>::One );
const CovariantVector<long double, 7>  NumericTraits<CovariantVector<long double, 7> >::Zero = CovariantVector<long double, 7>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 7>  NumericTraits<CovariantVector<long double, 7> >::One = CovariantVector<long double, 7>( NumericTraits<long double>::One );

const CovariantVector<unsigned char, 8>  NumericTraits<CovariantVector<unsigned char, 8> >::Zero = CovariantVector<unsigned char, 8>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 8>  NumericTraits<CovariantVector<unsigned char, 8> >::One = CovariantVector<unsigned char, 8>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 8>  NumericTraits<CovariantVector<signed char, 8> >::Zero = CovariantVector<signed char, 8>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 8>  NumericTraits<CovariantVector<signed char, 8> >::One = CovariantVector<signed char, 8>( NumericTraits<signed char>::One );
const CovariantVector<char, 8>  NumericTraits<CovariantVector<char, 8> >::Zero = CovariantVector<char, 8>( NumericTraits<char>::Zero );
const CovariantVector<char, 8>  NumericTraits<CovariantVector<char, 8> >::One = CovariantVector<char, 8>( NumericTraits<char>::One );
const CovariantVector<short, 8>  NumericTraits<CovariantVector<short, 8> >::Zero = CovariantVector<short, 8>( NumericTraits<short>::Zero );
const CovariantVector<short, 8>  NumericTraits<CovariantVector<short, 8> >::One = CovariantVector<short, 8>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 8>  NumericTraits<CovariantVector<unsigned short, 8> >::Zero = CovariantVector<unsigned short, 8>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 8>  NumericTraits<CovariantVector<unsigned short, 8> >::One = CovariantVector<unsigned short, 8>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 8>  NumericTraits<CovariantVector<int, 8> >::Zero = CovariantVector<int, 8>( NumericTraits<int>::Zero );
const CovariantVector<int, 8>  NumericTraits<CovariantVector<int, 8> >::One = CovariantVector<int, 8>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 8>  NumericTraits<CovariantVector<unsigned int, 8> >::Zero = CovariantVector<unsigned int, 8>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 8>  NumericTraits<CovariantVector<unsigned int, 8> >::One = CovariantVector<unsigned int, 8>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 8>  NumericTraits<CovariantVector<long, 8> >::Zero = CovariantVector<long, 8>( NumericTraits<long>::Zero );
const CovariantVector<long, 8>  NumericTraits<CovariantVector<long, 8> >::One = CovariantVector<long, 8>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 8>  NumericTraits<CovariantVector<unsigned long, 8> >::Zero = CovariantVector<unsigned long, 8>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 8>  NumericTraits<CovariantVector<unsigned long, 8> >::One = CovariantVector<unsigned long, 8>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 8>  NumericTraits<CovariantVector<float, 8> >::Zero = CovariantVector<float, 8>( NumericTraits<float >::Zero );
const CovariantVector<float, 8>  NumericTraits<CovariantVector<float, 8> >::One = CovariantVector<float, 8>( NumericTraits<float>::One );
const CovariantVector<double, 8>  NumericTraits<CovariantVector<double, 8> >::Zero = CovariantVector<double , 8>( NumericTraits<double>::Zero );
const CovariantVector<double, 8>  NumericTraits<CovariantVector<double, 8> >::One = CovariantVector<double, 8>( NumericTraits<double>::One );
const CovariantVector<long double, 8>  NumericTraits<CovariantVector<long double, 8> >::Zero = CovariantVector<long double, 8>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 8>  NumericTraits<CovariantVector<long double, 8> >::One = CovariantVector<long double, 8>( NumericTraits<long double>::One );

const CovariantVector<unsigned char, 9>  NumericTraits<CovariantVector<unsigned char, 9> >::Zero = CovariantVector<unsigned char, 9>( NumericTraits<unsigned char>::Zero );
const CovariantVector<unsigned char, 9>  NumericTraits<CovariantVector<unsigned char, 9> >::One = CovariantVector<unsigned char, 9>( NumericTraits<unsigned char>::One );
const CovariantVector<signed char, 9>  NumericTraits<CovariantVector<signed char, 9> >::Zero = CovariantVector<signed char, 9>( NumericTraits<signed char>::Zero );
const CovariantVector<signed char, 9>  NumericTraits<CovariantVector<signed char, 9> >::One = CovariantVector<signed char, 9>( NumericTraits<signed char>::One );
const CovariantVector<char, 9>  NumericTraits<CovariantVector<char, 9> >::Zero = CovariantVector<char, 9>( NumericTraits<char>::Zero );
const CovariantVector<char, 9>  NumericTraits<CovariantVector<char, 9> >::One = CovariantVector<char, 9>( NumericTraits<char>::One );
const CovariantVector<short, 9>  NumericTraits<CovariantVector<short, 9> >::Zero = CovariantVector<short, 9>( NumericTraits<short>::Zero );
const CovariantVector<short, 9>  NumericTraits<CovariantVector<short, 9> >::One = CovariantVector<short, 9>( NumericTraits<short>::One );
const CovariantVector<unsigned short, 9>  NumericTraits<CovariantVector<unsigned short, 9> >::Zero = CovariantVector<unsigned short, 9>( NumericTraits<unsigned short>::Zero );
const CovariantVector<unsigned short, 9>  NumericTraits<CovariantVector<unsigned short, 9> >::One = CovariantVector<unsigned short, 9>( NumericTraits<unsigned short>::One );
const CovariantVector<int, 9>  NumericTraits<CovariantVector<int, 9> >::Zero = CovariantVector<int, 9>( NumericTraits<int>::Zero );
const CovariantVector<int, 9>  NumericTraits<CovariantVector<int, 9> >::One = CovariantVector<int, 9>( NumericTraits<int>::One );
const CovariantVector<unsigned int, 9>  NumericTraits<CovariantVector<unsigned int, 9> >::Zero = CovariantVector<unsigned int, 9>( NumericTraits<unsigned int>::Zero );
const CovariantVector<unsigned int, 9>  NumericTraits<CovariantVector<unsigned int, 9> >::One = CovariantVector<unsigned int, 9>( NumericTraits<unsigned int>::One );
const CovariantVector<long, 9>  NumericTraits<CovariantVector<long, 9> >::Zero = CovariantVector<long, 9>( NumericTraits<long>::Zero );
const CovariantVector<long, 9>  NumericTraits<CovariantVector<long, 9> >::One = CovariantVector<long, 9>( NumericTraits<long>::One );
const CovariantVector<unsigned long, 9>  NumericTraits<CovariantVector<unsigned long, 9> >::Zero = CovariantVector<unsigned long, 9>( NumericTraits<unsigned long>::Zero );
const CovariantVector<unsigned long, 9>  NumericTraits<CovariantVector<unsigned long, 9> >::One = CovariantVector<unsigned long, 9>( NumericTraits<unsigned long>::One );
const CovariantVector<float, 9>  NumericTraits<CovariantVector<float, 9> >::Zero = CovariantVector<float, 9>( NumericTraits<float >::Zero );
const CovariantVector<float, 9>  NumericTraits<CovariantVector<float, 9> >::One = CovariantVector<float, 9>( NumericTraits<float>::One );
const CovariantVector<double, 9>  NumericTraits<CovariantVector<double, 9> >::Zero = CovariantVector<double , 9>( NumericTraits<double>::Zero );
const CovariantVector<double, 9>  NumericTraits<CovariantVector<double, 9> >::One = CovariantVector<double, 9>( NumericTraits<double>::One );
const CovariantVector<long double, 9>  NumericTraits<CovariantVector<long double, 9> >::Zero = CovariantVector<long double, 9>( NumericTraits<long double>::Zero );
const CovariantVector<long double, 9>  NumericTraits<CovariantVector<long double, 9> >::One = CovariantVector<long double, 9>( NumericTraits<long double>::One );

} // end namespace itk
