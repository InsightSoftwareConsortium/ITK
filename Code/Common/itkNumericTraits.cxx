/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraits.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraits.h"

namespace itk
{

const bool NumericTraits<bool>::Zero = false;
const bool NumericTraits<bool>::One = true;

const unsigned char NumericTraits<unsigned char>::Zero = 0;
const unsigned char NumericTraits<unsigned char>::One = 1;

const signed char NumericTraits<signed char>::Zero = 0;
const signed char NumericTraits<signed char>::One = 1;

const char NumericTraits<char>::Zero = 0;
const char NumericTraits<char>::One = 1;

const unsigned short NumericTraits<unsigned short>::Zero = 0;
const unsigned short NumericTraits<unsigned short>::One = 1;

const short NumericTraits<short>::Zero = 0;
const short NumericTraits<short>::One = 1;

const unsigned int NumericTraits<unsigned int>::Zero = 0;
const unsigned int NumericTraits<unsigned int>::One = 1;

const int NumericTraits<int>::Zero = 0;
const int NumericTraits<int>::One = 1;

const unsigned long NumericTraits<unsigned long>::Zero = 0;
const unsigned long NumericTraits<unsigned long>::One = 1;

const long NumericTraits<long>::Zero = 0UL;
const long NumericTraits<long>::One = 1UL;

const float NumericTraits<float>::Zero = 0.0F;
const float NumericTraits<float>::One = 1.0F;

const double NumericTraits<double>::Zero = 0.0;
const double NumericTraits<double>::One = 1.0;

const long double NumericTraits<long double>::Zero = 0.0;
const long double NumericTraits<long double>::One = 1.0;
  
const std::complex<float>  NumericTraits< std::complex<float> >::Zero = std::complex<float>(0.0,0.0);
const std::complex<float>  NumericTraits< std::complex<float> >::One  = std::complex<float>(1.0,0.0);

const std::complex<double>  NumericTraits< std::complex<double> >::Zero = std::complex<double>(0.0,0.0);
const std::complex<double>  NumericTraits< std::complex<double> >::One  = std::complex<double>(1.0,0.0);

} // end namespace itk
