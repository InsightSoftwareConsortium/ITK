/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVariableLengthVectorPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsVariableLengthVectorPixel.h"

namespace itk
{

const VariableLengthVector<unsigned char>  
NumericTraits<VariableLengthVector<unsigned char> >::Zero( const VariableLengthVector<unsigned char>  &a )
{ 
  VariableLengthVector< unsigned char > b(a.Size()); 
  b.Fill( NumericTraits< unsigned char >::Zero );
  return b;
}
const VariableLengthVector<unsigned char>  
NumericTraits<VariableLengthVector<unsigned char> >::One( const VariableLengthVector<unsigned char> &a )
{ 
  VariableLengthVector< unsigned char > b(a.Size()); 
  b.Fill( NumericTraits< unsigned char >::One );
  return b;
}
const VariableLengthVector<signed char>  
NumericTraits<VariableLengthVector<signed char> >::Zero( const VariableLengthVector<signed char> &a )
{ 
  VariableLengthVector< signed char > b(a.Size()); 
  b.Fill( NumericTraits< signed char >::Zero );
  return b;
}
const VariableLengthVector<signed char>  
NumericTraits<VariableLengthVector<signed char> >::One( const VariableLengthVector<signed char> &a )
{ 
  VariableLengthVector< signed char > b(a.Size()); 
  b.Fill( NumericTraits< signed char >::One );
  return b;
}
const VariableLengthVector<char>  
NumericTraits<VariableLengthVector<char> >::Zero( const VariableLengthVector<char> &a )
{ 
  VariableLengthVector< char > b(a.Size()); 
  b.Fill( NumericTraits< char >::Zero );
  return b;
}
const VariableLengthVector<char>  
NumericTraits<VariableLengthVector<char> >::One( const VariableLengthVector<char> &a )
{ 
  VariableLengthVector< char > b(a.Size()); 
  b.Fill( NumericTraits< char >::One );
  return b;
}
const VariableLengthVector<short>  
NumericTraits<VariableLengthVector<short> >::Zero( const VariableLengthVector<short> &a )
{ 
  VariableLengthVector< short > b(a.Size()); 
  b.Fill( NumericTraits< short >::Zero );
  return b;
}
const VariableLengthVector<short>  
NumericTraits<VariableLengthVector<short> >::One( const VariableLengthVector<short> &a )
{ 
  VariableLengthVector< short > b(a.Size()); 
  b.Fill( NumericTraits<short >::One );
  return b;
}
const VariableLengthVector<unsigned short>  
NumericTraits<VariableLengthVector<unsigned short> >::Zero( const VariableLengthVector<unsigned short> &a )
{ 
  VariableLengthVector< unsigned short > b(a.Size()); 
  b.Fill( NumericTraits< unsigned short >::Zero );
  return b;
}
const VariableLengthVector<unsigned short>  
NumericTraits<VariableLengthVector<unsigned short> >::One( const VariableLengthVector<unsigned short> &a )
{ 
  VariableLengthVector< unsigned short > b(a.Size()); 
  b.Fill( NumericTraits< unsigned short >::One );
  return b;
}
const VariableLengthVector<int>  
NumericTraits<VariableLengthVector<int> >::Zero( const VariableLengthVector<int> &a )
{ 
  VariableLengthVector< int > b(a.Size()); 
  b.Fill( NumericTraits< int >::Zero );
  return b;
}
const VariableLengthVector<int>  
NumericTraits<VariableLengthVector<int> >::One( const VariableLengthVector<int> &a )
{ 
  VariableLengthVector< int > b(a.Size()); 
  b.Fill( NumericTraits< int >::One );
  return b;
}
const VariableLengthVector<unsigned int>  
NumericTraits<VariableLengthVector<unsigned int> >::Zero( const VariableLengthVector<unsigned int> &a )
{ 
  VariableLengthVector< unsigned int > b(a.Size()); 
  b.Fill( NumericTraits< unsigned int >::Zero );
  return b;
}
const VariableLengthVector<unsigned int>  
NumericTraits<VariableLengthVector<unsigned int> >::One( const VariableLengthVector<unsigned int> &a )
{ 
  VariableLengthVector< unsigned int > b(a.Size()); 
  b.Fill( NumericTraits< unsigned int >::One );
  return b;
}
const VariableLengthVector<long>  
NumericTraits<VariableLengthVector<long> >::Zero( const VariableLengthVector<long> &a )
{ 
  VariableLengthVector< long > b(a.Size()); 
  b.Fill( NumericTraits< long >::Zero );
  return b;
}
const VariableLengthVector<long>  
NumericTraits<VariableLengthVector<long> >::One( const VariableLengthVector<long> &a )
{ 
  VariableLengthVector< long > b(a.Size()); 
  b.Fill( NumericTraits< long >::One );
  return b;
}
const VariableLengthVector<unsigned long>  
NumericTraits<VariableLengthVector<unsigned long> >::Zero( const VariableLengthVector<unsigned long> &a )
{ 
  VariableLengthVector< unsigned long > b(a.Size()); 
  b.Fill( NumericTraits< unsigned long >::Zero );
  return b;
}
const VariableLengthVector<unsigned long>  
NumericTraits<VariableLengthVector<unsigned long> >::One( const VariableLengthVector<unsigned long> &a )
{ 
  VariableLengthVector< unsigned long > b(a.Size()); 
  b.Fill( NumericTraits< unsigned long >::One );
  return b;
}
const VariableLengthVector<double>  
NumericTraits<VariableLengthVector<double> >::Zero( const VariableLengthVector<double> &a )
{ 
  VariableLengthVector< double > b(a.Size()); 
  b.Fill( NumericTraits< double >::Zero );
  return b;
}
const VariableLengthVector<double>  
NumericTraits<VariableLengthVector<double> >::One( const VariableLengthVector<double> &a )
{ 
  VariableLengthVector< double > b(a.Size()); 
  b.Fill( NumericTraits< double >::One );
  return b;
}
const VariableLengthVector<float>  
NumericTraits<VariableLengthVector<float> >::Zero( const VariableLengthVector<float> &a )
{ 
  VariableLengthVector< float > b(a.Size()); 
  b.Fill( NumericTraits< float >::Zero );
  return b;
}
const VariableLengthVector<float>  
NumericTraits<VariableLengthVector<float> >::One( const VariableLengthVector<float> &a )
{ 
  VariableLengthVector< float > b(a.Size()); 
  b.Fill( NumericTraits< float >::One );
  return b;
}
const VariableLengthVector<long double>  
NumericTraits<VariableLengthVector<long double> >::Zero( const VariableLengthVector<long double> &a )
{ 
  VariableLengthVector< long double > b(a.Size()); 
  b.Fill( NumericTraits< long double >::Zero );
  return b;
}
const VariableLengthVector<long double>  
NumericTraits<VariableLengthVector<long double> >::One( const VariableLengthVector<long double> &a )
{ 
  VariableLengthVector< long double > b(a.Size()); 
  b.Fill( NumericTraits< long double >::One );
  return b;
}
   

} // end namespace itk
