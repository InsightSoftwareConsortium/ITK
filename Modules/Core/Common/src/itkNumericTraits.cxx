/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkNumericTraits.h"

namespace itk
{
const bool NumericTraits< bool >:: Zero = false;
const bool NumericTraits< bool >:: One = true;

const unsigned char NumericTraits< unsigned char >:: Zero = 0;
const unsigned char NumericTraits< unsigned char >:: One = 1;

const signed char NumericTraits< signed char >:: Zero = 0;
const signed char NumericTraits< signed char >:: One = 1;

const char NumericTraits< char >:: Zero = 0;
const char NumericTraits< char >:: One = 1;

const unsigned short NumericTraits< unsigned short >:: Zero = 0;
const unsigned short NumericTraits< unsigned short >:: One = 1;

const short NumericTraits< short >:: Zero = 0;
const short NumericTraits< short >:: One = 1;

const unsigned int NumericTraits< unsigned int >:: Zero = 0;
const unsigned int NumericTraits< unsigned int >:: One = 1;

const int NumericTraits< int >:: Zero = 0;
const int NumericTraits< int >:: One = 1;

const unsigned long NumericTraits< unsigned long >:: Zero = 0;
const unsigned long NumericTraits< unsigned long >:: One = 1;

const long NumericTraits< long >:: Zero = 0UL;
const long NumericTraits< long >:: One = 1UL;

const float NumericTraits< float >:: Zero = 0.0F;
const float NumericTraits< float >:: One = 1.0F;

const double NumericTraits< double >:: Zero = 0.0;
const double NumericTraits< double >:: One = 1.0;

const long double NumericTraits< long double >:: Zero = 0.0;
const long double NumericTraits< long double >:: One = 1.0;

const std::complex< float >  NumericTraits< std::complex< float > >:: Zero = std::complex< float >(0.0f, 0.0f);
const std::complex< float >  NumericTraits< std::complex< float > >:: One  = std::complex< float >(1.0f, 0.0f);

const std::complex< double >  NumericTraits< std::complex< double > >:: Zero = std::complex< double >(0.0, 0.0);
const std::complex< double >  NumericTraits< std::complex< double > >:: One  = std::complex< double >(1.0, 0.0);

const long long NumericTraits< long long >:: Zero = 0LL;
const long long NumericTraits< long long >:: One = 1LL;

const unsigned long long NumericTraits< unsigned long long >:: Zero = 0ULL;
const unsigned long long NumericTraits< unsigned long long >:: One = 1ULL;
} // end namespace itk
