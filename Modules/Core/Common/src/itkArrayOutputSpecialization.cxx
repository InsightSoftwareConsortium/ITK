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
#include "itkArray.h"
#include "itkArray2D.h"
#include "itkNumberToString.h"
namespace itk
{

template<>
std::ostream & operator<< <double> (std::ostream & os, const Array< double > & arr)
{
  NumberToString<double> convert;
  os << "[";
  const unsigned int length = arr.size();
  if ( length >= 1 )
    {
    const unsigned int   last   = length - 1;
    for ( unsigned int i = 0; i < last; ++i )
      {
      os << convert(arr[i]) << ", ";
      }
    os << convert(arr[last]);
    }
  os << "]";
  return os;
}

template<>
std::ostream & operator<< <float> (std::ostream & os, const Array< float > & arr)
{
  NumberToString<float> convert;
  os << "[";
  const unsigned int length = arr.size();
  if ( length >= 1 )
    {
    const unsigned int   last   = length - 1;
    for ( unsigned int i = 0; i < last; ++i )
      {
      os << convert(static_cast<float>(arr[i])) << ", ";
      }
    os << convert(static_cast<float>(arr[last]));
    }
  os << "]";
  return os;
}

template<>
std::ostream & operator<< <double> (std::ostream & os, const Array2D< double > & arr)
{
  NumberToString<double> convert;
  const unsigned int numberOfRows    = arr.rows();
  const unsigned int numberOfColumns = arr.cols();

  for ( unsigned int r = 0; r < numberOfRows; ++r )
    {
    os << "[";
    if ( numberOfColumns >= 1 )
      {
      const unsigned int lastColumn = numberOfColumns - 1;
      for ( unsigned int c = 0; c < lastColumn; ++c )
        {
        os << convert(arr(r, c)) << ", ";
        }
      os << convert(arr(r, lastColumn));
      }
    os << "]" << std::endl;
    }

  return os;
}

template<>
std::ostream & operator<< <float> (std::ostream & os, const Array2D< float > & arr)
{
  NumberToString<float> convert;
  const unsigned int numberOfRows    = arr.rows();
  const unsigned int numberOfColumns = arr.cols();

  for ( unsigned int r = 0; r < numberOfRows; ++r )
    {
    os << "[";
    if ( numberOfColumns >= 1 )
      {
      const unsigned int lastColumn = numberOfColumns - 1;
      for ( unsigned int c = 0; c < lastColumn; ++c )
        {
        os << convert(arr(r, c)) << ", ";
        }
      os << convert(arr(r, lastColumn));
      }
    os << "]" << std::endl;
    }

  return os;
}


}
