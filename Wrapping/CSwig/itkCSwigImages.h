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
// Define useful short names to aid wrapper configuration.  Only
// define names for types that have been included by the wrapper
// configuration file that includes this file.
#if defined(__itkImage_h)
namespace image
{
  //typedef itk::Image<bool, 2> B2;
  //typedef itk::Image<bool, 3> B3;

  typedef itk::Image<float         , 2> F2;
  typedef itk::Image<double        , 2> D2;
  typedef itk::Image<unsigned char , 2> UC2;
  typedef itk::Image<unsigned short, 2> US2;
  typedef itk::Image<unsigned int  , 2> UI2;
  typedef itk::Image<unsigned long , 2> UL2;
  typedef itk::Image<signed char   , 2> SC2;
  typedef itk::Image<signed short  , 2> SS2;
  typedef itk::Image<signed int    , 2> SI2;
  typedef itk::Image<signed long   , 2> SL2;


  typedef itk::Image<float         , 3> F3;
  typedef itk::Image<double        , 3> D3;
  typedef itk::Image<unsigned char , 3> UC3;
  typedef itk::Image<unsigned short, 3> US3;
  typedef itk::Image<unsigned int  , 3> UI3;
  typedef itk::Image<unsigned long , 3> UL3;
  typedef itk::Image<signed char   , 3> SC3;
  typedef itk::Image<signed short  , 3> SS3;
  typedef itk::Image<signed int    , 3> SI3;
  typedef itk::Image<signed long   , 3> SL3;
}

# if defined(__itkVector_h)
namespace itkvector
{
  typedef itk::Vector< float,  2>  F2;
  typedef itk::Vector< float,  3>  F3;
  typedef itk::Vector< double, 2>  D2;
  typedef itk::Vector< double, 3>  D3;
}
namespace image
{
  typedef itk::Image< itkvector::F2, 2 > VF2;
  typedef itk::Image< itkvector::F3, 3 > VF3;
  typedef itk::Image< itkvector::F2, 2 > VD2;
  typedef itk::Image< itkvector::F3, 3 > VD3;
  typedef itk::Image< itkvector::F2, 2 > V2F2;
  typedef itk::Image< itkvector::F2, 3 > V2F3;
}
# endif

# if defined(__itkCovariantVector_h)
namespace covariantvector
{
  typedef itk::CovariantVector< float,  2>  F2;
  typedef itk::CovariantVector< float,  3>  F3;
  typedef itk::CovariantVector< double, 2>  D2;
  typedef itk::CovariantVector< double, 3>  D3;
}

namespace image
{
  typedef itk::Image< covariantvector::F2, 2 > CVF2;
  typedef itk::Image< covariantvector::F3, 3 > CVF3;
  typedef itk::Image< covariantvector::D2, 2 > CVD2;
  typedef itk::Image< covariantvector::D3, 3 > CVD3;
}
# endif
#endif
