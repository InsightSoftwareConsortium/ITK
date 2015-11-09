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

#ifndef itkLogicTestSupport_h
#define itkLogicTestSupport_h

#include "itkImageRegionIteratorWithIndex.h"
#include <functional>


// templates for explicit checking of image results in logic
// operations
// possibly extend to other binary functors.
// The result is recomputed and compared to the image

template<typename InIm1, typename InIm2, typename ResIm, typename OpT>
int checkImOnImRes(typename InIm1::Pointer A, typename InIm2::Pointer B,
                   typename ResIm::Pointer Res, typename ResIm::PixelType FG,
                   typename ResIm::PixelType BG)
{
  typedef typename itk::ImageRegionIteratorWithIndex<InIm1>  myIteratorType1;

  OpT Op;

  myIteratorType1 it1( A, A->GetBufferedRegion() );
  myIteratorType1 it2( B, B->GetBufferedRegion() );
  myIteratorType1 it3( Res, Res->GetBufferedRegion() );

  it1.GoToBegin();
  it2.GoToBegin();
  it3.GoToBegin();
  while( !it3.IsAtEnd() )
    {
    typename ResIm::PixelType Expected = BG;
    if (Op(it1.Get(), it2.Get()))
      {
      Expected = FG;
      }
    if (itk::Math::NotExactlyEquals(it3.Get(), Expected))
      {
      std::cerr << "Result : Im1 Op Im2" << std::endl;
      std::cerr << "Expected " << Expected << ", got " << it3.Get() << std::endl;
      return EXIT_FAILURE;
      }
    ++it3;
    ++it1;
    ++it2;
    }
  return EXIT_SUCCESS;
}

template<typename InIm1, typename ConstT, typename ResIm, typename OpT>
int checkImOnConstRes(typename InIm1::Pointer A, ConstT B,
                   typename ResIm::Pointer Res, typename ResIm::PixelType FG,
                   typename ResIm::PixelType BG)
{
  typedef typename itk::ImageRegionIteratorWithIndex<InIm1>  myIteratorType1;

  OpT Op;

  myIteratorType1 it1( A, A->GetBufferedRegion() );
  myIteratorType1 it3( Res, Res->GetBufferedRegion() );

  it1.GoToBegin();
  it3.GoToBegin();
  while( !it3.IsAtEnd() )
    {
    typename ResIm::PixelType Expected = BG;
    if (Op(it1.Get(), B))
      {
      Expected = FG;
      }
    if (itk::Math::NotExactlyEquals(it3.Get(), Expected))
      {
      std::cerr << "Result : Im1 Op Const" << std::endl;
      std::cerr << "Expected " << Expected << ", got " << it3.Get() << std::endl;
      return EXIT_FAILURE;
      }
    ++it3;
    ++it1;
    }
  return EXIT_SUCCESS;
}


template<typename ConstT, typename InIm1, typename ResIm, typename OpT>
int checkConstOnImRes(ConstT A, typename InIm1::Pointer B,
                      typename ResIm::Pointer Res, typename ResIm::PixelType FG,
                      typename ResIm::PixelType BG)
{
  typedef typename itk::ImageRegionIteratorWithIndex<InIm1>  myIteratorType1;

  OpT Op;

  myIteratorType1 it1( B, B->GetBufferedRegion() );
  myIteratorType1 it3( Res, Res->GetBufferedRegion() );

  it1.GoToBegin();
  it3.GoToBegin();
  while( !it3.IsAtEnd() )
    {
    typename ResIm::PixelType Expected = BG;
    if (Op(A, it1.Get()))
      {
      Expected = FG;
      }
    if (itk::Math::NotExactlyEquals(it3.Get(), Expected))
      {
      std::cerr << "Result : Const Op Im1" << std::endl;
      std::cerr << "Expected " << Expected << ", got " << it3.Get() << std::endl;
      return EXIT_FAILURE;
      }
    ++it3;
    ++it1;
    }
  return EXIT_SUCCESS;
}

#endif
