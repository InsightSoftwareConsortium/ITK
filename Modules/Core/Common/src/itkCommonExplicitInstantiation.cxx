// This class is simply to for explicit
// instantiation of the most common
// index types into a class that can
// be compiled independant of the main
// body of code
#include "itkFixedArray.h"
template class itk::FixedArray<uint8_t,1u>;
template class itk::FixedArray<uint8_t,2u>;
template class itk::FixedArray<uint8_t,3u>;
template class itk::FixedArray<uint8_t,4u>;
template class itk::FixedArray<uint8_t,5u>;
template class itk::FixedArray<uint8_t,6u>;
template class itk::FixedArray< int8_t,1u>;
template class itk::FixedArray< int8_t,2u>;
template class itk::FixedArray< int8_t,3u>;
template class itk::FixedArray< int8_t,4u>;
template class itk::FixedArray< int8_t,5u>;
template class itk::FixedArray< int8_t,6u>;

template class itk::FixedArray<uint16_t,1u>;
template class itk::FixedArray<uint16_t,2u>;
template class itk::FixedArray<uint16_t,3u>;
template class itk::FixedArray<uint16_t,4u>;
template class itk::FixedArray<uint16_t,5u>;
template class itk::FixedArray<uint16_t,6u>;
template class itk::FixedArray< int16_t,1u>;
template class itk::FixedArray< int16_t,2u>;
template class itk::FixedArray< int16_t,3u>;
template class itk::FixedArray< int16_t,4u>;
template class itk::FixedArray< int16_t,5u>;
template class itk::FixedArray< int16_t,6u>;

template class itk::FixedArray<uint32_t,1u>;
template class itk::FixedArray<uint32_t,2u>;
template class itk::FixedArray<uint32_t,3u>;
template class itk::FixedArray<uint32_t,4u>;
template class itk::FixedArray<uint32_t,5u>;
template class itk::FixedArray<uint32_t,6u>;
template class itk::FixedArray< int32_t,1u>;
template class itk::FixedArray< int32_t,2u>;
template class itk::FixedArray< int32_t,3u>;
template class itk::FixedArray< int32_t,4u>;
template class itk::FixedArray< int32_t,5u>;
template class itk::FixedArray< int32_t,6u>;

template class itk::FixedArray<uint64_t,1u>;
template class itk::FixedArray<uint64_t,2u>;
template class itk::FixedArray<uint64_t,3u>;
template class itk::FixedArray<uint64_t,4u>;
template class itk::FixedArray<uint64_t,5u>;
template class itk::FixedArray<uint64_t,6u>;
template class itk::FixedArray< int64_t,1u>;
template class itk::FixedArray< int64_t,2u>;
template class itk::FixedArray< int64_t,3u>;
template class itk::FixedArray< int64_t,4u>;
template class itk::FixedArray< int64_t,5u>;
template class itk::FixedArray< int64_t,6u>;

template class itk::FixedArray<float,1u>;
template class itk::FixedArray<float,2u>;
template class itk::FixedArray<float,3u>;
template class itk::FixedArray<float,4u>;
template class itk::FixedArray<float,5u>;
template class itk::FixedArray<float,6u>;

template class itk::FixedArray<double,1u>;
template class itk::FixedArray<double,2u>;
template class itk::FixedArray<double,3u>;
template class itk::FixedArray<double,4u>;
template class itk::FixedArray<double,5u>;
template class itk::FixedArray<double,6u>;

#include "itkIndex.h"

template class itk::Index<2u>;
template class itk::Index<3u>;
template class itk::Index<4u>;

template class itk::Functor::IndexLexicographicCompare<2u>;
template class itk::Functor::IndexLexicographicCompare<3u>;
template class itk::Functor::IndexLexicographicCompare<4u>;

#include "itkImageBase.h"
template class itk::ImageBase<2u>;
template class itk::ImageBase<3u>;
template class itk::ImageBase<4u>;

#include "itkImage.h"
template class itk::Image<int8_t, 2u>;
template class itk::Image<int8_t, 3u>;
template class itk::Image<int8_t, 4u>;

template class itk::Image<uint8_t, 2u>;
template class itk::Image<uint8_t, 3u>;
template class itk::Image<uint8_t, 4u>;

template class itk::Image<int16_t, 2u>;
template class itk::Image<int16_t, 3u>;
template class itk::Image<int16_t, 4u>;

template class itk::Image<uint16_t, 2u>;
template class itk::Image<uint16_t, 3u>;
template class itk::Image<uint16_t, 4u>;

template class itk::Image<uint32_t, 2u>;
template class itk::Image<uint32_t, 3u>;
template class itk::Image<uint32_t, 4u>;

template class itk::Image<int32_t, 2u>;
template class itk::Image<int32_t, 3u>;
template class itk::Image<int32_t, 4u>;

template class itk::Image<uint64_t, 2u>;
template class itk::Image<uint64_t, 3u>;
template class itk::Image<uint64_t, 4u>;

template class itk::Image<int64_t, 2u>;
template class itk::Image<int64_t, 3u>;
template class itk::Image<int64_t, 4u>;

template class itk::Image<float, 2u>;
template class itk::Image<float, 3u>;
template class itk::Image<float, 4u>;

template class itk::Image<double, 2u>;
template class itk::Image<double, 3u>;
template class itk::Image<double, 4u>;
