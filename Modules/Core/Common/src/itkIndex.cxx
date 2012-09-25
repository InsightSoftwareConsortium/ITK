// This class is simply to for explicit
// instantiation of the most common
// index types into a class that can
// be compiled independant of the main
// body of code
#include "itkIndex.h"

template class itk::Index<2u>;
template class itk::Index<3u>;
template class itk::Index<4u>;

template class itk::Functor::IndexLexicographicCompare<2u>;
template class itk::Functor::IndexLexicographicCompare<3u>;
template class itk::Functor::IndexLexicographicCompare<4u>;
