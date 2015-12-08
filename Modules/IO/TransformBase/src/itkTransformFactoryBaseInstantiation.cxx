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

#include "itkCompositeTransformIOHelper.h"
#include "itkCompositeTransformIOHelper.hxx"

#include "itkTransformFileReader.h"
#include "itkTransformFileReader.hxx"

#include "itkTransformFileWriter.h"
#include "itkTransformFileWriter.hxx"

#include "itkTransformIOBase.h"
#include "itkTransformIOBase.hxx"

#include "itkTransformIOFactory.h"
#include "itkTransformIOFactory.hxx"

namespace itk
{

template class CompositeTransformIOHelperTemplate< double >;
template class CompositeTransformIOHelperTemplate< float >;

template class TransformFileReaderTemplate< double >;
template class TransformFileReaderTemplate< float >;

template class TransformFileWriterTemplate< double >;
template class TransformFileWriterTemplate< float >;

template class TransformIOBaseTemplate< double >;
template class TransformIOBaseTemplate< float >;

template class TransformIOFactoryTemplate< double >;
template class TransformIOFactoryTemplate< float >;

}  // end namespace itk
