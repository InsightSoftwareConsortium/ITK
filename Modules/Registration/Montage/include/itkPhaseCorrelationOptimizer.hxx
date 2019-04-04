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
#ifndef itkPhaseCorrelationOptimizer_hxx
#define itkPhaseCorrelationOptimizer_hxx

#include "itkPhaseCorrelationOptimizer.h"

#ifndef NDEBUG
#include "itkImageFileWriter.h"

namespace
{
template< typename TImage >
void WriteDebug(const TImage* out, const char *filename)
{
  using WriterType = itk::ImageFileWriter<TImage>;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput(out);
  w->SetFileName(filename);
  try
    {
    w->Update();
    }
  catch (itk::ExceptionObject & error)
    {
    std::cerr << error << std::endl;
    }
}
}
#else
namespace
{
template< typename TImage >
void WriteDebug(TImage*, const char *) {}
}
#endif

namespace itk
{
template< typename TImage >
PhaseCorrelationOptimizer< TImage >
::PhaseCorrelationOptimizer()
{
  this->SetNumberOfRequiredInputs( 3 );
  this->SetOffsetCount( 4 );
}

template< typename TImage >
void
PhaseCorrelationOptimizer< TImage >
::SetOffsetCount( unsigned count )
{
  if ( m_Offsets.size() != count )
    {
    this->SetNumberOfRequiredOutputs( count );
    for ( unsigned i = m_Offsets.size(); i < count; i++ )
      {
      OffsetOutputPointer offsetDecorator =
          static_cast< OffsetOutputType * >( this->MakeOutput(i).GetPointer() );
      this->ProcessObject::SetNthOutput( i, offsetDecorator.GetPointer() );
      }
    m_Offsets.resize( count );

    this->Modified();
    }
}

template< typename TImage >
void
PhaseCorrelationOptimizer< TImage >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Offsets:";
  for ( unsigned i = 0; i < m_Offsets.size(); i++ )
    {
    os << " " << m_Offsets[i];
    }
  os << std::endl;
}

template< typename TImage >
void
PhaseCorrelationOptimizer< TImage >
::GenerateData()
{
  if ( !m_Updating )
    {
    this->Update();
    }
  else
    {
    OffsetType empty;
    empty.Fill( 0 );
    try
      {
      this->ComputeOffset();
      }
    catch ( ExceptionObject& err )
      {
      itkDebugMacro( "exception called while computing offset - passing" );

      this->SetOffsetCount( 1 );
      m_Offsets[0] = empty;

      // pass exception to caller
      throw err;
      }
    }

  for ( unsigned i = 0; i < m_Offsets.size(); i++ )
    {
    // write the result to the output
    OffsetOutputType* output = static_cast< OffsetOutputType* >( this->ProcessObject::GetOutput( 0 ) );
    output->Set( m_Offsets[i] );
    }
}

template< typename TImage >
void
PhaseCorrelationOptimizer< TImage >
::SetInput( const ImageType* image )
{
  itkDebugMacro( "setting input image to " << image );
  if ( this->GetInput( 0 ) != image )
    {
    this->ProcessObject::SetNthInput( 0, const_cast< ImageType* >( image ) );
    this->Modified();
    }
}

template< typename TImage >
void
PhaseCorrelationOptimizer< TImage >
::SetFixedImage( const ImageBase< ImageType::ImageDimension >* image )
{
  itkDebugMacro( "setting fixed image to " << image );
  if ( this->GetInput( 1 ) != image )
    {
    this->ProcessObject::SetNthInput( 1, const_cast< ImageBase< ImageType::ImageDimension >* >( image ) );
    this->Modified();
    }
}

template< typename TImage >
void
PhaseCorrelationOptimizer< TImage >
::SetMovingImage( const ImageBase< ImageType::ImageDimension >* image )
{
  itkDebugMacro( "setting moving image to " << image );
  if ( this->GetInput( 2 ) != image )
    {
    this->ProcessObject::SetNthInput( 2, const_cast< ImageBase< ImageType::ImageDimension >* >( image ) );
    this->Modified();
    }
}

} // end namespace itk

#endif
