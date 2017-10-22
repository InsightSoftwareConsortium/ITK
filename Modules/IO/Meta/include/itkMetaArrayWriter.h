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
#ifndef itkMetaArrayWriter_h
#define itkMetaArrayWriter_h
#include "ITKIOMetaExport.h"

#include "itkLightProcessObject.h"
#include "itkArray.h"
#include "itkCovariantVector.h"
#include "itkVariableLengthVector.h"
#include "metaArray.h"

namespace itk
{
class ITKIOMeta_EXPORT MetaArrayWriter : public LightProcessObject
{
public:

  /** SmartPointer typedef support. */
  typedef MetaArrayWriter             Self;
  typedef LightProcessObject          Superclass;

  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaArrayWriter, LightProcessObject);

  /** Set/Get the filename. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Set/Get the filename to which the data is written.
   *  Optional: use if header and data should be in separate files. */
  itkSetStringMacro(DataFileName);
  itkGetStringMacro(DataFileName);

  /** Boolean to set binary mode.
   *  If set to On, data will be stored in the file in binary format; if set
   *  to Off, data will be stored in ascci format.
   *  Default is Off. */
  itkBooleanMacro(Binary);
  itkSetMacro(Binary, bool);
  itkGetConstMacro(Binary, bool);

  /** Set the input itk Array to write. */
  template< typename TValue >
  void SetInput(MET_ValueEnumType _metaElementType,
                const Array< TValue > *_array)
  {
    m_Buffer = (const void *)( _array->data_block() );
    m_MetaArray.InitializeEssential(_array->Size(),
                                    _metaElementType);
  }

  /** Set the input itk FixedArray to write. */
  template< typename TValue, unsigned int VLength >
  void SetInput(MET_ValueEnumType _metaElementType,
                const FixedArray< TValue, VLength > *_array)
  {
    m_Buffer = (const void *)( _array->GetDataPointer() );
    m_MetaArray.InitializeEssential(VLength,
                                    _metaElementType);
  }

  /** Set the input itk Vector to write. */
  template< typename TValue, unsigned int VLength >
  void SetInput(MET_ValueEnumType _metaElementType,
                const Vector< TValue, VLength > *_vector)
  {
    m_Buffer = (const void *)( _vector->GetDataPointer() );
    m_MetaArray.InitializeEssential(VLength,
                                    _metaElementType);
  }

  /** Set the input itk CovariantVector to write. */
  template< typename TValue, unsigned int VLength >
  void SetInput(MET_ValueEnumType _metaElementType,
                const CovariantVector< TValue, VLength > *_vector)
  {
    m_Buffer = (const void *)( _vector->GetDataPointer() );
    m_MetaArray.InitializeEssential(VLength,
                                    _metaElementType);
  }

  /** Set the input itk VariableLengthVector to write. */
  template< typename TValue >
  void SetInput(MET_ValueEnumType _metaElementType,
                const VariableLengthVector< TValue > *_vector)
  {
    m_Buffer = (const void *)( _vector->GetDataPointer() );
    m_MetaArray.InitializeEssential(_vector->Size(),
                                    _metaElementType);
  }

  /** Copies the elements from an array of arrays into the output
   *  buffer. Requires all sub-arrays to have the same length.
   *  length of the major array is the "length" of the array, while
   *  the length of the sub-arrays is the "number of channels" at each
   *  array position. Expected form itk::Array< itk::Array< * > >.
   *  May work for othesub-array-types that define the [] operator and the
   *  GetSize() function. */
  template< typename TValue >
  void SetMultiChannelInput(MET_ValueEnumType _metaElementType,
                            int ,
                            const Array< TValue > *_array)
  {
    int rows = _array->GetSize();
    int cols = ( *_array )[0].GetSize();

    m_MetaArray.InitializeEssential(rows,
                                    _metaElementType,
                                    cols,
                                    ITK_NULLPTR,
                                    true,
                                    true);
    m_Buffer = m_MetaArray.ElementData();
    for ( int i = 0; i < rows; i++ )
      {
      for ( int j = 0; j < cols; j++ )
        {
        m_MetaArray.ElementData( i * cols + j, (double)( ( *_array )[i][j] ) );
        }
      }
  }

  /** Set/Get the precision of the writing. */
  itkSetMacro(Precision, unsigned int);
  itkGetConstMacro(Precision, unsigned int);

  /** Set the data type written to the file. */
  void ConvertTo(MET_ValueEnumType _metaElementType);

  /** Write out the array. */
  void Update();

protected:

  MetaArrayWriter();
  virtual ~MetaArrayWriter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  bool          m_Binary;

  unsigned int  m_Precision;

  std::string   m_FileName;
  std::string   m_DataFileName;

  MetaArray     m_MetaArray;

  const void   *m_Buffer;
};
} // namespace itk

#endif // itkMetaArrayWriter_h
