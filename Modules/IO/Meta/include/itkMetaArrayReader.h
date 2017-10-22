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
#ifndef itkMetaArrayReader_h
#define itkMetaArrayReader_h
#include "ITKIOMetaExport.h"

#include "itkLightProcessObject.h"
#include "itkArray.h"
#include "itkCovariantVector.h"
#include "itkVariableLengthVector.h"
#include "metaArray.h"

namespace itk
{
class ITKIOMeta_EXPORT MetaArrayReader : public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef MetaArrayReader            Self;
  typedef LightProcessObject         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaArrayReader, LightProcessObject);

  /** Set the filename. */
  itkSetStringMacro(FileName);

  /** Get the filename. */
  itkGetStringMacro(FileName);

  /** Get a pointer to the MetaArray so that its options, such as adding
   *  comments and user-defined fields, can be changed. */
  MetaArray * GetMetaArrayPointer();

  /** Specify the buffer (already allocated) into which data should be read. */
  void SetBuffer(void *_buffer);

  /** Read the Array in MetaIO format. */
  void Update();

  /** Return the MetaIO type that corresponds to the element type. */
  MET_ValueEnumType GetDataType(void) const
  {
    return m_MetaArray.ElementType();
  }

  /** Size is used by itkArray and VariableLengthVector. */
  int Size(void) const
  {
    return m_MetaArray.Length();
  }

  /** GetNumberOfElements is used by itkArray and VariableLengthVector. */
  int GetNumberOfElements(void) const
  {
    return m_MetaArray.Length();
  }

  /** GetVectorDimension is used by itkVector. */
  int GetVectorDimension(void) const
  {
    return m_MetaArray.Length();
  }

  /** GetNumberOfComponents is used by itkVector and itkCovariantVector. */
  int GetNumberOfComponents(void) const
  {
    return m_MetaArray.Length();
  }

  /** GetCovariantVectorDimension is used by itkVector. */
  int GetCovariantVectorDimension(void) const
  {
    return m_MetaArray.Length();
  }

  /** GetElement is used by itkArray, itkFixedArray, and VariableLengthVector. */
  template< typename TValue >
  inline void GetElement(TValue & value, unsigned int i,
    unsigned int channel = 0) const
  {
    value = static_cast< TValue >( m_MetaArray.ElementData(
      i * m_MetaArray.ElementNumberOfChannels() + channel) );
  }

  /** Get an itkArray.
   *  Specify the MetaType of the elements of the Array and provide a
   *  pointer to the Array. The buffer of the Array is replaced by the
   *  buffer into which the data was directly read (a copy does not occur).
   *  If _letVectorManageData is true, the buffer persists even after this
   *  MetaArrayReader is destroyed. Otherwise, the buffer (and therefore
   *  the validity of the dat ain the Array) is destroyed when the
   *  MetaArrayReader is dstroyed. */
  template< typename TValue >
  void GetOutput(MET_ValueEnumType _metaElementType,
                 Array< TValue > *_array,
                 bool _letArrayManageData = true)
  {
    if ( m_MetaArray.ElementType() != _metaElementType )
      {
      m_MetaArray.ConvertElementDataTo(_metaElementType);
      }
    _array->SetData( (TValue *)( m_MetaArray.ElementData() ),
                     m_MetaArray.Length(),
                     _letArrayManageData );
    if ( _letArrayManageData )
      {
      m_MetaArray.AutoFreeElementData(false);
      }
  }

  /** Get an itkFixedArray. */
  template< typename TValue, unsigned int VLength >
  bool GetOutput(MET_ValueEnumType itkNotUsed(_metaElementType),
                 FixedArray< TValue, VLength > *_array)
  {
    if ( static_cast< int >( VLength ) <= m_MetaArray.Length() )
      {
      unsigned int i;
      for ( i = 0; i < VLength; i++ )
        {
        this->GetElement( ( *_array )[i], i );
        }
      return true;
      }
    return false;
  }

  /** Get an itkVectort.
   *  Specify the MetaType of the elements of the itkVector and provide
   *  a pointer to the itkVector into which the data should be copied. */
  template< typename TValue, unsigned int VLength >
  bool GetOutput(MET_ValueEnumType itkNotUsed(_metaElementType),
                 Vector< TValue, VLength > *_vector)
  {
    if ( static_cast< int >( VLength ) <= m_MetaArray.Length() )
      {
      unsigned int i;
      for ( i = 0; i < VLength; i++ )
        {
        this->GetElement( ( *_vector )[i], i );
        }
      return true;
      }
    return false;
  }

  /** Get an itkCovariantVector.
   *  Specify the MetaType of the elements of the itkCovariantVector and
   *  provide a pointer to the itkCovariantVector into which the data
   *  should be copied. */
  template< typename TValue, unsigned int VLength >
  bool GetOutput(MET_ValueEnumType itkNotUsed(_metaElementType),
                 CovariantVector< TValue, VLength > *_vector)
  {
    if ( static_cast< int >( VLength ) <= m_MetaArray.Length() )
      {
      unsigned int i;
      for ( i = 0; i < VLength; i++ )
        {
        this->GetElement( ( *_vector )[i], i );
        }
      return true;
      }
    return false;
  }

  /** Get it VariableLengthVector.
   *  Specify the MetaType of the elements of the VariableLengthVector and
   *  provide a pointer to the VariableLengthVector. The buffer of the
   *  VariableLengthVector is replaced by the buffer into which the data
   *  was directly read (a copy does not occur).
   *  If _letVectorManageData is true, the buffer persists even after this
   *  MetaArrayReader is destroyed. Otherwise, the buffer (and therefore
   *  the validity of the dat ain the VariableLengthVector) is destroyed
   *  when the MetaArrayReader is dstroyed. */
  template< typename TValue >
  void GetOutput(MET_ValueEnumType _metaElementType,
                 VariableLengthVector< TValue > *_vector,
                 bool _letVectorManageData = true)
  {
    if ( m_MetaArray.ElementType() != _metaElementType )
      {
      m_MetaArray.ConvertElementDataTo(_metaElementType);
      }
    _vector->SetData( (TValue *)( m_MetaArray.ElementData() ),
                      m_MetaArray.Length(),
                      _letVectorManageData );
    if ( _letVectorManageData )
      {
      m_MetaArray.AutoFreeElementData(false);
      }
  }

  /** Get an itkArray of Arrays, itk::Array< itk::Array<short> >.
   *  Assumes all sub-arrays have the same length.
   *  Specify the MetaType of the elements of the Array< Array< * > > and
   *  provide a pointer to the Array of arrays. Elements are copied
   *  into the array of arrays. */
  template< typename TValue >
  void GetMultiChannelOutput(MET_ValueEnumType _metaElementType,
                             Array< TValue > *_array)
  {
    if ( m_MetaArray.ElementType() != _metaElementType )
      {
      m_MetaArray.ConvertElementDataTo(_metaElementType);
      }
    int rows = m_MetaArray.Length();
    int cols = m_MetaArray.ElementNumberOfChannels();
    _array->SetSize(rows);
    for ( int i = 0; i < rows; i++ )
      {
      ( *_array )[i].SetSize(cols);
      for ( int j = 0; j < cols; j++ )
        {
        ( *_array )[i][j] = static_cast< typename TValue::ValueType >
                            ( m_MetaArray.ElementData(i * cols + j) );
        }
      }
  }

protected:

  MetaArrayReader();
  virtual ~MetaArrayReader() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  MetaArray m_MetaArray;

  std::string m_FileName;

  void *m_Buffer;
};
} // namespace itk

#endif // itkMetaArrayReader_h
