/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorComponentDataAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVectorComponentDataAccessor_h
#define __itkVectorComponentDataAccessor_h

namespace itk {

/**
 * \class VectorComponentDataAccessor
 * \brief Provides flexible, pixel-level data access for
 *  vector-valued data that can be specified at run-time.
 *
 * This data accessor does not require compile-time specification of its
 * visible member, making scalar-method processing of arbitrary components of
 * vector-valued data possible.
 *
 * NOTE that this class may be phased out as the API for vector-valued data 
 * access stabilizes.
 */
template<class TInternalType, class TExternalType>
class VectorComponentDataAccessor
{
public:
  typedef TExternalType ExternalType;
  typedef TInternalType InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output[m_VisibleComponent] = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input[m_VisibleComponent]; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};

/**
 * Specializations for non-vector, native types follow.  These allow
 * native type instantiations of methods that use vector data accessors.
 */
template<>
class VectorComponentDataAccessor<float, float>
{
public:
  typedef float ExternalType;
  typedef float InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};

template<>
class VectorComponentDataAccessor<int, int>
{
public:
  typedef int ExternalType;
  typedef int InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};

template<>
class VectorComponentDataAccessor<bool, bool>
{
public:
  typedef bool ExternalType;
  typedef bool InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};

template<>
class VectorComponentDataAccessor<char, char>
{
public:
  typedef char ExternalType;
  typedef char InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};

template<>
class VectorComponentDataAccessor<unsigned char, unsigned char>
{
public:
  typedef unsigned char ExternalType;
  typedef unsigned char InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};


template<>
class VectorComponentDataAccessor<short, short>
{
public:
  typedef short ExternalType;
  typedef short InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};

template<>
class VectorComponentDataAccessor<unsigned short, unsigned short>
{
public:
  typedef unsigned short ExternalType;
  typedef unsigned short InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};



} // end namespace itk
  
#endif
