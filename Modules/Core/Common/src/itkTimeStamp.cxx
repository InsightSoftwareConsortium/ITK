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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkTimeStamp.h"

namespace
{
/** \brief A function which does nothing
 *
 * This function is to be used to mark parameters as unused to suppress
 * compiler warning. It can be used when the parameter needs to be named
 * (i.e. itkNotUsed cannot be used) but is not always used. It ensures
 * that the parameter is not optimized out.
 */
template <typename T>
void Unused( const T &) {};

// This ensures that m_GlobalTimeStamp is has been initialized once the library
// has been loaded. In some cases, this call will perform the initialization.
// In other cases, static initializers like the IO factory initialization code
// will have done the initialization.
static ::itk::TimeStamp::GlobalTimeStampType * initializedGlobalTimeStamp = ::itk::TimeStamp::GetGlobalTimeStamp();

/** \class GlobalTimeStampInitializer
 *
 * \brief Initialize a GlobalTimeStamp and delete it on program
 * completion.
 * */
class GlobalTimeStampInitializer
{
public:
  typedef GlobalTimeStampInitializer            Self;
  typedef ::itk::TimeStamp::GlobalTimeStampType GlobalTimeStampType;

  GlobalTimeStampInitializer() {}

  /** Delete the time stamp if it was created. */
  ~GlobalTimeStampInitializer()
    {
    delete m_GlobalTimeStamp;
    m_GlobalTimeStamp = ITK_NULLPTR;
    }

  /** Create the GlobalTimeStamp if needed and return it. */
  static GlobalTimeStampType * GetGlobalTimeStamp()
    {
    if( !m_GlobalTimeStamp )
      {
      m_GlobalTimeStamp = new GlobalTimeStampType( 0 );

      // To avoid being optimized out. The compiler does not like this
      // statement at a higher scope.
      Unused(initializedGlobalTimeStamp);
      }
    return m_GlobalTimeStamp;
    }

private:
  static GlobalTimeStampType * m_GlobalTimeStamp;
};

// Takes care of cleaning up the GlobalTimeStamp
static GlobalTimeStampInitializer GlobalTimeStampInitializerInstance;
// Initialized by the compiler to zero
GlobalTimeStampInitializer::GlobalTimeStampType * GlobalTimeStampInitializer::m_GlobalTimeStamp;

} // end anonymous namespace


namespace itk
{

/**
 * Instance creation.
 */
TimeStamp *
TimeStamp
::New()
{
  return new Self;
}

/**
 * Make this timestamp to be the same as another one.
 */
const TimeStamp &
TimeStamp::operator=( const Self & other )
{
  this->m_ModifiedTime = other.m_ModifiedTime;
  return *this;
}


TimeStamp::GlobalTimeStampType *
TimeStamp
::GetGlobalTimeStamp()
{
  if( m_GlobalTimeStamp == ITK_NULLPTR )
    {
    m_GlobalTimeStamp = GlobalTimeStampInitializer::GetGlobalTimeStamp();
    }
  return m_GlobalTimeStamp;
}


void
TimeStamp
::SetGlobalTimeStamp( GlobalTimeStampType * timeStamp )
{
  m_GlobalTimeStamp = timeStamp;
}

/**
 * Make sure the new time stamp is greater than all others so far.
 */
void
TimeStamp
::Modified()
{
  // This is called once, on-demand to ensure that m_GlobalTimeStamp is
  // initialized.
  static GlobalTimeStampType * globalTimeStamp = GetGlobalTimeStamp();
  Unused(globalTimeStamp);
  this->m_ModifiedTime = ++(*m_GlobalTimeStamp);
}

TimeStamp::GlobalTimeStampType * TimeStamp::m_GlobalTimeStamp;


} // end namespace itk
