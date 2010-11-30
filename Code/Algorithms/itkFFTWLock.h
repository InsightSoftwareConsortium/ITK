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
#ifndef __itkFFTWLock_h
#define __itkFFTWLock_h

#include "itkSimpleFastMutexLock.h"
#if defined(USE_FFTWF) || defined(USE_FFTWD)
#include "fftw3.h"

namespace itk
{
/**
 * A simple global lock which must be called before calling FFTW unsafe functions.
 * It also handle cleanly the initialization and cleanup of FFTW.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/10380/3154
 * or http://insight-journal.com/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 */
class  FFTWLock
{

  public:

  static void Lock();
  static void Unlock();
  static void NewWisdomAvailable();

  enum { ESTIMATE=FFTW_ESTIMATE,
         PATIENT=FFTW_PATIENT,
         EXHAUSTIVE=FFTW_EXHAUSTIVE } Optimization;

  static int GetGlobalOptimizationLevel();
  static void SetGlobalOptimizationLevel(int opt);

  static std::string GetWisdomFileDefaultBaseName();

  bool ImportWisdomFileDouble( std::string fname );
  bool ExportWisdomFileDouble( std::string fname );
  bool ImportWisdomFileDouble();
  bool ExportWisdomFileDouble();

  bool ImportWisdomFileFloat( std::string fname );
  bool ExportWisdomFileFloat( std::string fname );
  bool ImportWisdomFileFloat();
  bool ExportWisdomFileFloat();

  protected:

  FFTWLock();
  ~FFTWLock();

  static FFTWLock            m_Singleton;
  static SimpleFastMutexLock m_Lock;
  static bool                m_NewWisdomAvailable;
  static int                 m_GlobalOptimizationLevel;

};
}
#endif
#endif
