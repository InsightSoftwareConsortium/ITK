#ifndef CABLE_CONFIGURATION
// This file should *only* ever be included on windows systems!
// gcc-xml cannot be allowed to see the full MultiThreader header.
// If we're not gcc_xml, just pass through to the proper header.
#include "itkMultiThreader.h"
#else
// Here follows the restricted header made available just to gcc-xml on windows.
// The problem we are working around is the fact that on windows, the 
// ThreadFunctionType (defined in the full header) looks like this 
// unsigned long (__stdcall *)(void*)
// Unfortunately, gcc_xml and cswig can't properly wrap __stdcall functions,
// causing compile errors.
#ifndef __itkMultiThreader_h
#define __itkMultiThreader_h

#include "itkObject.h"
#include "itkMutexLock.h"

namespace itk
{
#ifdef ITK_USE_SPROC
#define ITK_MAX_THREADS              128
#endif
#ifdef ITK_USE_PTHREADS
#define ITK_MAX_THREADS              128
#endif
#ifdef ITK_USE_WIN32_THREADS
#define ITK_MAX_THREADS              128
#endif
#ifdef __CYGWIN__
#undef ITK_MAX_THREADS
#define ITK_MAX_THREADS 128 
#endif
#if defined(__MINGW32__)
#undef ITK_MAX_THREADS
#define ITK_MAX_THREADS 1 
#endif
#if defined(__sgi) && defined(_COMPILER_VERSION) && _COMPILER_VERSION <= 730
#undef ITK_MAX_THREADS
#define ITK_MAX_THREADS 1 
#endif
#ifndef ITK_MAX_THREADS
#define ITK_MAX_THREADS 1
#endif

class  MultiThreader : public Object 
  {
public:
    /** Standard class typedefs. */
    typedef MultiThreader         Self;
    typedef Object  Superclass;
    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;
    itkNewMacro(Self);  
    itkTypeMacro(MultiThreader, Object);
    itkSetClampMacro( NumberOfThreads, int, 1, ITK_MAX_THREADS );
    itkGetMacro( NumberOfThreads, int );
    static void SetGlobalMaximumNumberOfThreads(int val);
    static int  GetGlobalMaximumNumberOfThreads();
    static void SetGlobalDefaultNumberOfThreads(int val);
    static int  GetGlobalDefaultNumberOfThreads();
    void SingleMethodExecute();
    void MultipleMethodExecute();
    void TerminateThread( int thread_id );
    
#ifdef ITK_USE_SPROC
    static bool GetInitialized()
      { return m_Initialized; }
    static usptr_t * GetThreadArena()
      { return m_ThreadArena; }
    
    static void Initialize();
#endif
    friend class ProcessObject;
private:
  int                        m_NumberOfThreads;
  }; 
  
}  // end namespace itk
#endif // ifndef stuff
#endif // end if CABLE_CONFIGURATION
