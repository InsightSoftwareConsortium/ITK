set(itk-common_SRCS
LIST_OF_CXX_FILES
)

if(WIN32)
   set(itk-common_SRCS ${itk-common_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
endif(WIN32)


### genearting libraries
add_library(itk-common ${itk-common_SRCS})

target_link_libraries(itk-common  itksys itk-vnl-inst )

if(UNIX)
  target_link_libraries(itk-common ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
endif(UNIX)

itk_module_target(itk-common)
