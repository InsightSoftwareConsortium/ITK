set(ITK-Common_SRCS
LIST_OF_CXX_FILES
)

if(WIN32)
   set(ITK-Common_SRCS ${ITK-Common_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
endif(WIN32)


### genearting libraries
add_library(ITK-Common ${ITK-Common_SRCS})

target_link_libraries(ITK-Common  itksys itk-vnl-instantiation )

if(UNIX)
  target_link_libraries(ITK-Common ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
endif(UNIX)

itk_module_target(ITK-Common)
