INCLUDE (${CMAKE_ROOT}/Modules/FindITK.cmake)
IF(USE_ITK_FILE)
   INCLUDE(${USE_ITK_FILE})
ENDIF(USE_ITK_FILE)

LINK_LIBRARIES( ITKMetaIO )

SET( testMeta1Utils_SRCS testMeta1Utils.cxx )
ADD_EXECUTABLE( testMeta1Utils ${testMeta1Utils_SRCS} )

SET( testMeta2Object_SRCS testMeta2Object.cxx )
ADD_EXECUTABLE( testMeta2Object ${testMeta2Object_SRCS} )

SET( testMeta3Image_SRCS testMeta3Image.cxx )
ADD_EXECUTABLE( testMeta3Image ${testMeta3Image_SRCS} )

SET( testMeta4Tube_SRCS testMeta4Tube.cxx )
ADD_EXECUTABLE( testMeta4Tube ${testMeta4Tube_SRCS} )

SET( testMeta5Blob_SRCS testMeta5Blob.cxx )
ADD_EXECUTABLE( testMeta5Blob ${testMeta5Blob_SRCS} )

SET( testMeta6Surface_SRCS testMeta6Surface.cxx )
ADD_EXECUTABLE( testMeta6Surface ${testMeta6Surface_SRCS} )

SET( testMeta7Line_SRCS testMeta7Line.cxx )
ADD_EXECUTABLE( testMeta7Line ${testMeta7Line_SRCS} )

SET( testMeta8Scene_SRCS testMeta8Scene.cxx )
ADD_EXECUTABLE( testMeta8Scene ${testMeta8Scene_SRCS} )

SET( testMeta9Landmark_SRCS testMeta9Landmark.cxx )
ADD_EXECUTABLE( testMeta9Landmark ${testMeta9Landmark_SRCS} )