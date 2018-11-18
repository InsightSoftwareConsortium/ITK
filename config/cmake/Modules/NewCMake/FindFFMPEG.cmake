# Find the FFmpeg library
#
# Sets
#   FFMPEG_FOUND.  If false, don't try to use ffmpeg
#   FFMPEG_FOUND_SEVERAL. If true, there are several directories with headers (not only ../ffmpeg/)
#   FFMPEG_INCLUDE_DIR
#   FFMPEG_LIBRARIES

set( FFMPEG_FOUND "NO" )

find_path( FFMPEG_INCLUDE1_DIR ffmpeg/avcodec.h
  /usr/include
  /usr/local/include
)
find_path( FFMPEG_INCLUDE2_DIR libavcodec/avcodec.h
  /usr/include
  /usr/local/include
)
if( FFMPEG_INCLUDE1_DIR)
  set(FFMPEG_INCLUDE_DIR ${FFMPEG_INCLUDE1_DIR} )
  set( FFMPEG_FOUND_SEVERAL "NO" )
endif()

if( FFMPEG_INCLUDE2_DIR)
  set(FFMPEG_INCLUDE_DIR ${FFMPEG_INCLUDE2_DIR} )
  set( FFMPEG_FOUND_SEVERAL "YES" )
endif()

if( FFMPEG_INCLUDE_DIR )

find_program( FFMPEG_CONFIG ffmpeg-config
  /usr/bin
  /usr/local/bin
  ${HOME}/bin
)

if( FFMPEG_CONFIG )
  exec_program( ${FFMPEG_CONFIG} ARGS "--libs avformat" OUTPUT_VARIABLE FFMPEG_LIBS )
  set( FFMPEG_FOUND "YES" )
  set( FFMPEG_LIBRARIES "${FFMPEG_LIBS}" )

else()

  find_library( FFMPEG_avcodec_LIBRARY avcodec
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )

  find_library( FFMPEG_avformat_LIBRARY avformat
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )

  find_library( FFMPEG_avutil_LIBRARY avutil
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )

  find_library( FFMPEG_swscale_LIBRARY swscale
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )

  if( FFMPEG_avcodec_LIBRARY )
  if( FFMPEG_avformat_LIBRARY )

    set( FFMPEG_FOUND "YES" )
    set( FFMPEG_LIBRARIES ${FFMPEG_avformat_LIBRARY} ${FFMPEG_avcodec_LIBRARY} )
    if( FFMPEG_avutil_LIBRARY )
       set( FFMPEG_LIBRARIES ${FFMPEG_LIBRARIES} ${FFMPEG_avutil_LIBRARY} )
    endif()
    if( FFMPEG_swscale_LIBRARY )
       set( FFMPEG_LIBRARIES ${FFMPEG_LIBRARIES} ${FFMPEG_swscale_LIBRARY} )
    endif()

  endif()
  endif()

endif()

endif()
