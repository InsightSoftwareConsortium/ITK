# Find the FFmpeg library
#
# Sets
#   FFMPEG_FOUND.  If false, don't try to use ffmpeg
#   FFMPEG_INCLUDE_DIR
#   FFMPEG_LIBRARIES

SET( FFMPEG_FOUND "NO" )


FIND_PATH( FFMPEG_INCLUDE_DIR ffmpeg/avcodec.h
  /usr/include
  /usr/local/include
)

FIND_LIBRARY( FFMPEG_avcodec_LIBRARY avcodec
  /usr/lib
  /usr/local/lib
  /usr/lib64
  /usr/local/lib64
)

FIND_LIBRARY( FFMPEG_avformat_LIBRARY avformat
  /usr/lib
  /usr/local/lib
  /usr/lib64
  /usr/local/lib64
)

IF( FFMPEG_INCLUDE_DIR )
IF( FFMPEG_avcodec_LIBRARY )
IF( FFMPEG_avformat_LIBRARY )

  SET( FFMPEG_FOUND "YES" )
  SET( FFMPEG_LIBRARIES ${FFMPEG_avformat_LIBRARY} ${FFMPEG_avcodec_LIBRARY} )

ENDIF( FFMPEG_avformat_LIBRARY )
ENDIF( FFMPEG_avcodec_LIBRARY )
ENDIF( FFMPEG_INCLUDE_DIR )
