SUBDIRS(Numerics Common BasicFilters Algorithms)

LINK_LIBRARIES (
VXLNumerics
)

UNIX_LIBRARIES(
${THREAD_LIBS} 
${DL_LIBS} 
-lm
)


