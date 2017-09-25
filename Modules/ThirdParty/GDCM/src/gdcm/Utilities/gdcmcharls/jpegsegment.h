//
// (C) CharLS Team 2014, all rights reserved. See the accompanying "License.txt" for licensed use. 
//

#ifndef CHARLS_JPEGSEGMENT
#define CHARLS_JPEGSEGMENT

class JpegStreamWriter;

//
// Purpose: base class for segments that can be written to JPEG streams.
//
class JpegSegment
{
protected:
	JpegSegment() {}
public:
	virtual ~JpegSegment() {}
	virtual void Serialize(JpegStreamWriter& streamWriter) = 0;
};

#endif
