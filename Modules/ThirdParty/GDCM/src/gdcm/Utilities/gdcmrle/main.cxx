#include <iostream>
#include <vector>
#include <fstream>
#include <stddef.h>
#include <assert.h>

#include <stdlib.h>
#include <string.h>
#include "rle.h"

class memsrc : public ::rle::source
{
public:
	memsrc(const char * data, size_t datalen) :ptr(data), cur(data), len(datalen)
	{
	}
	int read(char * out, int l) 
	{
		memcpy(out, cur, l);
		cur += l;
    assert( cur <= ptr + len );
		return l;
	}
	streampos_t tell() 
	{
    assert( cur <= ptr + len );
		return (streampos_t)(cur - ptr);
	}
	bool seek(streampos_t pos) 
	{
		cur = ptr + pos;
    assert( cur >= ptr && cur <= ptr + len );
		return true;
	}
	bool eof() 
	{
    assert( cur >= ptr && cur <= ptr + len );
		return cur == ptr + len;
	}
	memsrc * clone() 
	{
		memsrc * ret = new memsrc(ptr, len);
		return ret;
	}
private:
	const char * ptr;
	const char * cur;
	size_t len;
};

class streamdest : public rle::dest
{
public:
	streamdest(size_t datalen)
	{
		ptr = (char*)malloc(datalen);
		cur = ptr;
		len = (int)datalen;
	}
	int write(const char * in, int l) 
	{
    std::cout << "w=" << l << std::endl;
    assert( l > 0 );
    assert( cur - ptr + l <= len );
		memcpy(cur, in, l);
		cur += l;
    assert( cur - ptr <= len );
		return l;
	}
	bool seek(streampos_t abs_pos) 
	{
    assert( cur - ptr + abs_pos <= len );
		cur =  ptr + abs_pos;
    assert( cur - ptr <= len );
		return true;
	}
	char * get_bulk() { return ptr; }
	size_t get_size() { return cur - ptr; }
private:
	char * ptr;
	char * cur;
	int len;
};

int main(int argc, char * argv[])
{
    const char * filename = argv[1];
    std::ifstream is( filename, std::ios::binary );
    int len = 258000;
    std::vector<char> v;
    v.resize(len);
    char* buf = &v[0];
    is.read( buf, len );
		rle::pixel_info pi(1/*samplesPerPixel*/, 8/*descr.bitsAllocated*/);
		rle::image_info ii(600/*descr.columns*/,430/* descr.rows*/, pi);
		const int h = 430;
		memsrc src((char*)buf, len);
		rle::rle_encoder re(src,ii);
		streamdest fd(len);
		if (!re.write_header(fd))
		{
			return 1;
		}

		for (int y = 0; y < h; ++y)
		{
			const int ret = re.encode_row(fd);
      std::cout << y << ";" << ret << std::endl;
			if (ret < 0)
			{
				return 1;
			}
		}
    return 0;
}
