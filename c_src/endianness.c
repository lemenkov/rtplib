/*
 *   This is a simple compatibility shim to convert
 *     Linux endian macros to the Mac OS X equivalents.
 *       It is public domain.
 * */

#include <arpa/inet.h>

void htobe16_map(int16_t* buffer, size_t size)
{
	size_t i = 0;
	for(i = 0; i < size; i++)
		buffer[i] = ntohs(buffer[i]);
	return;
}
