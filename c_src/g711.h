#ifndef __G711_H__
#define __G711_H__

unsigned char Snack_Lin2Alaw(short pcm_val);
short Snack_Alaw2Lin(unsigned char a_val);
unsigned char Snack_Lin2Mulaw(short pcm_val);
short Snack_Mulaw2Lin(unsigned char u_val);

#endif
