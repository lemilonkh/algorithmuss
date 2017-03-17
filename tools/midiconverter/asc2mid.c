/*

asc2mid: 
 Converts ascii to midi. Ascii is in format output by mid2asc in one
 of several modes, or it can be in a more mixed format.

 Time of an event can be determined by one of
   (i) BA+CR:               Bar number+Crotchet within bar
  (ii) CR:                  Crotchet number from the very start
 (iii) DT:                  Delta time from previous event - native midi format
  (iv) FOL[+/-crotchets]:   Time from end of previous note/rest on that track
   (v) SIM[+crotchets]:     Time from start of previous note/rest on that track

  Basic complication of mode (i) is that the Time signature affects
  the interpretation of time, and that time signature can change at
  any time on any track and should affect all tracks.

  At the moment it requires lines of each track to be in chronological
  order which can make it slightly awkward to change the track of a
  note in sep=1 mode. If all file were in mode (ii), then it could
  sort it out for you, I suppose. Or the user could sort the lines
  first.

  Should make more case insensitive?

  A.P.Selby
  November 2002

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

#define MIN(x,y) ((x)<(y)?(x):(y))
#define MAX(x,y) ((x)>(y)?(x):(y))
#define MAXNOB 10000

char *inbuf,*inend,*ip;
int tr,infinity;
int maj[14]={0,2,4,5,7,9,11, 12,14,16,17,19,21,23};
typedef struct {int l,st,von,bt,pt,et,sta,bn,cr,tt,ln,ip,op;unsigned char *out;} trackinfo;
trackinfo *tri;
/*
  trackinfo:
  st = midi state (state of previous midi event laid down)
  bt = time of last event laid down
  pt = start time of last note or rest
  et = end time of last note or rest
  sta = track state
     state 0 ==> Need to read time of new line
     state 1 ==> Time of next line specified by tt
     state 2 ==> Time of next line specified by bn,cr; tt is valid if there is no time sig. change before it
     state 3 ==> Track at an end - no more lines on this track
  bn,cr,tt time of next event defined by (bn,cr) or tt according to state
  ip = integer input pointer (inbuf+.ip is actual pointer)
  ln = input line number
  out = pointer to start of output buffer (can change when gets resized)
  op = integer output pointer (.out+.op is actual pointer)
  l = allocated size of output buffer
  von = volume of last note-on event
*/

void err(char *l){
  if(tr==-1)fprintf(stderr,"Error: %s\n",l); else fprintf(stderr,"Error: %s at line %d:\n%s\n",l,tri[tr].ln,ip);
  exit(1);
}
#define assrt(x,y) if(!(x))err(y);

char *getns(char *l){while(*l==' ')l++; return l;}

int strtoi(char *l,char **e){
  int b,n;
  char *l1,*l2;
  l1=l;while(*l1!=0&&*l1!='&'&&!isdigit(*l1)&&strncmp(l1,"infinity",8))l1++;
  if(strncmp(l1,"infinity",8)==0)return infinity;
  if(*l1=='&'){l1++;b=16;} else if(*l1=='0'&&l1[1]=='x'){l1+=2;b=16;} else b=10;
  n=strtol(l1,&l2,b);
  if(e){if(l2==l1)*e=l; else *e=l2;}
  return n;
}

/*
int getfrn(char *l,int d){
  int a,b,c;
  char *l1;
  a=strtoi(l,&l1);
  if(l1[0]=='+')b=strtoi(l1+1,&l1); else {b=a;a=0;}
  if(a==infinity||b==infinity)return infinity;
  if(l1[0]=='/')c=strtoi(l1+1,0); else c=1;
  b+=a*c;
  assrt(d%c==0,"fraction not allowed");
  return b*(d/c);
}
*/
int getfrn(char *l,int d){
  int a,b,t;
  char *l1;
  t=0;
  do{
    a=strtoi(l,&l1);if(l1==l)break;
    if(a==infinity)return infinity;
    if(l1[0]=='/')b=strtoi(l1+1,&l1); else b=1;
    a*=d;assrt(a%b==0,"fraction not allowed");t+=a/b;
    l=l1;
  }while(l[0]=='+');
  return t;
}

int strton(char *l,char **e){
  int n;
  char *l1,*l2;
  while(*l==' ')l++;
  l1="CDEFGABR";l2=strchr(l1,toupper(*l));assrt(l2,"unrecognised note");
  if(l2-l1==7){*e=l+1;return -1;}
  n=60+maj[l2-l1];l++;
  while(*l=='#'){n++;l++;}
  while(*l=='b'){n--;l++;}
  while(*l=='\''){n+=12;l++;}
  while(*l=='-'){n-=12;l++;}
  assrt(n>=0&&n<128,"note out of range");
  *e=l;return n;
}

void acc(int p){
  int nl;
  unsigned char *new;
  if(p<=tri[tr].l)return;
  if(tri[tr].l==0)nl=10240; else nl=tri[tr].l*2;
  if(nl<p)nl=p;
  new=(unsigned char *)malloc(nl);if(!new)err("Couldn't malloc output track space");
  if(tri[tr].out){memcpy(new,tri[tr].out,tri[tr].op);free(tri[tr].out);}
  tri[tr].out=new;tri[tr].l=nl;
}
void putn(int n,int t){
  acc(tri[tr].op+n);
  while(n>0)tri[tr].out[tri[tr].op++]=(t>>(--n)*8)&255;
}
void put1(int t){putn(1,t);}
void put2(int t){putn(2,t);}
void put3(int t){putn(3,t);}
void put4(int t){putn(4,t);}
void putv(int t){
  int n,u;
  assrt(t>=0&&t<0x10000000,"variable number out of range (event out of sequence?)");
  for(n=1,u=t;u>=128;n++)u>>=7;
  acc(tri[tr].op+n);
  while(n>0)tri[tr].out[tri[tr].op++]=((t>>(--n)*7)&127)|128;
  tri[tr].out[tri[tr].op-1]&=~128;
}
void putstn(char *l,int s){acc(tri[tr].op+s);memcpy(tri[tr].out+tri[tr].op,l,s);tri[tr].op+=s;}
void putst(char *l){putstn(l,strlen(l));}

char *getl(int tr){
  char *l;
  l=inbuf+tri[tr].ip;if(tri[tr].ln>0)l+=strlen(l)+1;
  tri[tr].ln++;
  while(l<inend&&(l[0]==0||l[0]=='#'||strncmp(l,"format=",7)==0)){l+=strlen(l)+1;tri[tr].ln++;}
  tri[tr].ip=l-inbuf;
  if(l<inend)return l; else return 0;
}

typedef struct {int tr,tt,ch,nt,voff;} noteoffbuf;
int main(int ac,char **av){
  int d,i,n,s,t,cr,bn,ch,nxt,mi,tr0,non,sf,st,rest,tis0,tis1,tis2,tpc,text,tt,siz,format,tracks,division;
  char *l1,*l2,*l3;
  noteoffbuf *nob;
  FILE *fpi;

  if(ac<2){
    fprintf(stderr,"Usage: asc2mid textfile > midifile\n\n");
    fprintf(stderr," Time of an event can be determined by one of\n");
    fprintf(stderr,"   (i) BA+CR:               Bar number+Crotchet within bar\n");
    fprintf(stderr,"  (ii) CR:                  Crotchet number from the very start\n");
    fprintf(stderr," (iii) DT:                  Delta time from previous event - native midi format\n");
    fprintf(stderr,"  (iv) FOL[+/-crotchets]:   Time from end of previous note/rest\n");
    fprintf(stderr,"   (v) SIM[+crotchets]:     Time from start of previous note/rest\n");
    exit(1);
  }
  fpi=fopen(av[1],"r");if(!fpi){fprintf(stderr,"Couldn't open %s\n",av[1]);exit(1);}
  fseek(fpi,0,SEEK_END);siz=ftell(fpi);rewind(fpi);
  fprintf(stderr,"File size %d byte%s\n",siz,siz==1?"":"s");
  inbuf=(char*)malloc(siz+1);if(!inbuf){fprintf(stderr,"Couldn't malloc %d bytes (inbuf)\n",siz+1);exit(1);}
  n=fread(inbuf,1,siz,fpi);assert(n<=siz);siz=n;inbuf[siz]=0;inend=inbuf+siz;fclose(fpi);
  for(i=0;i<siz;i++)if(inbuf[i]=='\n')inbuf[i]=0;
  
  ip=inbuf;tr-=1;
  while(ip<inend&&strncmp(ip,"format=",7))ip+=strlen(ip)+1;
  if(ip==inend)err("Didn't find format line");
  l1=strstr(ip,"format=");assert(l1);format=strtoi(l1+7,0);
  l1=strstr(ip,"tracks=");assert(l1);tracks=strtoi(l1+7,0);
  l1=strstr(ip,"division=");assert(l1);division=strtoi(l1+9,0);
  fprintf(stderr,"format=%d tracks=%d division=%d\n",format,tracks,division);

  nob=(noteoffbuf *)malloc(MAXNOB*sizeof(noteoffbuf));assert(nob);
  tri=(trackinfo*)malloc((tracks+1)*sizeof(trackinfo));assert(tri); /* Track number <tracks> is for the preamble */
  for(i=0;i<=tracks;i++){
    tri[i].out=0;tri[i].ip=0;tri[i].op=(i<tracks?8:0);/* leave 8 bytes for MTrk, length */
    tri[i].l=0;tri[i].ln=0;
    tri[i].bt=tri[i].pt=tri[i].et=0;tri[i].von=100;
    tri[i].bn=tri[i].cr=tri[i].tt=0;tri[i].sta=0;
    tri[i].st=-1;
  }
  
  if(division&(1<<15))tpc=48; else tpc=division;
  infinity=0x7fffffff;
  tr=tracks;putst("MThd");put4(6);put2(format);put2(tracks);put2(division);
  
  tis0=4;tis1=4;tis2=(tis0*4*tpc)/tis1;/* Initial time signature 4/4 */
  tt=0;bn=0;cr=0;
  /* (tt,bn,cr) are always kept in step, and it is guaranteed that there
     are no time sig. changes between tt and the present new time. */
  non=0;/* number of pending note-off events */
  while(1){
    nxt=infinity;tr0=-1;
    for(tr=0;tr<tracks;tr++){
      if(tri[tr].sta==3)goto nl0;
      if(tri[tr].sta==0){
	do{
	  ip=getl(tr);if(!ip){tri[tr].sta=3;goto nl0;}
	  ip=getns(ip);
	  l1=strstr(ip,"TR ");if(!l1)err("Expect TR");
	}while(strtoi(l1+3,0)!=tr);
	if(strncmp(ip,"DT ",3)==0){tri[tr].tt=tri[tr].bt+getfrn(ip+3,tpc);tri[tr].sta=1;goto ok0;}
	if(strncmp(ip,"FOL",3)==0||strncmp(ip,"SIM",3)==0){
	  tri[tr].tt=tri[tr].pt;
	  if(ip[0]=='F'){
	    if(tri[tr].et>=0)tri[tr].tt=tri[tr].et; else
	      fprintf(stderr,"Warning - FOL reverts to SIM following a DT-defined or infinite length note/rest\n");
	  }
	  if(ip[3]=='+')tri[tr].tt+=getfrn(ip+4,tpc);
	  if(ip[3]=='-')tri[tr].tt-=getfrn(ip+4,tpc);
	  tri[tr].sta=1;goto ok0;
	}
	if(strncmp(ip,"CR ",3)==0){
	  tri[tr].tt=getfrn(ip+3,tpc);
	  tri[tr].sta=1;goto ok0;
	}
	if(strncmp(ip,"BA ",3)==0){
	  tri[tr].bn=strtoi(ip+3,&l1)-1;
	  l1=getns(l1);if(strncmp(l1,"CR ",3))err("Expect CR after BA");
	  tri[tr].cr=getfrn(l1+3,tpc);
	  tri[tr].sta=2;goto ok0;
	}
	err("Expected time specification (DT, FOL, SIM, CR, or BA) at start of line");
      }
    ok0:
      if(tri[tr].sta==2)tri[tr].tt=tt+(tri[tr].bn-bn)*tis2+tri[tr].cr-cr;
      if(tri[tr].tt<nxt){nxt=tri[tr].tt;tr0=tr;}
    nl0:
      tr=tr;/* because ISO C forbids label at end of compound statement, apparently */
    }
    if(tr0==-1)break;
    tr=tr0;ip=inbuf+tri[tr].ip;tri[tr].sta=0;
    
    text=(strstr(ip,"Text")!=0);
    /* Need to know if Text line, since otherwise the quoted string
       could cause a false match with some other key word */
    t=nxt-tt+cr; bn+=t/tis2; cr=t%tis2; tt=nxt;
    /* At this point, tt=time of next non-note-off event.
       Have to lay down note-off events which have been bypassed. */
    tr0=tr;
    for(i=0;i<non&&nob[i].tt<=tt;i++){
      tr=nob[i].tr;putv(nob[i].tt-tri[tr].bt);tri[tr].bt=nob[i].tt;
      if(nob[i].voff!=0x40){
	st=0x80+nob[i].ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
	put1(nob[i].nt);put1(nob[i].voff);
      }else{
	st=0x90+nob[i].ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
	put1(nob[i].nt);put1(0);
      }
    }
    if(i>0){non-=i;memmove(nob,nob+i,non*sizeof(noteoffbuf));}
    tr=tr0;
    rest=0;if(!text){l1=strstr(ip,"NT ");if(l1)rest=(toupper(*(getns(l1+3)))=='R');}
    if(!rest){putv(tt-tri[tr].bt);tri[tr].bt=tt;}
    if(text){
      put1(tri[tr].st=0xFF);
      l1=strstr(ip,"type");assrt(l1,"missing 'type' in text line");t=strtoi(l1+4,0);put1(t);
      l1=strchr(l1+4,'"');assrt(l1,"missing opening quote in text line");l1++;
      l2=strrchr(l1,'"');assrt(l2,"missing closing quote in text line");
      for(i=s=0;i<l2-l1;i++){
	t=l1[i];
	if(t=='\\'&&i<l2-l1-1){
	  t=l1[++i];
	  if(t=='t')t=9;
	  if(t=='n')t=10;
	  if(t=='v')t=11;
	  if(t=='f')t=12;
	  if(t=='r')t=13;
	}
	l1[s++]=t;
      }
      putv(s);putstn(l1,s);
      continue;
    }
    l1=strstr(ip,"CH ");assrt(l1,"missing channel number");ch=strtoi(l1+3,0)-1;assrt(ch>=0&&ch<16,"channel number out of range");
    l1=strstr(ip,"Time signature");if(l1){
      tis0=strtoi(l1+14,&l1);tis1=strtoi(l1+1,0);assrt((tis0*4*tpc)%tis1==0,"New time signature not commensurate with division");
      tis2=(tis0*4*tpc)/tis1;
      put1(tri[tr].st=0xFF);
      put1(0x58);putv(4);put1(tis0);
      for(i=0,n=tis1;n>1;i++)n>>=1;put1(i);
      l2=strstr(ip,"clocks/mtick");assrt(l2,"missing clocks/mtick");put1(strtoi(l2+12,0));
      l2=strstr(ip,"crotchets/32ndnote");assrt(l2,"missing crotchets/32ndnote");put1(strtoi(l2+18,0));
      continue;
    }
    l1=strstr(ip,"Channel volume");if(l1){
      st=0xB0+ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
      t=strtoi(l1+14,0);assrt(t>=0&&t<128,"channel volume out of range");put1(7);put1(t);
      continue;
    }
    l1=strstr(ip,"Instrument");if(l1){
      st=0xC0+ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
      t=strtoi(l1+10,0)-1;assrt(t>=0&&t<128,"instrument out of range");put1(t);
      continue;
    }
    l1=strstr(ip,"Sysex event");if(l1){
      put1(tri[tr].st=strtoi(l1+11,&l1));
      n=0;l2=l1;while(1){strtoi(l2,&l3);if(l3==l2)break;l2=l3;n++;}
      putv(n);for(l2=l1;n>0;n--)put1(strtoi(l2,&l2));
      continue;
    }
    l1=strstr(ip,"End of track");if(l1){
      put1(tri[tr].st=0xFF);
      put1(0x2F);put1(0);
      continue;
    }
    l1=strstr(ip,"Tempo");if(l1){
      put1(tri[tr].st=0xFF);
      n=(int)(60*1000000./atof(l1+5)+.5);assrt(n>=0&&n<=0xFFFFFF,"tempo out of range");
      put1(0x51);putv(3);put3(n);
      continue;
    }
    l1=strstr(ip,"Key ");if(l1){
      put1(tri[tr].st=0xFF);
      l1=getns(l1+4);l2="FCGDAEB";l3=strchr(l2,toupper(*l1));assrt(l3,"unrecognised key");sf=l3-l2-1;
      do{l1++;sf+=7*((*l1=='#')-(*l1=='b'));}while(*l1=='#'||*l1=='b');
      l2=strstr(l1," major");l3=strstr(l1," minor");assrt((l2!=0)^(l3!=0),"missing minor/major in key");
      mi=(l3!=0);sf-=3*mi;
      put1(0x59);putv(2);put1(sf&255);put1(mi);
      continue;
    }
    l1=strstr(ip,"Meta Event");if(l1){
      put1(tri[tr].st=0xFF);
      put1(strtoi(l1+10,&l1));
      n=0;l2=l1;while(1){strtoi(l2,&l3);if(l3==l2)break;l2=l3;n++;}
      putv(n);for(l2=l1;n>0;n--)put1(strtoi(l2,&l2));
      continue;
    }      
    l1=strstr(ip,"ST ");if(l1){
      st=strtoi(l1+3,&l1);if(st!=tri[tr].st||((st&0xF0)==0xF0))put1(tri[tr].st=st);
      while(1){n=strtoi(l1,&l2);if(l2==l1)break;l1=l2;put1(n);}
      continue;
    }
    l1=strstr(ip,"NT ");if(l1){
      tri[tr].pt=tt;tri[tr].et=-1;
      n=strton(l1+3,&l1);l1=getns(l1);
      if(strncmp(l1,"on",2)==0){
	assrt(n>=0,"can't have rest and 'on'");
	st=0x90+ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
	l2=strstr(l1,"von=");if(l2)tri[tr].von=strtoi(l2+4,0);
	assrt(tri[tr].von>=1&&tri[tr].von<128,"von out of range");
	put1(n);put1(tri[tr].von);
	continue;
      }
      if(strncmp(l1,"off",3)==0){
	l2=strstr(l1,"voff=");if(l2)t=strtoi(l2+5,0); else t=0x40;
	assrt(t>=0&&t<128,"voff out of range");
	if(t!=0x40){/* not most efficient coding, but most likely to mimic convention used by others(?) */
	  st=0x80+ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
	  put1(n);put1(t);
	}else{
	  st=0x90+ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
	  put1(n);put1(0);
	}
	continue;
      }
      d=getfrn(l1,tpc);if(d<infinity){nxt=tt+d;tri[tr].et=nxt;}
      if(rest)continue;
      assert(n>=0);
      l2=strstr(l1,"von=");if(l2)tri[tr].von=strtoi(l2+4,0);
      assrt(tri[tr].von>=1&&tri[tr].von<128,"von out of range");
      st=0x90+ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
      put1(n);put1(tri[tr].von);
      if(d<infinity){
	l2=strstr(l1,"voff=");if(l2)t=strtoi(l2+5,0); else t=0x40;
	for(i=0;i<non;i++)if(nob[i].ch==ch&&nob[i].nt==n){
	  fprintf(stderr,"Warning - overlapping note at line %d:\n%s\n",tri[tr].ln,ip);
	  /*
	    memmove(nob+i,nob+i+1,(non-(i+1))*sizeof(noteoffbuf));non--;
	    Can't necessarily delete note-off event when notes overlap because
	    some devices might stack the extra soundings of same note, and so
	    deleting this event may result in a note stuck on.
	  */
	  break;
	}
	assrt(non<MAXNOB,"Note-off buffer full");
	for(i=0;i<non&&nob[i].tt<=nxt;i++);
	memmove(nob+i+1,nob+i,(non-i)*sizeof(noteoffbuf));non++;
	nob[i].tr=tr;nob[i].tt=nxt;nob[i].ch=ch;nob[i].nt=n;nob[i].voff=t;
      }
      continue;
    }
    fprintf(stderr,"Unrecognised line %d:\n%s\n",tri[tr].ln,ip);exit(1);
  }/* main loop */
  for(i=0;i<non;i++){
    tr=nob[i].tr;putv(nob[i].tt-tri[tr].bt);tri[tr].bt=nob[i].tt;
    if(nob[i].voff!=0x40){
      st=0x80+nob[i].ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
      put1(nob[i].nt);put1(nob[i].voff);
    }else{
      st=0x90+nob[i].ch;if(st!=tri[tr].st)put1(tri[tr].st=st);
      put1(nob[i].nt);put1(0);
    }
  }
  if(i>0){non-=i;memmove(nob,nob+i,non*sizeof(noteoffbuf));}
  fprintf(stderr,"Successfully parsed\n");
  fwrite(tri[tracks].out,1,tri[tracks].op,stdout);
  for(tr=0;tr<tracks;tr++){
    t=tri[tr].op;tri[tr].op=0;putst("MTrk");put4(t-8);
    if(tri[tr].out)fwrite(tri[tr].out,1,t,stdout); else assert(t==0);
  }
  return 0;
}
