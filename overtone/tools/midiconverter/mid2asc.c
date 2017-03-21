/*

mid2asc:
 Converts midi to ascii. Output can be in one of several formats
 determined by command line flags.

 Time of an event can be determined by one of
   (i) BA+CR:               Bar number+Crotchet within bar
  (ii) CR:                  Crotchet number from the very start
 (iii) DT:                  Delta time from previous event - native midi format
  (iv) FOL[+/-crotchets]:   Time from end of previous note/rest on that track
   (v) SIM[+crotchets]:     Time from start of previous note/rest on that track

  Basic complication of mode (i) is that the Time signature affects
  the interpretation of time, and that time signature can change at
  any time on any track and should affect all tracks.

  Could make it closer to the spec. (e.g., skip over unknown chunk types)
  
  Midi files are going to be ambiguous, e.g. order of some events
  occuring at the same time, or whether choose to use running st mode,
  or whether to choose to use 0x8* or 0x9* for voff when
  velocity=0x40.

  A.P.Selby
  November 2002

  Could print key changes from other tracks as comments in sep mode

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#define MIN(x,y) ((x)<(y)?(x):(y))
#define MAX(x,y) ((x)>(y)?(x):(y))

#define MAXLL 10000
#define MAXCH 16
#define MAXNT 128
unsigned char *dat;
int p,siz,pr=0,raw,cro,fol,sep,frpad=9;
int bgn[12][2]={{0,0},{0,1},{1,0},{2,-1},{2,0},{3,0},{3,1},{4,0},{4,1},{5,0},{6,-1},{6,0}};
/* ^ best guess note from pitch relative to keynote: (In C maj/A min this would be: C C# D Eb E F F# G G# A Bb B) */
int maj[14]={0,2,4,5,7,9,11, 12,14,16,17,19,21,23};

void err(char *l){fprintf(stderr,"Error: %s\n",l);exit(1);}
#define assrt(x,y) if(!(x))err(y);

char *note2(int n,int s,int pad){
  static char buf[100][20];
  static int p=0;
  int i;
  p=(p+1)%100;
  buf[p][0]="CDEFGAB"[(n+700000)%7];
  i=1;
  while(s>0){assert(i<19);buf[p][i++]='#';s--;}
  while(s<0){assert(i<19);buf[p][i++]='b';s++;}
  while(n>=7){assert(i<19);buf[p][i++]='\'';n-=7;}
  while(n<0){assert(i<19);buf[p][i++]='-';n+=7;}
  if(pad)while(i<pad&&i<19)buf[p][i++]=' ';
  buf[p][i]=0;
  return buf[p];
}
char *note(int n,int sf,int mi,int pad){
  int a,b,m,a0,b0;
  assert(n>=0&&n<128&&sf>=-128&&sf<=127);
  a0=(4*sf+7000)%7;b0=(sf+7001)/7-1000;/* (a0,b0) = keynote of (sf,mi) in pair form */
  n-=60+maj[a0]+b0;/* subtract keynote in single pitch form */
  m=(n+120000)%12;a=bgn[m][0];b=bgn[m][1];/* look up most likely pair form */
  return note2(a0+a+((n-m)/12)*7,maj[a0]+maj[a]-maj[a0+a]+b0+b,pad);/* evaluate cocycle! */
}
char *key(int sf,int mi,int pad){
  static char buf[100][21];
  static int p=0;
  int i,n;
  p=(p+1)%100;
  assert(sf>=-128&&sf<=127);
  if(mi)sf+=3;
  buf[p][0]="FCGDAEB"[(sf+7001)%7];
  n=(sf+7001)/7-1000;i=1;
  while(n>0){buf[p][i++]='#';n--;}
  while(n<0){buf[p][i++]='b';n++;}
  while(i<pad&&i<20)buf[p][i++]=' ';
  buf[p][i]=0;
  return buf[p];
}
int gcd(int a,int b){if(b==0)return a; else return gcd(b,a%b);}
int sprfrac(char *l,int n,int d,int sum,int pad){
  int a,g,s;
  s=0;
  if(sum){
    a=n/d;if(a){
      s=sprintf(l,"%d",a);n-=a*d;
      if(n==0)goto pad;
      s+=sprintf(l+s,"+");
    }
  }
  g=gcd(n,d);n/=g;d/=g;
  if(d!=1)s+=sprintf(l+s,"%d/%d",n,d); else s+=sprintf(l+s,"%d",n);
 pad:
  if(s<pad){memmove(l+pad-s,l,s+1);memset(l,' ',pad-s);s=pad;}
  return s;
}
int sprfrac2(char *li,int n,int d){
  /* Prints the simplest (or near simplest) number x such that (int)(n/x+.5)=d */
  int i,t,dp;
  double m,r,x0,x1;
  char l[100],*l0,*l1;
  if(li)l0=li; else l0=l;
  x0=n/(d+.49);x1=n/(d-.49);l1=l0;
  /* Find minimum dp such that [x0,x1] intersects Z*10^(-dp) */
  for(dp=0,m=1;dp<15;dp++){
    r=floor(x1*m);
    if(r>x0*m){
      t=(int)(r/m);
      l1+=sprintf(l1,"%d",t);r-=t*m;
      if(dp){
	l1+=sprintf(l1,".");
	for(i=0;i<dp;i++){m/=10;t=(int)(r/m);l1+=sprintf(l1,"%d",t);r-=t*m;}
      }
      if((int)(n/atof(l0)+.5)!=d){fprintf(stderr,"Error 2 in prfrac2 n=%d d=%d\n",n,d);exit(1);}
      if(li)return l1-l0; else return printf("%s",l);
    }
    m*=10;
  }
  fprintf(stderr,"Error 1 in prfrac2 n=%d d=%d\n",n,d);exit(1);
}

void need(int n,char *l){
  if(p+n>siz){fprintf(stderr,"Unexpected EOF in %s, p=&%06x n=%d siz=&%06x\n",l,p,n,siz);exit(1);}
}
void skip(int n){need(n,"skip");p+=n;}
void match(char *l){
  int i;
  need(strlen(l),"match");
  for(i=0;l[i];i++)if(l[i]!=dat[p++]){
    fprintf(stderr,"Bad match, p=%X i=%d l=%s &%02X!=&%02X\n",p,i,l,l[i],dat[p-1]);exit(1);
  }
}
int getn(int n,int a){
  int i,t;
  need(n,"getn");
  for(i=0,t=0;i<n;i++)t=(t<<8)+dat[p+i];
  if(a)p+=n;
  return t;
}
int get1(){return getn(1,1);}
int get2(){return getn(2,1);}
int get3(){return getn(3,1);}
int get4(){return getn(4,1);}
int look1(){return getn(1,0);}
int getv(){
  int c,t;
  t=0;
  do{
    need(1,"getv");c=dat[p++];t=(t<<7)+(c&127);
  }while(c&128);
  return t;
}
int getnnodt(int st,int e,int *st1){
  /* Returns the dt to next non-noteoff event (raw=0), or next event (raw=1). return=-1 means end of track. */
  int t,p0,dt,sth;
  dt=0;
  while(p<e){
    dt+=getv();*st1=st;if(raw)return dt;
    p0=p;
    t=look1();if(t>=0x80)st=get1(); else assert(st>=0x80);
    sth=st>>4;
    if(sth==8||(sth==9&&dat[p+1]==0))skip(2); else {p=p0;return dt;}
  }
  return -1;
}

typedef struct {int tt,ch,nt,voff;} noteoffbuf;
int cmp(const void*x,const void*y){return ((noteoffbuf*)x)->tt-((noteoffbuf*)y)->tt;}

int main(int ac,char **av){
  int i,s,t,u,bf,bn,btt,fol1,rest,ch,dt,infinity,ln,mi,maxno,nat,non,ols,prb,help,
    sk,sf,st,sth,tis0,tis1,tis2,tpc,tr,trl,tt,ty,alltrfl,format,tracks,division,tempo,nobp[MAXCH][MAXNT];
  char l[MAXLL],*l1;
  double ert;
  typedef struct {int a,p0,l,e,p,pv,st,dt,tt,et,pt;} trackinfo;
  typedef struct fred {int all,bn,tr;char *l;struct fred *nx;} outline;
  trackinfo *tri;
  noteoffbuf *nob;
  outline *outbuf0,*outbuf;
  FILE *fpi;

  raw=cro=fol=sep=help=0;
  for(i=1;i<ac;i++){
    if(!strncmp(av[i],"-c",2))cro=1;
    if(!strncmp(av[i],"-f",2))fol=1;
    if(!strncmp(av[i],"-r",2))raw=1;
    if(!strncmp(av[i],"-s",2))sep=1;
    if(!strncmp(av[i],"-h",2)||!strncmp(av[i],"--help",6))help=1;
  }
  if(ac<2||help){
    fprintf(stderr,"Usage: mid2asc  [-c] [-f,-r] [-s] midifile > textfile\n\n");
    fprintf(stderr,"   Default timing is to use absolute time determined by BA+CR\n");
    fprintf(stderr,"   and use durations for notes.\n");
    fprintf(stderr,"-c Use absolute crotchets rather than BA+CR to determine time.\n");
    fprintf(stderr,"   Useful if inserting or deleting.\n");
    fprintf(stderr,"-f 'Follow' (take time from start or end of previous note) to determine time.\n");
    fprintf(stderr,"-r \"raw\" format: time determined by DT, and note-off events\n");
    fprintf(stderr,"   are used instead of note durations.\n\n");
    fprintf(stderr,"   Default is to print all tracks merged in chronological order.\n");
    fprintf(stderr,"-s Print all tracks separately.\n");
    exit(1);
  }
  if(fol&&raw){fprintf(stderr,"fol and raw modes cannot be combined\n");exit(1);}
  fpi=fopen(av[ac-1],"rb");if(!fpi){fprintf(stderr,"Couldn't open %s\n",av[ac-1]);exit(1);}
  fseek(fpi,0,SEEK_END);siz=ftell(fpi);rewind(fpi);
  if(pr>=1)printf("# File size %d byte%s\n",siz,siz==1?"":"s");
  dat=(unsigned char*)malloc(siz);if(!dat){fprintf(stderr,"Couldn't malloc %d bytes\n",siz);exit(1);}
  fread(dat,1,siz,fpi);p=0;fclose(fpi);
  maxno=siz/2;
  match("MThd");
  assert(get4()==6);
  format=get2();tracks=get2();division=get2();
  printf("format=%d tracks=%d division=%d\n",format,tracks,division);
  if(division&(1<<15))tpc=48; else tpc=division;
  tri=(trackinfo *)malloc(tracks*sizeof(trackinfo));assert(tri);
  nob=(noteoffbuf *)malloc(maxno*sizeof(noteoffbuf));assert(nob);
  infinity=0x7fffffff;

  for(tr=0;p<siz&&tr<tracks;tr++){
    match("MTrk");trl=get4();
    tri[tr].p0=p;tri[tr].l=trl;tri[tr].e=tri[tr].p0+tri[tr].l;
    p+=trl;
  }

  non=0;/* outside 'if' statement to stop warning */
  if(!raw){
    for(tr=0;tr<tracks;tr++){
      st=0;tt=0;p=tri[tr].p0;
      while(p<tri[tr].e){
	dt=getv();tt+=dt;
	t=look1();if(t>=0x80)st=get1(); else assert(st>=0x80);
	sth=st>>4;ch=st&15;
	if(sth==8||sth==9){
	  t=get1();u=get1();
	  if(sth==9){if(u)continue; else u=0x40;}
	  assert(non<maxno);nob[non].tt=tt;nob[non].ch=ch;nob[non].nt=t;nob[non].voff=u;non++;
	  continue;
	}
	if(sth==0xC||sth==0xD){skip(1);continue;}
	if(sth==0xF){
	  if(st==0xF0||st==0xF7){skip(getv());continue;}
	  if(st==0xFF){skip(1);skip(getv());continue;}
	  fprintf(stderr,"Unrecognised status %02x\n",st);exit(1);
	}
	skip(2);
      }
    }
    qsort(nob,non,sizeof(noteoffbuf),cmp);
    for(ch=0;ch<MAXCH;ch++)for(t=0;t<MAXNT;t++)nobp[ch][t]=0;
  }

  nat=0;
  for(tr=0;tr<tracks;tr++){
    p=tri[tr].p=tri[tr].p0;
    t=getnnodt(0,tri[tr].e,&tri[tr].st);tri[tr].a=(t>=0);
    if(tri[tr].a){
      tri[tr].et=tri[tr].pt=0;tri[tr].dt=tri[tr].tt=t;tri[tr].p=p;tri[tr].pv=-1;nat++;
    }
  }
  sf=0;mi=0;/* Initial key C major */
  tis0=4;tis1=4;tis2=(tis0*4*tpc)/tis1;/* Initial time signature 4/4 */
  tempo=500000;/* Default usec/beat (= 120 bpm) */
  btt=0;bn=0;bf=0;ert=0;/* at time btt, bar number was bn (from 0), and part of bar complete was bf/tpc crotchets */
  prb=-1;
  ols=MAXLL*10;outbuf=outbuf0=(outline*)malloc(ols);outbuf->nx=0;
  if(!outbuf){fprintf(stderr,"Couldn't malloc %d bytes (outbuf1)\n",ols);exit(1);}
  while(nat>0){
    tt=infinity;tr=-1;alltrfl=0;
    for(i=0;i<tracks;i++)if(tri[i].a){
      t=tri[i].tt;if(fol&&tri[i].et<t)t=tri[i].et;
      if(t<tt){tr=i;tt=t;}
    }
    assert(tr>=0);
    p=tri[tr].p;t=look1();if(t>=0x80)st=get1(); else st=tri[tr].st;
    sth=st>>4;ch=st&15;
    assert(!(raw==0&&(sth==8||(sth==9&&dat[p+1]==0))));
    rest=(fol&&tri[tr].et<tri[tr].tt);
    fol1=(fol&&tri[tr].et<=tri[tr].tt);
    l1=l;
    if(tt>btt){
      ert+=(double)(tt-btt)*(double)tempo/(double)tpc;
      /* Elapsed crotchets=(btt-tt)/tpc, ticks/bar=tis2 */
      t=tt-btt+bf; bn+=t/tis2; bf=t%tis2; btt=tt;
    }
    if(!sep&&bn!=prb){l1+=sprintf(l1,"\n");prb=bn;}
    if(fol){
      u=0;
      if(fol1)l1+=sprintf(l1,"FOL"); else {
	l1+=sprintf(l1,"SIM");t=tri[tr].tt-tri[tr].pt;if(t){
	  assert(t>0);l1+=sprintf(l1,"+");u=sprfrac(l1,t,tpc,1,0);l1+=u;u++;
	}
      }
      while(u<frpad+1){*(l1++)=' ';u++;}
      l1+=sprintf(l1," (");
    }
    if(raw){l1+=sprintf(l1,"DT ");l1+=sprfrac(l1,tri[tr].dt,tpc,1,frpad);l1+=sprintf(l1,"   (");}
    if(cro){l1+=sprintf(l1,"CR ");l1+=sprfrac(l1,tt,tpc,1,4+frpad);l1+=sprintf(l1,"   (");}
    l1+=sprintf(l1,"BA %4d   CR ",bn+1);l1+=sprfrac(l1,bf,tpc,1,frpad);
    if(cro)l1+=sprintf(l1,")");
    if(raw)l1+=sprintf(l1,")");
    if(fol)l1+=sprintf(l1,")");
    l1+=sprintf(l1,"   TR %2d   CH %2d   ",tr,ch+1);
    if(rest){
      l1+=sprintf(l1,"NT  R       ");l1+=sprfrac(l1,tri[tr].tt-tt,tpc,1,frpad);l1+=sprintf(l1,"\n");
      tri[tr].pt=tt;tri[tr].et=tri[tr].tt;goto bp2;
    }
    if(sth==8||sth==9){
      t=get1();u=get1();
      l1+=sprintf(l1,"NT  %s ",note(t,sf,mi,7));
      if(sth==8||(sth==9&&u==0)){
	assert(raw);
	l1+=sprintf(l1,"off");
	if(sth==8&&u!=0x40)l1+=sprintf(l1,"   voff=%d",u);
	l1+=sprintf(l1,"\n");
	goto bp1;
      }
      if(raw)l1+=sprintf(l1,"on"); else {
	assert(ch<MAXCH&&t<MAXNT);
	for(i=nobp[ch][t];i<non;i++)if(nob[i].tt>tt&&nob[i].ch==ch&&nob[i].nt==t)break;
	nobp[ch][t]=i;
	if(i<non){
	  l1+=sprfrac(l1,nob[i].tt-tt,tpc,1,frpad);
	  tri[tr].pt=tt;tri[tr].et=nob[i].tt;
	}else l1+=sprintf(l1,"infinity");
      }
      if(u!=tri[tr].pv){l1+=sprintf(l1,"   von=%d",u);tri[tr].pv=u;}
      if(!raw&&i<non&&nob[i].voff!=0x40)l1+=sprintf(l1,"   voff=%d",nob[i].voff);
      l1+=sprintf(l1,"\n");
      if(!raw&&i==non)l1+=sprintf(l1,"# Warning tr=%d p=&%X, note never turned off\n",tr,p);
      goto bp1;
    }
    if(sth==0xB){
      t=look1();if(t==7){
	t=get1();u=get1();
	l1+=sprintf(l1,"Channel volume %d\n",u);goto bp1;
      }
    }
    if(sth==0xC){t=get1();l1+=sprintf(l1,"Instrument %d\n",t+1);goto bp1;}
    if(sth==0xF){
      if(st==0xF0||st==0xF7){
	t=getv();l1+=sprintf(l1,"Sysex event &%02X ",st);
	for(i=0;i<t;i++)l1+=sprintf(l1," &%02X",dat[p++]);l1+=sprintf(l1,"\n");
	goto bp1;
      }
      if(st==0xFF){
	ty=get1();ln=getv();need(ln,"Meta");
	if(ty>=1&&ty<=7){
	  l1+=sprintf(l1,"Text type %d: \"",ty);
	  for(i=0;i<ln;i++){
	    t=dat[p++];switch(t){
	    case '\t':l1+=sprintf(l1,"\\t");break;
	    case '\n':l1+=sprintf(l1,"\\n");break;
	    case '\v':l1+=sprintf(l1,"\\v");break;
	    case '\f':l1+=sprintf(l1,"\\f");break;
	    case '\r':l1+=sprintf(l1,"\\r");break;
	    case '\\':l1+=sprintf(l1,"\\\\");break;
	    default:if(t!=0)l1+=sprintf(l1,"%c",t);break;
	    }
	  }
	  l1+=sprintf(l1,"\"\n");
	  goto bp1;
	}
	if(ty==0x2F){
	  assert(ln==0);
	  l1+=sprintf(l1,"End of track\n");goto bp1;
	}
	if(ty==0x51){
	  assert(ln==3);tempo=get3();
	  l1+=sprintf(l1,"Tempo ");l1+=sprfrac2(l1,60*1000000,tempo);l1+=sprintf(l1,"\n");
	  alltrfl=1;
	  goto bp1;
	}
	if(ty==0x58){
	  assert(ln==4);assert(tt==btt);
	  tis0=dat[p];tis1=1<<dat[p+1];assrt((tis0*4*tpc)%tis1==0,"New time signature not commensurate with division");
	  tis2=(tis0*4*tpc)/tis1;
	  l1+=sprintf(l1,"Time signature %d/%d, clocks/mtick %d, crotchets/32ndnote %d\n",tis0,tis1,dat[p+2],dat[p+3]);
	  alltrfl=1;
	  p+=ln;goto bp1;
	}
	if(ty==0x59){
	  assert(ln==2);
	  sf=get1();if(sf>=128)sf-=256;mi=get1();
	  l1+=sprintf(l1,"Key %s %s\n",key(sf,mi,0),mi?"minor":"major");
	  alltrfl=1;
	  goto bp1;
	}
	l1+=sprintf(l1,"Meta Event   type &%02X  ",ty);
	for(i=0;i<ln;i++)l1+=sprintf(l1," %d",dat[p++]);l1+=sprintf(l1,"\n");
	goto bp1;
      }
      assert(0);
    }
    if(sth==0xD)sk=1; else sk=2;
    l1+=sprintf(l1,"ST &%02X",st);
    for(i=0;i<sk;i++)l1+=sprintf(l1," &%02X",dat[p++]);l1+=sprintf(l1,"\n");
  bp1:
    tri[tr].dt=getnnodt(st,tri[tr].e,&tri[tr].st);
    if(tri[tr].dt>=0){tri[tr].tt+=tri[tr].dt;tri[tr].p=p;} else {tri[tr].a=0;nat--;}
  bp2:
    if(sep){
      s=l1-l+1;t=sizeof(outline);u=(s+t-1)/t;
      assert((u+1)*t<=MAXLL&&ols>=MAXLL);
      outbuf->tr=tr;outbuf->all=alltrfl;outbuf->bn=bn;outbuf->l=(char*)(outbuf+1);
      if(ols-(u+1)*t<MAXLL){
	ols=MAXLL*10;outbuf->nx=(outline*)malloc(ols);
	if(!outbuf->nx){fprintf(stderr,"Couldn't malloc %d bytes (outbuf2)\n",ols);exit(1);}
      } else {
	outbuf->nx=outbuf+u+1;ols-=(u+1)*t;
      }
      memcpy(outbuf->l,l,s);outbuf=outbuf->nx;outbuf->nx=0;
    } else printf("%s",l);
  } /* main loop */

  if(sep)for(tr=0;tr<tracks;tr++){
    printf("# TRACK %d\n",tr);
    prb=-1;
    for(outbuf=outbuf0;outbuf->nx;outbuf=outbuf->nx){
      if((outbuf->all||outbuf->tr==tr)&&outbuf->bn!=prb){printf("\n");prb=outbuf->bn;}
      if(outbuf->all&&outbuf->tr!=tr)printf("# ");
      if(outbuf->all||outbuf->tr==tr)printf("%s",outbuf->l);
    }
    printf("\n");
  }
  
  printf("# Successfully parsed\n");
  printf("# Duration = %g seconds\n",ert/1000000);
  return 0;
}
