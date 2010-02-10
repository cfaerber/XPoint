/* nomarch 1.4 - extract old `.arc' archives.
 * Copyright (C) 2001-2006 Russell Marks.
 *
 * main.c - most of the non-extraction stuff.
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#define NOMARCH_VER	"1.4"

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <utime.h>
#include <unistd.h>

#include "readrle.h"
#include "readhuff.h"
#include "readlzw.h"


char *archive_filename=NULL;
char **archive_matchstrs=NULL;		/* NULL, or points to argv+N */
int num_matchstrs=0;

int opt_list=0,opt_print=0,opt_test=0,opt_verbose=0;
int opt_preservecase=0;

int quiet=0;


struct archived_file_header_tag
  {
  unsigned char method;
  char name[13];
  unsigned long compressed_size;	/* 4 bytes in file */
  unsigned int date,time,crc;		/* 2 bytes each in file */
  unsigned long orig_size;		/* 4 bytes in file */
  
  int has_crc;
  };


int maybe_downcase(int c)
{
if(opt_preservecase)
  return(c);

return(tolower(c));
}


/* there is no overall header for the archive, but there's a header
 * for each file stored in it.
 * returns zero if we couldn't get a header.
 * NB: a header with method zero marks EOF.
 */
int read_file_header(FILE *in,struct archived_file_header_tag *hdrp)
{
unsigned char buf[4+2+2+2+4];	/* used to read size1/date/time/crc/size2 */
int bufsiz=sizeof(buf);
int method_high;
int c,f;

hdrp->method=0xff;
if(fgetc(in)!=0x1a)
  return(0);

if((c=fgetc(in))==EOF)
  return(0);

/* allow for the spark archive variant's alternate method encoding */
method_high=(c>>7);
hdrp->method=(c&127);

/* zero if EOF, which also means no further `header' */
if(hdrp->method==0)
  return(1);

/* `old' version of uncompressed storage was weird */
if(hdrp->method==1)
  bufsiz-=4;		/* no `orig_size' field */

if(fread(hdrp->name,1,sizeof(hdrp->name),in)!=sizeof(hdrp->name) ||
   fread(buf,1,bufsiz,in)!=bufsiz)
  return(0);

/* extract the bits from buf */
hdrp->compressed_size=(buf[0]|(buf[1]<<8)|(buf[2]<<16)|(buf[3]<<24));
hdrp->date=(buf[4]|(buf[5]<<8));
hdrp->time=(buf[6]|(buf[7]<<8));
hdrp->crc=(buf[8]|(buf[9]<<8));		/* yes, only 16-bit CRC */
hdrp->has_crc=1;
if(hdrp->method==1)
  hdrp->orig_size=hdrp->compressed_size;
else
  hdrp->orig_size=(buf[10]|(buf[11]<<8)|(buf[12]<<16)|(buf[13]<<24));

/* make *sure* name is asciiz */
hdrp->name[12]=0;

/* strip top bits, and lowercase the name */
for(f=0;f<strlen(hdrp->name);f++)
  hdrp->name[f]=maybe_downcase(hdrp->name[f]&127);

/* lose the possible extra bytes in spark archives */
if(method_high)
  {
  if(fread(buf,1,12,in)!=12)
    return(0);

  /* has a weird recursive-.arc file scheme for subdirs,
   * and since these are supposed to be dealt with inline
   * (though they aren't here) the CRCs could be junk.
   * So check for it being marked as a stored dir.
   */
  if(hdrp->method==2 && buf[3]==0xff && buf[2]==0xfd && buf[1]==0xdc)
    hdrp->has_crc=0;
  }

return(1);
}


/* self-extracting archives, for both CP/M and MS-DOS, have up to
 * 3 bytes before the initial ^Z. This skips those if present.
 * Returns zero if there's an input error, or we fail to find ^Z in
 * the first 4 bytes.
 *
 * This should work with self-extracting archives for CP/M
 * (e.g. unarc16.ark), and those produced by `arc'. It won't work with
 * pkpak self-extracting archives, for two reasons:
 *
 * - they have 4 bytes before the ^Z.
 * - they have an EOF member (zero byte) right after that, giving you
 *   an archive containing no files (grrr).
 *
 * So I thought it was better (and less confusing) to effectively stick
 * with the not-an-archive error for those. :-)
 */
int skip_sfx_header(FILE *in)
{
int c,f,got=0;

for(f=0;f<4;f++)
  {
  if((c=fgetc(in))==EOF)
    return(0);
  if(c==0x1a)
    {
    got=1;
    ungetc(c,in);
    break;
    }
  }

return(got);
}


/* read file data, assuming header has just been read from in
 * and hdrp's data matches it. Caller is responsible for freeing
 * the memory allocated.
 * Returns NULL for file I/O error only; OOM is fatal (doesn't return).
 */
unsigned char *read_file_data(FILE *in,struct archived_file_header_tag *hdrp)
{
unsigned char *data;
int siz=hdrp->compressed_size;

if((data=malloc(siz))==NULL)
  fprintf(stderr,"nomarch: out of memory!\n"),exit(1);

if(fread(data,1,siz,in)!=siz)
  {
  free(data);
  data=NULL;
  }

return(data);
}


/* variant which just skips past the data */
int skip_file_data(FILE *in,struct archived_file_header_tag *hdrp)
{
int siz=hdrp->compressed_size;
int f;

for(f=0;f<siz;f++)
  if(fgetc(in)==EOF) return(0);

return(1);
}


/* XXX should probably do a table-based version, but I'm not sure
 * it's really worth the effort :-)
 */
unsigned int calc_crc(unsigned char *data,unsigned long len)
{
unsigned long f;
unsigned char *ptr=data;
unsigned long crc=0;
int i;

for(f=0;f<len;f++)
  {
  crc^=*ptr++;
  for(i=0;i<8;i++)
    {
    if(crc&1)
      crc=((crc>>1)^0xA001);
    else
      crc>>=1;
    }
  }

crc&=0xffff;

return(crc);
}


/* convert MS-DOS time format to string */
char *mkdatetimestr(unsigned int hdate,unsigned int htime)
{
static char buf[128];
int day,month,year,hour,min;

year=1980+(hdate>>9);
month=((hdate>>5)&15);
day=(hdate&31);
hour=(htime>>11);
min=((htime>>5)&63);
/* seconds ignored */

sprintf(buf,"%4d-%02d-%02d %02d:%02d",year,month,day,hour,min);
return(buf);
}


/* convert to time_t */
time_t mkdatetimet(unsigned int hdate,unsigned int htime)
{
struct tm tms;

tms.tm_year=80+(hdate>>9);
tms.tm_mon=((hdate>>5)&15)-1;
tms.tm_mday=(hdate&31);
tms.tm_hour=(htime>>11);
tms.tm_min=((htime>>5)&63);
tms.tm_sec=(htime&31)*2;
tms.tm_isdst=-1;	/* i.e. unknown */

return(mktime(&tms));
}


/* simple wildcard-matching code. It implements shell-like
 * `*' and `?', but not `[]' or `{}'.
 * XXX might be nice to replace this with something better :-)
 */
int is_match(char *filename,char *wildcard)
{
char *ptr=filename,*match=wildcard;
char *tmp,*tmp2;
int old;
int ok=1;

while(*ptr && *match && ok)
  {
  switch(*match)
    {
    case '*':
      /* need to check that everything up to the next * or ? matches
       * at some point from here onwards */
       
      /* skip the `*', and any * or ? following */
      while(*match=='*' || *match=='?')
        match++;
      
      /* find the next * or ?, or the end of the name */
      tmp=strchr(match,'*');
      if(tmp==NULL) tmp=strchr(match,'?');
      if(tmp==NULL) tmp=match+strlen(match);
      tmp--;
        
      /* if *match is NUL, the * was the last char, so it's already matched
       * if tmp+1-match>strlen(ptr) then it can't possibly match
       */
      if(*match==0)
        ptr+=strlen(ptr);	/* just `*', so skip to end */
      else
        {
        if(tmp+1-match>strlen(ptr))
          ok=0;
        else
          {
          /* go forward through the string, attempting to match the text
           * from match to tmp (inclusive) each time
           */
          old=tmp[1]; tmp[1]=0;
          if((tmp2=strstr(ptr,match))==NULL)
            ok=0;		/* didn't match */
          else
            ptr=tmp2;
            
          tmp[1]=old;
          }
        }
      break;
      
    case '?':
      match++; ptr++;		/* always matches */
      break;
      
    default:
      if(*match!=*ptr)
        ok=0;
      else
        {
        match++;
        ptr++;
        }
    }
  }

if(*ptr || *match) ok=0;	/* if any text left, it didn't match */

return(ok);
}


/* see if file matches any of the match strings specified
 * on the cmdline.
 */
int file_matches(char *filename)
{
int f;

if(!num_matchstrs) return(1);

for(f=0;f<num_matchstrs;f++)
  if(is_match(filename,archive_matchstrs[f]))
    return(1);

return(0);
}



int arc_list(int verbose)
{
#define NUM_METHODSTR 10
char *methodstr[NUM_METHODSTR]=
  {
  "EOF",	/* should never be shown */
  "Stored",	/* adopting a zip term here :-) */
  "Stored",
  "Packed",
  "Squeezed",
  "crunched",	/* lowercase `important' */
  "crunched",
  "crunched",
  "Crunched",
  "Squashed"
  };
FILE *in;
struct archived_file_header_tag hdr;
int done=0;

if((in=fopen(archive_filename,"rb"))==NULL)
  fprintf(stderr,"nomarch: %s\n",strerror(errno)),exit(1);

if(!skip_sfx_header(in) || !read_file_header(in,&hdr))
  fprintf(stderr,"nomarch: bad first header - not a .arc file?\n"),exit(1);

do
  {
  if(hdr.method==0)	/* EOF */
    {
    done=1;
    continue;
    }
  
  if(!skip_file_data(in,&hdr))
    fprintf(stderr,"nomarch: error reading data (hit EOF)\n"),exit(1);

  if(file_matches(hdr.name))
    {
    if(verbose)
      {
      printf("%-13s\t%d (%s)\t%9ld %9ld  %s  ",
             hdr.name,hdr.method,
             hdr.method<NUM_METHODSTR?
               methodstr[hdr.method]:
               (hdr.method==127?"Compress":"unknown"),
             hdr.compressed_size,hdr.orig_size,
             mkdatetimestr(hdr.date,hdr.time));
      if(hdr.has_crc)
        printf("%04X",hdr.crc);
      putchar('\n');
      }
    else
      printf("%-13s\t%9ld   %s\n",
             hdr.name,hdr.orig_size,mkdatetimestr(hdr.date,hdr.time));
    }
  
  /* read header ready for next file */
  if(!read_file_header(in,&hdr))
    fprintf(stderr,"nomarch: error reading record header\n"),exit(1);
  }
while(!done);

fclose(in);

return(0);
}


int arc_extract_or_test(int test_only)
{
FILE *in;
struct archived_file_header_tag hdr;
int done=0;
unsigned char *data,*orig_data;
int supported;
int exitval=0;

if((in=fopen(archive_filename,"rb"))==NULL)
  fprintf(stderr,"nomarch: %s\n",strerror(errno)),exit(1);

if(!skip_sfx_header(in) || !read_file_header(in,&hdr))
  fprintf(stderr,"nomarch: bad first header - not a .arc file?\n"),exit(1);

do
  {
  if(hdr.method==0)	/* EOF */
    {
    done=1;
    continue;
    }
  
  if(!file_matches(hdr.name))
    {
    if(!skip_file_data(in,&hdr))
      fprintf(stderr,"nomarch: error reading data (hit EOF)\n"),exit(1);
    }
  else
    {
    if((data=read_file_data(in,&hdr))==NULL)
      fprintf(stderr,"nomarch: error reading data (hit EOF)\n"),exit(1);

    /* decompress file data */
    if(!quiet)
      {
      printf("%-12s\t",hdr.name);
      fflush(stdout);
      }
    
    orig_data=NULL;
    supported=0;
  
    /* FWIW, most common types are (by far) 8/9 and 2.
     * (127 is the most common in Spark archives, but only those.)
     * 3 and 4 crop up occasionally. 5 and 6 are very, very rare.
     * And I don't think I've seen a *single* file with 1 or 7 yet.
     */
    switch(hdr.method)
      {
      case 1: case 2:	/* no compression */
        supported=1;
        orig_data=data;
        break;

      case 3:		/* "packed" (RLE) */
        supported=1;
        orig_data=convert_rle(data,hdr.compressed_size,hdr.orig_size);
        break;

      case 4:		/* "squeezed" (Huffman, like CP/M `SQ') */
        supported=1;
        orig_data=convert_huff(data,hdr.compressed_size,hdr.orig_size);
        break;

      case 5:		/* "crunched" (12-bit static LZW) */
        supported=1;
        orig_data=convert_lzw_dynamic(data,0,0,
                                      hdr.compressed_size,hdr.orig_size);
        break;
        
      case 6:		/* "crunched" (RLE+12-bit static LZW) */
        supported=1;
        orig_data=convert_lzw_dynamic(data,0,1,
                                      hdr.compressed_size,hdr.orig_size);
        break;
        
      case 7:		/* PKPAK docs call this one "internal to SEA" */
        /* it looks like this one was only used by a development version
         * of SEA ARC, so chances are it can be safely ignored.
         * OTOH, it's just method 6 with a slightly different hash,
         * so I presume it wouldn't be *that* hard to add... :-)
         */
        break;

      case 8:		/* "Crunched" [sic]
                         * (RLE+9-to-12-bit dynamic LZW, a *bit* like GIF) */
        supported=1;
        orig_data=convert_lzw_dynamic(data,12,1,
                                      hdr.compressed_size,hdr.orig_size);
        break;

      case 9:		/* "Squashed" (9-to-13-bit, no RLE) */
        supported=1;
        orig_data=convert_lzw_dynamic(data,13,0,
                                      hdr.compressed_size,hdr.orig_size);
        break;
      
      case 127:		/* "Compress" (9-to-16-bit, no RLE) ("Spark" only) */
        supported=1;
        orig_data=convert_lzw_dynamic(data,16,0,
                                      hdr.compressed_size,hdr.orig_size);
        break;
      }
    
    /* there was a `pak 2.0' which added a type 10 ("distill"), but I don't
     * plan to support that unless there's some desperate need for it.
     */
    
    if(orig_data==NULL)
      {
      if(supported)
        fprintf(quiet?stderr:stdout,
                "error%s\n",test_only?"":" extracting file");
      else
        fprintf(quiet?stderr:stdout,
                "unsupported compression method %d\n",hdr.method);
      exitval=1;
      }
    else
      {
      FILE *out=NULL;
      char *ptr;

      /* CP/M stuff in particular likes those slashes... */
      while((ptr=strchr(hdr.name,'/'))!=NULL)
        *ptr='_';

      if(opt_print)
        out=stdout;
      
      /* write the file, if not just testing */
      if(!opt_print && !test_only && (out=fopen(hdr.name,"wb"))==NULL)
        {
        fprintf(quiet?stderr:stdout,"error, %s\n",strerror(errno));
        exitval=1;
        }
      else
        {
        if(!test_only && fwrite(orig_data,1,hdr.orig_size,out)!=hdr.orig_size)
          {
          fprintf(quiet?stderr:stdout,"error, %s\n",strerror(errno));
          exitval=1;
          }
        else
          {
          if(hdr.has_crc && calc_crc(orig_data,hdr.orig_size)!=hdr.crc)
            {
            fprintf(quiet?stderr:stdout,
                    "%s, bad CRC\n",test_only?"test failed":"warning");
            exitval=1;
            }
          else
            {
            if(!quiet)
              printf("ok\n");
            }
          }
      
        if(!test_only && out!=stdout)
          {
          struct utimbuf ubuf;
          time_t t;

          fclose(out);

          /* alter atime/mtime */
          t=mkdatetimet(hdr.date,hdr.time);
          ubuf.actime=ubuf.modtime=t;
          utime(hdr.name,&ubuf);
          }
        }

      if(orig_data!=data)		/* don't free uncompressed stuff twice :-) */
        free(orig_data);
      }
  
    free(data);
    }

  /* read header ready for next file */
  if(!read_file_header(in,&hdr))
    fprintf(stderr,"nomarch: error reading record header\n"),exit(1);
  }
while(!done);

fclose(in);

return(exitval);
}


void usage_help(void)
{
printf("nomarch %s - copyright (c) 2001-2006 Russell Marks.\n",NOMARCH_VER);
printf("\n"
"usage: nomarch [-hlptUv] [archive.arc] [match1 [match2 ...]]\n"
"\n"
"	-h	this usage help.\n"
"	-l	list contents of archive.\n"
"	-p	extract to standard output, rather than to separate files.\n"
"	-t	test archive (i.e. check file CRCs).\n"
"	-U	use uppercase filenames (preserve case).\n"
"	-v	give verbose listing (when used with `-l').\n"
"\n"
"  archive.arc	the .arc or .ark file to list/extract/test.\n"
"		(The default action is to extract the files.)\n"
"\n"
"  match1 etc.	zero or more wildcards, for files to list/extract/test;\n"
"		the file is processed if it matches any one of these.\n"
"		(If *no* wildcards are specified, all files match.)\n"
"		Wildcard operators supported are shell-like `*' and `?'.\n");
}


void parse_options(int argc,char *argv[])
{
int done=0;

archive_filename=NULL;

opterr=0;

do
  switch(getopt(argc,argv,"hlptUv"))
    {
    case 'l':	/* list */
      opt_list=1;
      break;
    case 'h':
      usage_help();
      exit(0);
    case 'p':	/* `print' files to stdout */
      opt_print=1;
      break;
    case 't':	/* test */
      opt_test=1;
      break;
    case 'U':	/* preserve case (uppercase) */
      opt_preservecase=1;
      break;
    case 'v':	/* verbose (list) */
      opt_verbose=1;
      break;
    case '?':
      fprintf(stderr,"nomarch: option `%c' not recognised.\n",optopt);
      exit(1);
    case -1:
      done=1;
    }
while(!done);

/* get any match strings */
if(argc-optind>1)
  {
  archive_matchstrs=argv+optind+1;
  num_matchstrs=argc-optind-1;
  }

if(argc==optind)	/* if no filename */
  usage_help(),exit(1);

archive_filename=argv[optind];

if(opt_print)
  quiet=1;
}


int main(int argc,char *argv[])
{
parse_options(argc,argv);

if(opt_list && opt_test)
  opt_list=0;	/* test wins */

if(opt_list)
  exit(arc_list(opt_verbose));

exit(arc_extract_or_test(opt_test));
}
