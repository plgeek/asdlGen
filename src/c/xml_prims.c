#include "XMLPkl.h"
#include "XMLPrims.h"
#include "cii/table.h"
#include <ctype.h>
static Table_T tag_tbl = NULL;
static int get_tag(char *x) {
  const char *key = Atom_string(x);
  int *v;
  if (tag_tbl != NULL) {
    v = Table_get(tag_tbl,key);
  } else {
    int i = 0;
    tag_tbl = Table_new(128,NULL,NULL);
    while(XMLPkl_tag_map[i].name) {
	 Table_put(tag_tbl,
		   Atom_string(XMLPkl_tag_map[i].name),
		   &(XMLPkl_tag_map[i].tag));
	 i++;
    }
    v = Table_get(tag_tbl,key);
  }
  if (v == NULL) return -1;
  return (*v);
}
static int eat_ws(FILE *s) {
  int ch;
  for(ch = getc(s); isspace(ch); ch=getc(s))
    ; /* nop */
  ungetc(ch,s);
  return 1;
}
static int eat_till(char c,FILE *s) {
  int ch;
  for(ch = getc(s); (c!=ch); ch=getc(s))
    ; /* nop */
  return 1;
}
static int match_char(char c,FILE *s) {
  return(c == getc(s));
}
static int match_string(const char *n,FILE *s) {
  int ch;
  const char *p = n;
  ch = getc(s);
  while(ch == *p) {
       p++;
       if(*p == '\0') 
	    return 1;
       ch = getc(s);
  }
  return 0;
}
static int scan_tok(char *buf,int max,FILE * s) {
     int ch = getc(s);
     max--;
     /* first char must be a letter */
     if(!isalpha(ch)) return 0;
     while(max && (isalnum(ch) || (ch == '_') || (ch == '.'))) {
	  *buf = ch;
	  buf++;
	  max--;
	  ch = getc(s);
	  if(!(isalnum(ch) || (ch == '_') || (ch == '.'))) {
	       *buf='\0';
	       ungetc(ch,s);
	       return 1;
	  }
     }
     return 0;
}
static int scan_int(int *v,FILE * s) {
     int res = 0;
     int ch = getc(s);
     int neg = (ch == '-') || (ch == '~');
     if(neg) ch = getc(s);
     while(isdigit(ch)) {
	  res += (ch - '0');
	  ch = getc(s);
	  if(isdigit(ch)) {
	       res *= 10;
	  } else {
	       ungetc(ch,s);
	       if(neg) { res = -res; }
	       *v = res;
	       return 1;
	  }
     }
     return 0;
}
static int ent2char(char *src) {
     int ch;
     if(!strcmp(src,"amp")) { return '&'; }
     else if(!strcmp(src,"apos")) { return '\''; } 
     else if(!strcmp(src,"quot")) { return '"'; }
     else if(!strcmp(src,"gt")) { return '>'; }
     else if(!strcmp(src,"lt")) { return '>'; } 
     else if(sscanf(src,"#x%x",&ch) == 1) { return ch; } 
     else { return *src; }
}
/* messy clean it up */
static int scan_string(const char **res,int *len,FILE *s) {
     int max = 32;
     int i = 0;
     char *buf = malloc(max);
     char *src,*dst,*ptr;
     int ch = getc(s);
     if(buf == NULL) die();
     if(ch != '"') return 0;
     ptr = buf;
     ch = getc(s);
     /* read the string expanding the buffer as necssary */
     while(ch != '"') {
	  if(i >= max) {
	       max *= 2;
	       buf = realloc(buf,max);
	       if(buf == NULL) die();
	       ptr = &(buf[i]);
	  }
	  *ptr = ch;
	  ch = getc(s);
	  i++;ptr++;
     }
     if (ch != '"')  return 0;
     /* rewrite the string in place replacing entity references */
     src = dst = buf;
     while(src < ptr) {
	  if(*src == '&') {
	       char *tmp = src+1;
	       while(*tmp != ';' && (tmp < ptr)) {
		    tmp++;
	       } 
	       *tmp = '\0';
	       *dst = ent2char(src+1);
	       dst++;
	       src = tmp+1;
	  } else {
	       *dst = *src;
	       dst++; src++;
	  }
     }
     *len = dst - buf;
     buf = realloc(buf,*len);
     if(buf == NULL) die();
     *res = buf;
     return 1;
}
void XMLPkl_write_element_begin(const char* n,outstream_ty s) {
  putc('<',s);
  fputs(n,s);
  putc('>',s);
}
void XMLPkl_write_element_end(const char* n,outstream_ty s) {
  putc('<',s);
  putc('/',s);
  fputs(n,s);
  putc('>',s);
}
void XMLPkl_read_element_begin(const char* n,instream_ty s) {
  if (eat_ws(s) && match_char('<',s) &&
      eat_ws(s) && match_string(n,s) &&
      eat_till('>',s)) {
    return;
  } else {
    die();
  }
}
void XMLPkl_read_element_end(const char* n,instream_ty s) {
  if (eat_ws(s) && match_char('<',s) &&
      eat_ws(s) && match_char('/',s) &&
      eat_ws(s) && match_string(n,s) &&
      eat_till('>',s)) {
    return;
  } else {  die(); }

}
int XMLPkl_read_tagged_element(instream_ty s) {
  char buf[256];
  eat_ws(s);
  match_char('<',s);
  eat_ws(s);
  scan_tok(buf,128,s);
  eat_till('>',s); /* ignore attributes */
  return (get_tag(buf));
}

void XMLPkl_write_option(const char *n,generic_writer_ty wr, 
		      opt_ty v, outstream_ty s) {

  if (v == NULL) {
    fprintf(s,"<%s-opt sz=\"0\"></%s-opt>",n,n);
  } else {
    fprintf(s,"<%s-opt sz=\"0\">",n);
    (*wr)(v,s);
    fprintf(s,"</%s-opt>",n);
  }
}

void XMLPkl_write_list(const char *n,generic_writer_ty wr, list_ty v, 
		    outstream_ty s) {
  int len = Seq_length(v);
  int i;
  fprintf(s,"<%s-seq sz=\"%d\">",n,len);
  for(i=0;i<len;i++) {
    (*wr)(Seq_get(v,i),s);
  }
  fprintf(s,"</%s-seq>",n);
}

opt_ty XMLPkl_read_option(const char *n,generic_reader_ty rd, instream_ty s) {
     int len = 0;
  if (eat_ws(s) && match_char('<',s) &&
      eat_ws(s) && match_string(n,s) && match_string("-opt",s) &&
      eat_ws(s) && match_string("sz=\"",s) &&
      scan_int(&len,s) && eat_till('>',s)) {
       opt_ty ret;
       if(len == 0) {
	    ret = (*rd)(s);
       } else {
	    ret = NULL;
       }
       /* check end tag */
       if (eat_ws(s) && match_char('<',s) &&
	   eat_ws(s) && match_char('/',s) &&
	   eat_ws(s) && match_string(n,s) && match_string("-opt",s) &&
	   eat_till('>',s)) {
	    return ret;
       } else { die (); }
  } else {  die(); }
}

list_ty XMLPkl_read_list(const char *n,generic_reader_ty rd,instream_ty s) {
     int len = 0;
  if (eat_ws(s) && match_char('<',s) &&
      eat_ws(s) && match_string(n,s) && match_string("-seq",s) &&
      eat_ws(s) && match_string("sz=\"",s) &&
      scan_int(&len,s) && eat_till('>',s)) {
       Seq_T ret = Seq_new(len);
       while(len) {
	    Seq_addhi(ret,(*rd)(s));
	    len--;
       }
       /* check end tag */
       if (eat_ws(s) && match_char('<',s) &&
	   eat_ws(s) && match_char('/',s) &&
	   eat_ws(s) && match_string(n,s) && match_string("-seq",s) &&
	   eat_till('>',s)) {
	    return ret;
       } else { die (); }
  } else {  die(); }
  return NULL; /* not reached */
}

XMLPrims_int_ty XMLPrims_read_int(instream_ty s) {
  int res = 0;
  if (eat_ws(s) && match_char('<',s) &&
      eat_ws(s) && match_string("int",s) &&
      eat_ws(s) && match_string("v=\"",s) &&
      scan_int(&res,s) && eat_till('>',s)) {
    return res;
  } else {  die(); }
  return 0; /* not reached */
}

XMLPrims_big_int_ty XMLPrims_read_big_int(instream_ty s) {
  die();
  return NULL;
}

XMLPrims_string_ty XMLPrims_read_string(instream_ty s) {
  Text_T ret;
  if (eat_ws(s) && match_char('<',s) &&
      eat_ws(s) && match_string("string",s) &&
      eat_ws(s) && match_string("v=",s) &&
      scan_string(&ret.str,&ret.len,s) && eat_till('>',s))  {
       return ret;
  } else {  die(); }
  return Text_null; /* not reached */
}
XMLPrims_identifier_ty XMLPrims_read_identifier(instream_ty s) {
  const char *buf;
  const char *ret;
  int len;
  if (eat_ws(s) && match_char('<',s) &&
      eat_ws(s) && match_string("identifier",s) &&
      eat_ws(s) && match_string("v=",s) &&
      scan_string(&buf,&len,s) && eat_till('>',s)) {
       ret = Atom_new(buf,len);
       free((void*)buf); /* free space allocated by scan_string */
       return  ret;
  } else {  die(); }
  return NULL; /* not reached */
}
static void emit_cdata(const char *str,int len,outstream_ty s) {
     while(len) {
	  switch(*str) {
	  case '&': fputs("&amp;",s); break;
	  case '\'': fputs("&apos;",s); break;
	  case '"': fputs("&quot;",s); break;
	  case '>': fputs("&gt;",s); break;
	  case '<': fputs("&lt;",s);  break;
	  default: 
	       if(isprint(*str)) {
		    putc(*str,s);
	       } else {
		    fprintf(s,"&#x%x;",*str);
	       }
	  }
	  str++;
	  len--;
     }
}

void XMLPrims_write_int(XMLPrims_int_ty x,outstream_ty s) {
  fprintf(s,"<int v=\"%d\"/>",x);
}
void XMLPrims_write_big_int(XMLPrims_big_int_ty x,outstream_ty s) {
  die();
}
void XMLPrims_write_string(XMLPrims_string_ty x,outstream_ty s) {
  fputs("<string v=\"",s);
  emit_cdata(x.str,x.len,s);
  fputs("\"/>",s);
}
void XMLPrims_write_identifier(XMLPrims_identifier_ty x,outstream_ty s) {
  fputs("<identifier v=\"",s);
  emit_cdata(Atom_string(x),Atom_length(x),s);
  fputs("\"/>",s);
}

void* XMLPrims_read_generic_int(instream_ty s) {
  XMLPrims_int_ty* ret = malloc(sizeof(XMLPrims_int_ty));
  *ret = XMLPrims_read_int(s);
  return ret;
}

void* XMLPrims_read_generic_string(instream_ty s) {
  Text_T* ret = malloc(sizeof(Text_T));
  *ret = XMLPrims_read_string(s);
  return ret;
}
void* XMLPrims_read_generic_identifier(instream_ty s) {
  return ((void*)XMLPrims_read_identifier(s));
}
void XMLPrims_write_generic_int(void *x,instream_ty s) {
  XMLPrims_write_int(*((XMLPrims_int_ty*)x),s);
}
void XMLPrims_write_generic_string(void *x,instream_ty s) {
  XMLPrims_write_string(*((Text_T*)x),s);
}
void XMLPrims_write_generic_identifier(void *x,instream_ty s) {
  XMLPrims_write_identifier(x,s);
}



