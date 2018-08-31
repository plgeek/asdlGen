#ifndef _SHARE_
#define _SHARE_
#define SREF(x) x
typedef struct {
  const char* key;
    void* value; /* value is NULL when this is a USE of the key */
} share_ty;

share_ty ptr2share(void *ptr);
void *   share2ptr(share_ty s);
void share_clear_table(void);
#endif /* _SHARE */
