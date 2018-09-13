#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

int main(int ac, char **av)
{
  char a[256], *b, *c[256], **d;
  pid_t p;
  int s;

  while(1)
  {
    write(1,"$",2);
    for (b=a; *(b-1) != '\n'; b++)
    {
      read(0,b,1);
    }
    *b=0;d=c;
    *d++=a;
    for (b=a; b < a+256 && *b!=0; b++)
    {
      if (*b == ' ' || *b == '\n')
      {
        *b=0;
        *d=b+1;
        d++;
      }
    }
    *(d-1) = 0;
    if (!(p = fork()))
      execvp(a,c);
    else
      wait(&s);
  }
  return 0;
}

